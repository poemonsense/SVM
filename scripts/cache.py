import argparse
import math
import time
from tqdm import tqdm

from inspect import currentframe

class CacheTrace(object):
  def __init__(self, index, message) -> None:
    self.index = index
    self.message = message

class CacheBlock(object):
  def __init__(self, clock) -> None:
    self.evicted = False
    self.valid = False
    self.dirty = False
    self.tag = 0
    self.data = 0
    self.clock = clock
    self.time = self.clock()

  def empty(self):
    return not self.valid

  def hit(self, tag):
    return self.valid and self.tag == tag

  def touch(self):
    self.time = self.clock()

  def replacer(self):
    score = 0
    if self.time <= self.clock() - 500000:
      score += 2
    if self.evicted:
      score += 1
    return score

  def __str__(self) -> str:
    v = 1 if self.valid else 0
    e = 1 if self.evicted else 0
    d = 1 if self.dirty else 0
    return f"V({v}) E({e}) D({d}) TAG({self.tag:x}) DATA({self.data:x}) TIME({self.time})"

class CacheSet(object):
  def __init__(self, set_index, n_ways, clock) -> None:
    self.set_index = set_index
    self.n_ways = n_ways
    self.blocks = [CacheBlock(clock) for _ in range(self.n_ways)]

  def hit_block(self, tag):
    for block in self.blocks:
      if block.hit(tag):
        return (True, block)
    return (False, self.blocks)

  def replaced_block(self):
    # 1) invalid
    for block in self.blocks:
      if block.empty():
        return block
    # 2) valid && clean
    clean = [b for b in self.blocks if not b.dirty]
    if not len(clean):
      return None
    score = [b.replacer() for b in clean]
    sel_score = max(score)
    sel_clean = [b for (b, c) in zip(clean, score) if c == sel_score]
    return min(sel_clean, key=lambda b: b.time)

  def evicted_time(self):
    # decrease time to 1) newest of evicted, or 2) oldest of not evicted but at most clock-500000
    evicted = [b for b in self.blocks if b.valid and b.evicted]
    if evicted:
      return max([b.time for b in evicted]) + 1
    not_evicted = [b for b in self.blocks if b.valid and not b.evicted]
    return min([b.time for b in not_evicted]) - 1

  def __getitem__(self, key):
    return self.blocks[key]

  def __str__(self) -> str:
    return '\n'.join([f"Block[{i}] {b}" for (i, b) in enumerate(self.blocks)])

class Cache(object):
  def __init__(self, cache_size, n_ways, debug, block_size = 8) -> None:
    self.memory = None
    self.block_size = block_size
    self.n_ways = n_ways
    self.n_sets = cache_size // block_size // n_ways
    set_width = int(math.log2(self.n_sets))
    assert 2 ** set_width == self.n_sets
    self.addr = lambda pair: (pair[1] << set_width) | pair[0]
    self.tag = lambda index: index >> set_width
    self.set = lambda index: index & (self.n_sets - 1)
    self.time = 0
    clock = lambda : self.time
    self.blocks = [CacheSet(i, self.n_ways, clock) for i in range(self.n_sets)]
    self.traces = []
    self.enable_debug = debug
    self.expect_count = 0
    self.write_count = 0
    self.refill_count = 0
    self.evict_count = 0
    self.evict_miss_count = 0

  def trace(self, index, message):
    if self.enable_debug:
      self.traces.append(CacheTrace(index, f"{message} at {self.time}"))

  def warn(self, index, cond, line=None):
    if not cond:
      if line is None:
        line = currentframe().f_back.f_lineno
      print(f"===================")
      print(f"Assertion failed at line {line} for index {index:x}")
      print("Debug trace:")
      trace = [t for t in self.traces if t.index == index]
      for t in trace[-10:]:
        print(t.message)
      print(f"TIME: {self.time}")
      print(f"SET: {self.set(index):x}")
      print(f"TAG: {self.tag(index):x}")
      print(self.blocks[self.set(index)])

  def debug(self, index, cond):
    if not cond:
      self.warn(index, cond, line=currentframe().f_back.f_lineno)
      exit(1)

  def hit_block(self, index):
    return self.blocks[self.set(index)].hit_block(self.tag(index))

  # SRAM Read: get and touch the cache block if possible
  def read(self, index):
    self.trace(index, f"read {index:x}")
    (is_hit, block) = self.hit_block(index)
    if is_hit:
      block.touch()
      return block.data
    return None

  # Cache Read: read the data (and expect it to be the same as golden trace)
  # 1) hit: return it
  # 2) miss: read main memory
  def expect(self, index, data):
    self.expect_count += 1
    self.trace(index, f"expect {index:x} {data:x}")
    rdata = self.read(index)
    # miss: get the rdata
    if rdata is None:
      rdata = self.memory.read(index)
      # try allocating an empty block
      block = self.replace(index)
      if block is not None:
        block.data = rdata
    self.debug(index, rdata == data)

  # Cache Write: write the data with mask
  # 1) hit: update it
  # 2) miss: read main memory first and then update it
  def write(self, index, data, mask):
    self.write_count += 1
    self.trace(index, f"write {index:x} {data:x} {mask:x}")
    (is_hit, result) = self.hit_block(index)
    if is_hit:
      result.touch()
      result.evicted = False
      result.dirty = True
      result.data = (result.data & (~mask)) | (data & mask)
    else:
      block = self.replace(index)
      self.debug(index, block is not None)
      # read it unless full mask
      if mask != (1 << (self.block_size * 8)) - 1:
        block.data = self.memory.read(index)
        block.data = (block.data & (~mask)) | (data & mask)
      else:
        block.data = data
      block.dirty = True

  # Cache Refill: refill data to SRAM
  # 1) hit && clean: check data is correct
  # 2) hit && dirty: skip?
  # 3) miss: replace a block and refill
  # This is controlled by the DUT-DDR interface (as cache hints)
  def refill(self, index, data):
    self.refill_count += 1
    self.trace(index, f"refill {index:x} {data:x}")
    (is_hit, result) = self.hit_block(index)
    if is_hit:
      result.touch()
      result.evicted = False
      if not result.dirty:
        self.debug(index, result.data == data)
    else:
      block = self.replace(index)
      # allocate unless cache full
      if block is not None:
        block.data = data

  # Cache Evict: mark a block as evicted
  # 1) hit: mark as evicted
  # 2) miss: nothing happens
  def evict(self, index, data):
    self.evict_count += 1
    self.trace(index, f"evict {index:x} {data:x}")
    (is_hit, block) = self.hit_block(index)
    if is_hit:
      if block.data == data:
        block.evicted = True
        block.dirty = False
      else:
        # data mismatch. no change?
        self.debug(index, block.dirty)
    else:
      self.trace(index, f"evict check skipped {index:x} {data:x}")
      self.evict_miss_count += 1

  # Replace a cache block
  def replace(self, index):
    block = self.blocks[self.set(index)].replaced_block()
    if block is None:
      return None
    replaced_index = self.addr((self.set(index), block.tag))
    self.trace(replaced_index, f"replace {block}\nothers: {self.blocks[self.set(index)]}")
    block.valid = True
    block.evicted = False
    block.dirty = False
    block.tag = self.tag(index)
    block.touch()
    self.trace(index, f"allocate {block}")
    return block

class Memory(object):
  def __init__(self) -> None:
    self.data = dict()
    self.read_count = 0

  def read(self, address):
    self.read_count += 1
    return self.data[address]

  def write(self, address, data):
    self.data[address] = data

def simulate(memory, cache, trace_filename, count):
  pbar = tqdm(total=count)
  with open(trace_filename) as f:
    cache.time = 0
    for line in f:
      if count is not None and cache.time >= count:
        break
      items = line.split()
      # DUT
      if items[0][0] == "d":
        index, data = int(items[1]), int(items[3])
        # DUT.READ: golden data from DDR
        if items[2][0] == "0":
          cache.refill(index, data)
        # DUT.WRITE: try eviction && data check against REF (self)
        else:
          cache.evict(index, data)
        memory.write(index, data)
      # REF.READ
      elif items[0][0] == "r":
        cache.expect(int(items[2]), int(items[3]))
      # REF.WRITE
      elif items[0][0] == "w":
        cache.write(int(items[2]), int(items[3]), int(items[4]))
      cache.time += 1
      pbar.update(1)
  pbar.close()

def run_test(trace, size, ways, count, debug):
  memory = Memory()
  cache = Cache(size, ways, debug)
  cache.memory = memory

  start = time.time()
  simulate(memory, cache, trace, count)
  end = time.time()
  print(f"Elapsed: {end - start:.2f} seconds")

  print("Statistics:")
  print(f"  CacheTime: {cache.time}")
  print(f"  CacheRead: {cache.expect_count} ({100.0 * cache.expect_count / cache.time:.2f}%)")
  print(f"  CacheWrite: {cache.write_count} ({100.0 * cache.write_count / cache.time:.2f}%)")
  print(f"  CacheRefill: {cache.refill_count} ({100.0 * cache.refill_count / cache.time:.2f}%)")
  print(f"  CacheEvict: {cache.evict_count} ({100.0 * cache.evict_count / cache.time:.2f}%)")
  print(f"  CacheEvictMiss: {cache.evict_miss_count} ({100.0 * cache.evict_miss_count / cache.time:.2f}%)")
  print(f"  NumMemRead: {memory.read_count} ({100.0 * memory.read_count / (cache.expect_count + cache.write_count):.4f}% of read+write)")

  stats = [
    size,
    cache.block_size,
    cache.n_sets,
    cache.n_ways,
    cache.time,
    cache.expect_count,
    cache.write_count,
    cache.refill_count,
    cache.evict_count,
    cache.evict_miss_count,
    memory.read_count
  ]
  print("Short Report: " + " ".join(map(str, stats)))

def parse_size(size):
  size = size.upper()
  if size.endswith("KB"):
    return 1024 * int(size[:-2])
  elif size.endswith("MB"):
    return 1024 * 1024 * int(size[:-2])
  else:
    return int(size)

if __name__ == "__main__":
  parser = argparse.ArgumentParser(description='cache simulator')
  parser.add_argument('trace', help='path to the trace file')
  parser.add_argument('--size', '-s', default="128KB", help="cache size")
  parser.add_argument('--ways', '-w', default="8", help="number of cache ways")
  parser.add_argument('--count', '-c', type=int, help="max number of trace sequences (in millions)")
  parser.add_argument('--debug', '-d', action='store_true', help="add debug trace")

  args = parser.parse_args()

  # the unit of trace count is 1 million
  trace_count = args.count
  if trace_count is not None:
    trace_count *= 1000000
  # https://stackoverflow.com/questions/845058/how-to-get-the-line-count-of-a-large-file-cheaply-in-python
  if trace_count is None:
    trace_count = sum(1 for _ in open(args.trace))

  size = [parse_size(s) for s in args.size.split(",")]
  ways = [int(w) for w in args.ways.split(",")]
  for s in size:
    for w in ways:
      run_test(args.trace, s, w, trace_count, args.debug)
