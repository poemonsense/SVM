import argparse
import os
import re
import time
import xlsxwriter

class TestResult:
  def __init__(self, run_id, generator):
    self.run_id = run_id
    self.generator = generator
    self.cache_size = None
    self.cache_sets = None
    self.cache_ways = None
    self.cache_banks = None
    self.cache_sram_ports = None
    self.cache_refill_on_read_miss = None
    self.cache_replacement = None

    self.output = None
    self.counters = dict()

def find_all(directory):
  results = []
  for run_id in range(100000):
    run_dir = f"{directory}/{run_id}"
    if not os.path.isdir(run_dir):
      break
    run_sh = f"{run_dir}/run.sh"
    if not os.path.isfile(run_sh):
      print(f"run.sh not found in {run_id}: why?")
      exit(1)
    generator = None
    with open(run_sh, "r") as f:
      lines = f.readlines()
      for line in lines:
        if "python3 SVM/scripts/svm.py" in line:
          items = line.split()
          generator_index = items.index("--generator") + 1
          generator = items[generator_index].lower()
    if generator is None:
      print(f"generator not found in {run_id}: why?")
      exit(1)
    script_dir = f"{run_dir}/generated-scripts"
    if not os.path.isdir(script_dir):
      print(f"Skipping {run_id}: please check what happened")
      exit(1)
    build_log = f"{script_dir}/build.log"
    if not os.path.isfile(build_log):
      print(f"build.log not found in {run_id}: why?")
      exit(1)
    if generator == "simulator":
      simulate_log = f"{script_dir}/simulate.log"
      if not os.path.isfile(simulate_log):
        print(f"simulate.log not found in {run_id}: why?")
        exit(1)
    if not os.path.isfile(f"{run_dir}/success"):
      print(f"success not found for {run_id}: it may not finished")
      exit(1)
    results.append((run_id, script_dir, generator))
  print(f"Total {len(results)} runs found: from {results[0][0]} to {results[-1][0]}")
  return results

def parse_build_log(test, build_log):
  with open(build_log, "r") as f:
    lines = f.readlines()
    for line in lines:
      if "--cache-size" in line:
        test.cache_size = line.split()[line.split().index("--cache-size") + 1]
      elif line.startswith(" nSets: "):
        test.cache_sets = int(line.split()[-1])
      elif line.startswith(" nWays: "):
        test.cache_ways = int(line.split()[-1])
      elif line.startswith(" numSRAMBanks: "):
        test.cache_banks = int(line.split()[-1])
      elif line.startswith(" numSRAMReadPorts: "):
        test.cache_sram_ports = int(line.split()[-1])
      elif line.startswith(" refillOnReadMiss: "):
        test.cache_refill_on_read_miss = line.split()[-1]
      elif line.startswith(" replacement: "):
        test.cache_replacement = line.split()[-1]

def parse_simulate_log(test, simulate_log):
  is_finished = False
  with open(simulate_log, "r") as f:
    lines = f.readlines()
    for line in lines:
      if "HIT GOOD TRAP" in line:
        test.output = "GOOD"
      elif "Assertion" in line and "failed" in line:
        info = line.split("@[")[-1].split("]")[0].split()
        test.output = (info[0], int(info[1].split(":")[0]))
      elif "REF aborts with code" in line:
        test.output = "ABORT"
      elif "[PERF]" in line:
        name = line.split()[1][:-1]
        value = int(line.split()[2][2:], 16)
        test.counters[name] = value
      elif "Guest cycle spent" in line:
        is_finished = True
  if not is_finished:
    test.output = "UNFINISHED"

def collect_results(test_results):
  results = []
  for run_id, script_dir, generator in test_results:
    try:
      test = TestResult(run_id, generator)
      parse_build_log(test, f"{script_dir}/build.log")
      if generator == "simulator":
        parse_simulate_log(test, f"{script_dir}/simulate.log")
      results.append(test)
    except Exception as e:
      print(f"Error in {run_id}: {e}")
      raise e
  results = sorted(results, key=lambda x: x.run_id)
  return results

def update_assertions(test_results, directory):
  svm_path = f"{directory}/SVM"
  if not os.path.isdir(svm_path):
    print(f"SVM not found in {directory}: why?")
    exit(1)
  for test in test_results:
    if test.generator != "simulator":
      continue
    if isinstance(test.output, str):
      continue
    (filename, line) = test.output
    with open(f"{svm_path}/{filename}", "r") as f:
      lines = f.readlines()
      # match assert(cond, messages) in the file
      assert_re = re.compile(r"assert\(([^,]+),\s*(s|p|)\"([^\"]+)\"\)")
      assert_match = assert_re.search(lines[line - 1])
      if assert_match is not None:
        test.output = f"Assertion failed: {assert_match.group(3)}"
      else:
        test.output = f"Assertion failed: {lines[line - 1].strip()}"

def dump_to_xlsx(test_results, output):
  workbook = xlsxwriter.Workbook(output)
  worksheet = workbook.add_worksheet()

  counters = sorted(list(set(sum(map(lambda x: list(x.counters.keys()), test_results), []))))
  title_row = [
    "run_id",
    "generator",
    "cache_size",
    "cache_sets",
    "cache_ways",
    "cache_banks",
    "cache_sram_ports",
    "cache_refill_on_read_miss",
    "cache_replacement",
    "output",
  ] + counters

  for (i, title) in enumerate(title_row):
    worksheet.write(0, i, title)

  row = 1
  col = 0
  for test in test_results:
    worksheet.write(row, col, test.run_id)
    worksheet.write(row, col + 1, test.generator)
    if test.cache_size is not None:
      worksheet.write(row, col + 2, test.cache_size)
    if test.cache_sets is not None:
      worksheet.write(row, col + 3, test.cache_sets)
    if test.cache_ways is not None:
      worksheet.write(row, col + 4, test.cache_ways)
    if test.cache_banks is not None:
      worksheet.write(row, col + 5, test.cache_banks)
    if test.cache_sram_ports is not None:
      worksheet.write(row, col + 6, test.cache_sram_ports)
    if test.cache_refill_on_read_miss is not None:
      worksheet.write(row, col + 7, test.cache_refill_on_read_miss)
    if test.cache_replacement is not None:
      worksheet.write(row, col + 8, test.cache_replacement)
    worksheet.write(row, col + 9, test.output)
    for (i, name) in enumerate(counters):
      if name in test.counters:
        worksheet.write(row, col + 10 + i, test.counters[name])
    row += 1
  workbook.close()

if __name__ == "__main__":
  parser = argparse.ArgumentParser(description='collect results')
  parser.add_argument('directory', help='path to major test dir')
  parser.add_argument('--output', '-o', help="output report name")

  args = parser.parse_args()

  if args.output is None:
    basename = os.path.basename(args.directory)
    timestamp = time.strftime("%Y%m%d-%H%M%S")
    args.output = f"{args.directory}/{timestamp}-report-{basename}.xlsx"

  test_results = find_all(args.directory)
  run_results = collect_results(test_results)
  update_assertions(run_results, args.directory)
  dump_to_xlsx(run_results, args.output)
