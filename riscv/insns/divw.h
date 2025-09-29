require_extension('M');
require_rv64;
sreg_t lhs = sext32(RS1);
sreg_t rhs = sext32(RS2);
assert(sext32(speculative_rd) == speculative_rd);
reg_t mul = uint32_t(int32_t(speculative_rd) * int32_t(rhs)) + uint32_t(div_data);
if (rhs == 0)
  assert(speculative_rd == UINT64_MAX)
else
  assert(uint32_t(lhs) == mul);
WRITE_RD(speculative_rd);
