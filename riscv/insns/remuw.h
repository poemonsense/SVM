require_extension('M');
require_rv64;
reg_t lhs = zext32(RS1);
reg_t rhs = zext32(RS2);
assert(sext32(speculative_rd) == speculative_rd);
reg_t mul = uint32_t(uint32_t(div_data) * uint32_t(rhs)) + uint32_t(speculative_rd);
if (rhs == 0)
  assert(speculative_rd == sext32(lhs))
else
  assert(lhs == mul);
WRITE_RD(speculative_rd);
