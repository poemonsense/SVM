require_extension('M');
reg_t lhs = zext_xlen(RS1);
reg_t rhs = zext_xlen(RS2);
reg_t mul = speculative_rd * rhs + div_data;
if (rhs == 0)
  assert(speculative_rd == UINT64_MAX)
else
  assert(lhs == mul);
WRITE_RD(speculative_rd);
