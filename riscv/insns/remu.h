require_extension('M');
reg_t lhs = zext_xlen(RS1);
reg_t rhs = zext_xlen(RS2);
reg_t mul = uint64_t(div_data * rhs) + speculative_rd;
if (rhs == 0)
  assert(speculative_rd == sext_xlen(RS1))
else
  assert(mul == lhs);
WRITE_RD(speculative_rd);
