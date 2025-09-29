require_extension('M');
sreg_t lhs = sext_xlen(RS1);
sreg_t rhs = sext_xlen(RS2);
reg_t mul = uint64_t(int64_t(div_data) * rhs) + speculative_rd;
if (rhs == 0)
  assert(speculative_rd == lhs)
else if (lhs == INT64_MIN && rhs == -1)
  assert(speculative_rd == 0)
else
  assert(mul == lhs);
WRITE_RD(speculative_rd);
