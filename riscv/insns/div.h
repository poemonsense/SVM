require_extension('M');
sreg_t lhs = sext_xlen(RS1);
sreg_t rhs = sext_xlen(RS2);
reg_t mul = uint64_t(int64_t(speculative_rd) * rhs) + div_data;
if (rhs == 0)
  assert(speculative_rd == UINT64_MAX)
else if (lhs == INT64_MIN && rhs == -1)
  assert(speculative_rd == INT64_MIN)
else
  assert(mul == lhs);
WRITE_RD(speculative_rd);
