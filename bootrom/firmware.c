#include <stdint.h>
#include <printf.h>
#include "generated-perf.h"
#include "generated-assertion.h"

#define RECORDER_BASE   0x40000000

#define decl(name, offset) \
  volatile uint64_t * const name = (volatile uint64_t * const)(RECORDER_BASE + (offset));

decl(ref_state, 0x0)
decl(dut_state, 0x8)
decl(system_state, 0x10)
decl(uart_out, 0x18)
decl(uart_out_hex, 0x20)
decl(assertion_state, 0x80)
decl(perf_counter, 0x80 + 0x8 * 16)

#define deref(ptr) (*(volatile uint64_t *)(ptr))

inline void _putchar(char character) {
  deref(uart_out) = character;
}

void output_hex(uint64_t hex);
inline void output_hex(uint64_t hex) {
  deref(uart_out_hex) = hex;
}

void sim_exit(int i);
inline void sim_exit(int i) {
  // performance counters
  if (i == 0) {
    for (int i = 0; i < SVM_PERF_COUNTERS && perf_names[i]; i++) {
      printf("[PERF] %s: 0x", perf_names[i]);
      output_hex(deref(perf_counter + i));
      printf("\n");
    }
  }

  if (i == 0) {
    deref(system_state) = 0xffffffffffffffffUL;
  }
  else {
    deref(system_state) = 1 + i;
  }
}

void main_exit() {
  // assertions
  const int assertion_max = 16;
  const int assertion_real = ((ASSERTION_EXTRACTOR + 63) / 64);
  const int assertion = (assertion_real > assertion_max) ? assertion_max : assertion_real;
  for (int i = 0; i < assertion; i++) {
    volatile uint64_t *assertion_addr = assertion_state + i;
    uint64_t bits = deref(assertion_addr);
    if (bits != 0) {
      int j = 0;
      while (j <= 63 && !(bits & 0x1)) {
        j++;
        bits >>= 1;
      }
      uint64_t index = (i << 6) + j;
      printf("Assertion %d failed: %s\n", index, assertion_messages[index]);
      sim_exit(1);
    }
  }

  // exit from dut
  uint64_t dut_code = deref(dut_state);
  if (dut_code) {
    printf("DUT exits with code = 0x");
    output_hex(dut_code);
    printf("\n");
    sim_exit(1);
  }

  // ref state
  uint64_t ref_code = deref(ref_state);
  if (ref_code != 1) {
    printf("REF aborts with code = 0x");
    output_hex(ref_code);
    printf("\n");
    sim_exit(1);
  }

  printf("HIT GOOD TRAP\n");
  sim_exit(0);
}

void main_boot() {
    void (*func_ptr)(void);
    func_ptr = (void (*)(void))0x80000000;
    func_ptr();
}
