.section .text.start, "ax", @progbits
.globl _start
_start:
  la sp, bootrom_end
  j main_exit

.section .text.boot, "ax", @progbits
boot:
  la sp, bootrom_end
  j main_boot

.section .text.bootrom_end, "ax", @progbits
bootrom_end:
