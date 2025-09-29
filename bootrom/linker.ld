SECTIONS
{
    ROM_BASE = 0x10000;
    ROM_SIZE = 0x10000; /* 64KB */

    . = ROM_BASE;
    .text.start : { *(.text.start) }

    . = ROM_BASE + 0x20;
    .text.boot : { *(.text.boot) }
    .text : { *(.text) }
    .srodata : { *(.srodata) }
    .data : { *(.data) }

    . = ROM_BASE + ROM_SIZE;
    .text.bootrom_end : { *(.text.bootrom_end) }
}
