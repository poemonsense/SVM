TARGET = bootrom

bootrom_txt = $(TARGET).txt
bootrom_objdump = $(TARGET).objdump

CROSS=riscv64-unknown-elf-
GCC=$(CROSS)gcc
OBJCOPY=$(CROSS)objcopy
OBJDUMP=$(CROSS)objdump

all: $(bootrom_txt) $(bootrom_objdump)

ifeq ($(CROSS),riscv64-linux-gnu-)
SECTIONS = --only-section=.text*
endif

C_SOURCE = $(wildcard *.c)
C_HEADER = $(wildcard *.h)

$(TARGET).bin: $(TARGET).elf
	$(OBJCOPY) -O binary $(SECTIONS) $< $@

$(TARGET).objdump: $(TARGET).elf
	$(OBJDUMP) -D $< > $@

$(TARGET).elf: $(TARGET).S $(C_SOURCE) $(C_HEADER) linker.ld
	$(GCC) -Tlinker.ld $< $(C_SOURCE) -I. -nostdlib -static -Wl,--no-gc-sections -o $@ -flto -march=rv64gc

$(TARGET).txt: $(TARGET).bin
	hexdump -v -e '/8 "%016x\n"' $< > $@

clean:
	@rm -f *.elf *.img *.txt *.objdump
