SVM_DIR = $(abspath .)
export NOOP_HOME = $(SVM_DIR)

init: riscv
	git submodule update --init

BUILD_DIR = $(SVM_DIR)/build

RTL_DIR = $(BUILD_DIR)/rtl

MILL_ARGS += --target-dir $(RTL_DIR) --split-verilog
MILL_ARGS += --difftest-config Tr

ifneq ($(FIRTOOL),)
MILL_ARGS += --firtool-binary-path $(FIRTOOL)
endif

ifneq ($(CONFIG),)
MILL_ARGS += --golden-config $(CONFIG)Config
endif

ifneq ($(PROFILE),)
MILL_ARGS += --dut-profile $(abspath $(PROFILE))
endif

ifneq ($(PLATFORM),)
MILL_ARGS += --platform $(PLATFORM)
endif

ifneq ($(RELEASE),)
MILL_ARGS += --disable-debug
endif

ifneq ($(CONFIG_ARGS),)
MILL_ARGS += $(CONFIG_ARGS)
endif

verilog:
ifeq ($(PROFILE),)
	$(error "you must specify the dut profile")
endif
	mill -i svm.test.runMain svm.Generator $(MILL_ARGS)
	@for file in $(RTL_DIR)/*.sv; do                          \
		sed -i -e 's/$$fatal/xs_assert(`__LINE__)/g' "$$file";           \
		sed -i -e "s/\$$error(/\$$fwrite(32\'h80000002, /g" "$$file";    \
	done

.DEFAULT_GOAL = verilog

riscv:
	git clone https://github.com/riscv-software-src/riscv-isa-sim .riscv-isa-sim
	cd .riscv-isa-sim && git checkout 055624200a34bcc8d4c7acde040466f7d200163 && cd ..
	cp -rn .riscv-isa-sim/riscv .
	rm -rf .riscv-isa-sim

emu: verilog
	$(MAKE) -C ./difftest emu

clean:
	@rm -rf $(BUILD_DIR)

format:
	@mill -i mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources

idea:
	@mill -i mill.idea.GenIdea/idea

.PHONY: verilog riscv clean format
