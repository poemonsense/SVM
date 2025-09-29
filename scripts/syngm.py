import argparse
import os
from pathlib import Path

class Generator(object):
  def __init__(self, args):
    self.config = args.config

    self.dut_type = self.__detect_dut_type(self.config)
    self.dut_path = args.dut_path

    self.ref_path = Path(__file__).parent.parent.absolute()
    self.cache_size = args.cache_size
    self.cache_ways = args.cache_ways if args.cache_ways is not None else 16
    self.cache_banks = args.cache_banks
    self.cache_sram_ports = args.cache_sram_ports
    self.cache_refill_on_miss = args.cache_refill_on_miss
    self.cache_replacement = args.cache_replacement

    self.release = args.release
    self.debug = args.debug

    self.commands = []
    self.scripts = dict()

  def build(self):
    self.build_dut()
    build_dut_sh = self.cmd_gen("build_dut.sh")

    self.build_ref()
    build_ref_sh = self.cmd_gen("build_ref.sh")

    self.build_emu()
    build_emu_sh = self.cmd_gen("build_emu.sh")

    self.build_others()
    build_others_sh = self.cmd_gen("build_others.sh")

    self.cmd(f"bash {build_dut_sh}")
    self.cmd(f"bash {build_ref_sh}")
    self.cmd(f"bash {build_emu_sh}")
    self.cmd(f"bash {build_others_sh}")
    self.cmd_gen("build.sh")

  def build_dut(self):
    pass

  def build_ref(self):
    pass

  def build_emu(self):
    pass

  def build_others(self):
    pass

  def simulate(self):
    self.cmd_gen("simulate.sh")

  def __detect_dut_type(self, config):
    # Only XiangShan has multiple configs now
    if config.startswith("XiangShan"):
      return "XiangShan"
    else:
      return config

  def cmd(self, c):
    if isinstance(c, str):
      self.commands.append(c)
    elif isinstance(c, list):
      self.commands += c
    else:
      raise ValueError("command must be str or list")

  def cmd_gen(self, filename):
    self.scripts[filename] = self.commands
    self.commands = []
    return filename

  def default_output_dir(self):
    filename = f"{self.__class__.__name__.lower()}_{self.config.lower()}"
    if self.cache_size is not None:
      filename += f"_{self.cache_size}{self.cache_ways}way"
    else:
      filename += "_no_cache"
    return filename

  def generate_script(self, output_dir=None):
    if output_dir is None:
      output_dir = self.default_output_dir()
    os.mkdir(output_dir)
    for filename, commands in self.scripts.items():
      with open(f"{output_dir}/{filename}", "w+") as f:
        f.write("set -xe\n\n")
        for command in commands:
          f.write(f"{command}\n")

class Simulator(Generator):
  def __init__(self, args):
    super().__init__(args)
    self.platform = "sim"

  def build_dut(self):
    self.cmd(f"cd {self.dut_path}")
    command = []
    command.append(f"NOOP_HOME={self.dut_path} make sim-verilog DIFFTEST_GOLDEN=1")
    if self.dut_type == "XiangShan":
      command.append(f"MFC=1 WITH_CONSTANTIN=0 WITH_CHISELDB=0 SIM_ARGS=\"--disable-assert --disable-perf\"")
    if self.config == "XiangShanMinimal":
      command.append(f"CONFIG=MinimalConfig")
    self.cmd(" ".join(command))
    self.cmd("")

  def build_ref(self):
    self.cmd(f"cd {self.ref_path}")
    self.cmd("make clean")
    command = []
    command.append(f"NOOP_HOME={self.ref_path} make CONFIG={self.config} PLATFORM={self.platform}")
    command.append(f"PROFILE={self.dut_path}/build/generated-src/difftest_profile.json")
    config_args = []
    if self.cache_size is not None:
      config_args.append(f"--cache-size {self.cache_size}")
      if self.cache_ways is not None:
        config_args.append(f"--cache-ways {self.cache_ways}")
      if self.cache_banks is not None:
        config_args.append(f"--cache-banks {self.cache_banks}")
      if self.cache_sram_ports is not None:
        config_args.append(f"--cache-sram-ports {self.cache_sram_ports}")
      if self.cache_refill_on_miss is not None:
        config_args.append(f"--cache-refill-on-miss {self.cache_refill_on_miss}")
      if self.cache_replacement is not None:
        config_args.append(f"--cache-repl {self.cache_replacement}")
      config_str = " ".join(config_args)
      command.append(f"CONFIG_ARGS=\"{config_str}\"")
    if self.release:
      command.append("RELEASE=1")
    self.cmd(" ".join(command))
    self.cmd("make -C bootrom")
    self.cmd("")

  def build_emu(self):
    self.cmd(f"cd {self.dut_path}")
    self.cmd("rm -rf build/emu-compile")
    command = []
    command.append(f"NOOP_HOME={self.dut_path} make emu -j16 NO_DIFF=1 RTL_INCLUDE={self.ref_path}/build/rtl/filelist.f")
    command.append(f"WITH_CHISELDB=0 WITH_CONSTANTIN=0 ASSERT_VERBOSE_COND=1 STOP_COND=0")
    if self.debug:
      if self.cache_size is not None:
        command.append("EMU_THREADS=8")
      elif self.dut_type == "XiangShan":
        command.append("EMU_THREADS=4")
      command.append("EMU_TRACE=1")
    self.cmd(" ".join(command))
    self.cmd("")

  def simulate(self, workload, max_cycles=None):
    self.cmd(f"cd {self.dut_path}")
    command = []
    command.append(f"./build/emu -i {workload} -e 0")
    if max_cycles is not None:
      command.append(f"-C {max_cycles}")
    if self.cache_size is None:
      command.append("--copy-ram=2GB --ram-size=4GB")
    command.append("2> stderr.txt")
    self.cmd(" ".join(command))
    self.cmd("")
    super().simulate()


class Palladium(Simulator):
  def __init__(self, args):
    super().__init__(args)
    self.workloads = [args.image]
    self.platform = "palladium"

  def build(self):
    super().build()

    self.extract()
    self.cmd_gen("extract.sh")

    self.build_ixcom()
    self.cmd_gen("build_ixcom.sh")

  def build_others(self):
    scripts = [
      self.prepare_workloads(),
      self.prepare_tar_gz(),
    ]
    for script in scripts:
      self.cmd(f"bash {script}")

  def prepare_workloads(self):
    self.cmd([
      f"cd {self.dut_path}",
      f"mkdir -p workloads",
    ])
    for workload in self.workloads:
      self.cmd(f"cp {workload} workloads/")
    return self.cmd_gen("prepare_workloads.sh")

  def required_dut_files(self):
    return [
      "build/rtl",
      "build/generated-src",
      "difftest",
      "workloads"
    ]

  def required_ref_files(self):
    return [
      "build/rtl",
      "bootrom",
    ]

  def prepare_tar_gz(self):
    self.cmd([
      "work_dir=$(pwd)",
      "work_dir_name=$(basename ${work_dir})",
      "",
      f"cd {self.dut_path}",
      "rm -f build/rtl/*.fir",
      f"tar -czf dut.tar.gz {' '.join(self.required_dut_files())}",
      "mv dut.tar.gz ${work_dir}/",
      "",
      f"cd {self.ref_path}",
      f"tar -czf ref.tar.gz {' '.join(self.required_ref_files())}",
      "mv ref.tar.gz ${work_dir}/",
      "",
      "cd ${work_dir}/..",
      "tar -czf ${work_dir_name}.tar.gz ${work_dir_name}",
      "mv ${work_dir_name}.tar.gz ${work_dir}/",
    ])
    return self.cmd_gen("prepare_tar_gz.sh")

  def build_emu(self):
    pass

  def extract(self):
    self.cmd([
      "mkdir dut",
      "tar -xzf dut.tar.gz -C dut",
      "mkdir ref",
      "tar -xzf ref.tar.gz -C ref",
    ])

  def build_ixcom(self):
    self.cmd("export NOOP_HOME=$(pwd)/dut")
    self.cmd("export REF_PATH=$(pwd)/ref")
    self.cmd("for file in ref/build/rtl/*_init.sv; do")
    self.cmd(f'  sed -i "s|{self.ref_path}|' + '${REF_PATH}|g" $file')
    self.cmd("done")
    self.cmd("cd dut/difftest")
    make_flags = "NO_DIFF=1 RTL_INCLUDE=${REF_PATH}/build/rtl/filelist.f"
    make_flags += " WITH_CHISELDB=0 WITH_CONSTANTIN=0 ASSERT_VERBOSE_COND=1 STOP_COND=0"
    make_flags += " SYNTHESIS=1"
    self.cmd(f"make pldm-build {make_flags}")

  def simulate(self, workload, max_cycles=None):
    pldm_args = f"+workload=../../workloads/{os.path.basename(workload)}"
    if max_cycles is not None:
      pldm_args += f" +max-cycles={max_cycles}"
    if self.cache_size is None:
      pldm_args += f" +copy-ram=2GB"
    self.cmd([
      "export NOOP_HOME=$(pwd)/dut",
      "cd dut/difftest",
      f'make pldm-run SYNTHESIS=1 PLDM_EXTRA_ARGS="{pldm_args}"',
    ])
    Generator.simulate(self)


class XSFpga(Palladium):
  def __init__(self, args):
    super().__init__(args)
    self.workloads = []
    self.release = True
    self.platform = "fpga"

  def required_dut_files(self):
    return [
      "build/rtl",
    ]

  def required_ref_files(self):
    return [
      "build/rtl",
    ]

  def build_dut(self):
    self.cmd(f"cd {self.dut_path}")
    self.cmd(f"NOOP_HOME={self.dut_path} make clean")
    command = []
    command.append(f"NOOP_HOME={self.dut_path} make DIFFTEST_GOLDEN=1")
    if self.dut_type == "NutShell":
      command.append(f"BOARD=PXIe")
    if self.dut_type == "XiangShan":
      command.append(f"MFC=1 WITH_CONSTANTIN=0 WITH_CHISELDB=0 SIM_ARGS=\"--disable-assert --disable-perf\"")
    if self.config == "XiangShanMinimal":
      command.append(f"CONFIG=MinimalConfig")
    self.cmd(" ".join(command))
    if self.dut_type == "NutShell":
      self.cmd('cd build/rtl')
      self.cmd('sed -i "s|module Top(|module XSTop(|g" TopMain.sv')
      self.cmd('mv TopMain.sv TopMain.v')
      self.cmd("")

if __name__ == "__main__":
  parser = argparse.ArgumentParser(description='script generator for svm')
  parser.add_argument('config', help='NutShell, XiangShan, XiangShanMinimal, Rocket, Piper')
  parser.add_argument('--dut-path', '-d', help="dut path")
  parser.add_argument('--cache-size', '-c', help="cache size")
  parser.add_argument('--cache-ways', '-w', help="number of cache ways")
  parser.add_argument('--cache-banks', '-b', help="number of cache banks")
  parser.add_argument('--cache-sram-ports', '-p', help="number of cache sram ports")
  parser.add_argument('--cache-refill-on-miss', help="refill on cache miss")
  parser.add_argument('--cache-replacement', help="replacement policy")
  parser.add_argument('--image', '-i', default="./ready-to-run/linux.bin", help="image (dut-relative)")
  parser.add_argument('--release', action="store_true", help="enable RELEASE=1")
  parser.add_argument('--debug', action="store_true", help="enable EMU_TRACE")
  parser.add_argument('--output', help="output directory name")
  parser.add_argument('--generator', default="Simulator", help="generator class")

  args = parser.parse_args()

  if args.dut_path is None:
    args.dut_path = os.environ['NOOP_HOME']

  runner = eval(f"{args.generator}(args)")
  runner.build()
  runner.simulate(args.image)
  runner.generate_script(args.output)
