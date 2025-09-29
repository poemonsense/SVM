import argparse
import os

all_tests = {
  "NutShell": {
    "repo": "git@github.com:poemonsense/NutShell.git",
    "branch": "bist",
    "dut_cache": 512,
    "configs": [
      {
        "generator": "Simulator",
      },
      {
        "generator": "Simulator",
        "ref_cache_size": ["128KB", "256KB", "512KB", "1MB", "2MB"],
        "ref_cache_way": ["4", "8", "16"],
        "ref_cache_banks": ["1", "2", "4"],
        "ref_cache_sram_ports": ["2", "3"],
        "ref_cache_refill_on_miss": [None, "read"],
        "ref_cache_replacement": [None, "plru"],
      },
      {
        "generator": "Palladium",
      },
      {
        "generator": "XSFpga",
        "ref_cache_size": ["256KB"],
        "ref_cache_way": ["16"],
        "ref_cache_banks": ["4"],
        "ref_cache_sram_ports": ["3"],
        "ref_cache_refill_on_miss": ["read"],
        "ref_cache_replacement": ["plru"],
      },
    ]
  },
  "XiangShanMinimal": {
    "repo": "git@github.com:poemonsense/XiangShan.git",
    "branch": "poemonsense-work",
    "dut_cache": 512,
    "configs": [
      {
        "generator": "Simulator",
      },
      {
        "generator": "Simulator",
        "ref_cache_size": ["512KB", "1MB", "2MB"],
        "ref_cache_way": ["8", "16"],
        "ref_cache_banks": ["2", "4", "8"],
        "ref_cache_sram_ports": ["2", "3", "4", "5"],
        "ref_cache_refill_on_miss": [None, "read"],
        "ref_cache_replacement": [None, "plru"],
      },
      {
        "generator": "Palladium",
      },
    ]
  },
  "XiangShan": {
    "repo": "git@github.com:poemonsense/XiangShan.git",
    "branch": "poemonsense-work",
    "dut_cache": 2048,
    "configs": [
      {
        "generator": "Simulator",
      },
      {
        "generator": "Simulator",
        "ref_cache_size": ["2MB", "4MB"],
        "ref_cache_way": ["8", "16"],
        "ref_cache_banks": ["4", "8", "16"],
        "ref_cache_sram_ports": ["3", "4", "5"],
        "ref_cache_refill_on_miss": [None, "read"],
        "ref_cache_replacement": [None, "plru"],
      },
      {
        "generator": "Palladium",
      },
    ]
  },
}

def create_test_scripts(test_name, test_info):
  dut_dir_name = test_info["repo"].split("/")[-1].split(".")[0]
  cmd_init_dut = f"cp -r ../{dut_dir_name} ."
  cmd_init_ref = f"cp -r ../SVM ."
  for config in test_info["configs"]:
    gen = config["generator"]
    cache_size_configs = config["ref_cache_size"] if "ref_cache_size" in config else [None]
    cache_way_configs = config["ref_cache_way"] if "ref_cache_way" in config else [None]
    cache_bank_configs = config["ref_cache_banks"] if "ref_cache_banks" in config else [None]
    cache_sram_port_configs = config["ref_cache_sram_ports"] if "ref_cache_sram_ports" in config else [None]
    cache_refill_on_miss_configs = config["ref_cache_refill_on_miss"] if "ref_cache_refill_on_miss" in config else [None]
    cache_replacement_configs = config["ref_cache_replacement"] if "ref_cache_replacement" in config else [None]
    for cache_size in cache_size_configs:
      for cache_way in cache_way_configs:
        for cache_bank in cache_bank_configs:
          for cache_sram_port in cache_sram_port_configs:
            for cache_refill_on_miss in cache_refill_on_miss_configs:
              for cache_replacement in cache_replacement_configs:
                script_dir = "generated-scripts"
                cmd_ref_gen = f"python3 SVM/scripts/svm.py {test_name} --dut-path $(pwd)/{dut_dir_name} --output {script_dir}"
                cmd_ref_gen += f" --generator {gen}"
                if cache_size is not None:
                  cmd_ref_gen += f" -c {cache_size}"
                if cache_way is not None:
                  cmd_ref_gen += f" -w {cache_way}"
                if cache_bank is not None:
                  cmd_ref_gen += f" -b {cache_bank}"
                if cache_sram_port is not None:
                  cmd_ref_gen += f" -p {cache_sram_port}"
                if cache_refill_on_miss is not None:
                  cmd_ref_gen += f" --cache-refill-on-miss {cache_refill_on_miss}"
                if cache_replacement is not None:
                  cmd_ref_gen += f" --cache-replacement {cache_replacement}"
                all_cmd = [
                  cmd_init_dut,
                  cmd_init_ref,
                  cmd_ref_gen,
                  f"cd {script_dir}",
                  "bash build.sh 2> build_stderr.log | tee build.log",
                ]
                if gen == "Simulator":
                  all_cmd.append("bash simulate.sh | tee simulate.log")
                  all_cmd.append(f"cp ../{dut_dir_name}/stderr.txt simulate_stderr.txt")
                # clean up
                all_cmd.append("cd ..")
                if gen == "Simulator":
                  all_cmd.append(f"rm -rf {dut_dir_name} SVM")
                all_cmd.append("touch success")
                yield "\n".join(all_cmd) + "\n"

def create_eval_scripts(test_name, test_info, eval_dir):
  os.mkdir(eval_dir)

  # init script for each test
  num_script = 0
  for script in create_test_scripts(test_name, test_info):
    test_path = f"{eval_dir}/{num_script}"
    os.mkdir(test_path)
    with open(f"{test_path}/run.sh", "w+") as f:
      f.write(f"set -xe\n")
      f.write(script)
    num_script += 1

  # top-level
  with open(f"{eval_dir}/init.sh", "w+") as top:
    top.write("#!/bin/bash\n")
    top.write("set -xe\n")

    top.write(f'git clone --depth 1 -b {test_info["branch"]} --single-branch {test_info["repo"]}\n')
    dut_dir_name = test_info["repo"].split("/")[-1].split(".")[0]
    top.write(f"make -C {dut_dir_name} init\n")
    top.write("git clone --depth 1 git@github.com:poemonsense/SVM.git\n")
    top.write(f"make -C SVM init\n")

  # each test batch as a tmux session
  batch_size = 120
  sleep_interval = 20 if test_name == "nutshell" else 180
  num_batch = (num_script + batch_size - 1) // batch_size
  for batch in range(num_batch):
    lower = batch * batch_size
    upper = lower + batch_size if num_script >= lower + batch_size else lower + num_script % batch_size
    this_batch_size = upper - lower
    tmux_session_name = f"autoeval_{test_name.lower()}_{batch}"
    with open(f"{eval_dir}/init_{batch}.sh", "w+") as top:
      top.write("#!/bin/bash\n")
      top.write("set -xe\n")
      top.write(f'SESSION_NAME="{tmux_session_name}"\n')
      top.write('tmux new-session -d -s $SESSION_NAME -n "0"\n')
      num_window = (this_batch_size + 3) // 4
      # create windows with 4 panes
      for i in range(num_window):
        if i > 0:
          top.write(f'tmux new-window -t $SESSION_NAME -n "{i}"\n')
        top.write(f"tmux split-window -h -t $SESSION_NAME:{i}\n")
        top.write(f"tmux split-window -v -t $SESSION_NAME:{i}.0\n")
        top.write(f"tmux split-window -v -t $SESSION_NAME:{i}.2\n")
      # send the init command to each pane
      for i in range(this_batch_size):
        cmd = f"cd {lower + i} && sleep {sleep_interval * i} && bash run.sh"
        top.write(f"tmux send-keys -t $SESSION_NAME:{i // 4}.{i % 4} '{cmd}' Enter\n")
      # attach to the session
      top.write(f"tmux attach-session -t $SESSION_NAME")


if __name__ == "__main__":
  parser = argparse.ArgumentParser(description='script generator for svm')
  parser.add_argument('config', help='NutShell, XiangShan, XiangShanMinimal, Rocket, Piper')
  parser.add_argument('--output', '-o', help="output directory name")

  args = parser.parse_args()

  if args.output is None:
    args.output = f"test_{args.config.lower()}"

  create_eval_scripts(args.config, all_tests[args.config], args.output)
