#!/usr/bin/env python3

import argparse
import math
import os
import re
import sys

import matplotlib

matplotlib.use("Agg")
matplotlib.rcParams["pdf.fonttype"] = 42
matplotlib.rcParams["ps.fonttype"] = 42
import matplotlib.pyplot as plt


TARGET_COUNTERS = (
    "core_out",
    "core_out_mmu",
    "core_out_load_store",
    "core_out_miss",
    "cache_evict",
    "cache_refill",
)
COUNTER_PATTERN = re.compile(r"^\[\s*(\d+)\]\s+([A-Za-z0-9_]+):\s*(-?\d+)\s*$")
SMOOTH_RADIUS = 5
FIGURE_WIDTH_INCHES = 3.4
FIGURE_WIDTH_HEIGHT_RATIO = 2.5
LINE_ALPHA = 0.72
LEFT_MARGIN = 0.13
RIGHT_MARGIN = 0.88
BOTTOM_MARGIN = 0.24
TOP_MARGIN = 0.84
X_AXIS_CYCLE_SCALE = 2 ** 16
FONT_SIZE = 7.4
AGGREGATION_POINTS = 2 ** 6
LEGEND_Y = 1.005
LEGEND_HEIGHT = 0.1
LEGEND_COLUMN_SPACING = 0.32
LEGEND_HANDLE_LENGTH = 1.1
LEGEND_HANDLE_TEXT_PAD = 0.22


def parse_args():
    parser = argparse.ArgumentParser(
        description="Plot IPC and miss rate curves from NutShell counter logs."
    )
    parser.add_argument("logfile", help="Counter log file, e.g. stdout_linux.txt")
    parser.add_argument(
        "-o",
        "--output",
        default="graph.pdf",
        help="Output figure path (default: graph.pdf)",
    )
    parser.add_argument(
        "--xmax",
        type=float,
        default=None,
        help="Only plot data up to this x-axis value",
    )
    parser.add_argument(
        "--mode",
        choices=("ipc", "lsu", "cache", "ports"),
        default="ipc",
        help="Plot miss_rate with IPC, IPC_lsu, cache activity, or port comparison (default: ipc)",
    )
    parser.add_argument(
        "--other-logfile",
        default=None,
        help="Second counter log file for --mode ports",
    )
    return parser.parse_args()


def candidate_paths(path):
    base_dir = os.path.dirname(os.path.abspath(path))
    base_name = os.path.basename(path)
    candidates = [os.path.abspath(path)]

    if "stdout" in base_name:
        candidates.append(os.path.join(base_dir, base_name.replace("stdout", "stderr", 1)))
        candidates.append(os.path.join(base_dir, base_name.replace("stdout", "stderr")))

    candidates.append(os.path.join(base_dir, "stderr_linux.txt"))
    candidates.append(os.path.join(base_dir, "stderr.txt"))

    unique_candidates = []
    seen = set()
    for candidate in candidates:
        if candidate not in seen:
            seen.add(candidate)
            unique_candidates.append(candidate)
    return unique_candidates


def finalize_sample(cycle, values, samples):
    if cycle is None:
        return
    if all(counter in values for counter in TARGET_COUNTERS):
        sample = {"cycle": cycle}
        for counter in TARGET_COUNTERS:
            sample[counter] = values[counter]
        samples.append(sample)


def parse_samples(path):
    samples = []
    current_cycle = None
    current_values = {}
    last_seen_cycle = None

    with open(path, "r", encoding="utf-8", errors="ignore") as handle:
        for line in handle:
            match = COUNTER_PATTERN.match(line)
            if match is None:
                continue

            cycle = int(match.group(1))
            counter_name = match.group(2)
            if counter_name not in TARGET_COUNTERS:
                continue

            if last_seen_cycle is not None and cycle < last_seen_cycle:
                break
            last_seen_cycle = cycle

            if current_cycle is None:
                current_cycle = cycle
            elif cycle != current_cycle:
                finalize_sample(current_cycle, current_values, samples)
                current_cycle = cycle
                current_values = {}

            current_values[counter_name] = int(match.group(3))

    finalize_sample(current_cycle, current_values, samples)
    return samples


def load_samples(requested_path):
    checked = []
    for candidate in candidate_paths(requested_path):
        checked.append(candidate)
        if not os.path.exists(candidate):
            continue
        samples = parse_samples(candidate)
        if samples:
            return candidate, samples
    checked_text = "\n".join(checked)
    raise FileNotFoundError(
        "No counter samples were found in the requested log or fallback logs:\n"
        f"{checked_text}"
    )


def build_series(samples):
    if len(samples) < 2:
        raise ValueError("At least two counter snapshots are required to compute deltas.")

    x_values = []
    cycle_deltas = []
    core_out_deltas = []
    core_out_mmu_deltas = []
    core_out_lsu_deltas = []
    core_out_miss_deltas = []
    cache_evict_deltas = []
    cache_refill_deltas = []
    ipc = []
    ipc_mmu = []
    ipc_lsu = []
    miss_rate = []
    cache_evict_rate = []
    cache_refill_rate = []

    for prev, curr in zip(samples, samples[1:]):
        cycle_delta = curr["cycle"] - prev["cycle"]
        if cycle_delta <= 0:
            continue

        core_out_delta = curr["core_out"] - prev["core_out"]
        core_out_mmu_delta = curr["core_out_mmu"] - prev["core_out_mmu"]
        core_out_lsu_delta = curr["core_out_load_store"] - prev["core_out_load_store"]
        core_out_miss_delta = curr["core_out_miss"] - prev["core_out_miss"]
        cache_evict_delta = curr["cache_evict"] - prev["cache_evict"]
        cache_refill_delta = curr["cache_refill"] - prev["cache_refill"]

        if min(
            core_out_delta,
            core_out_mmu_delta,
            core_out_lsu_delta,
            core_out_miss_delta,
            cache_evict_delta,
            cache_refill_delta,
        ) < 0:
            continue

        x_values.append(curr["cycle"] / float(X_AXIS_CYCLE_SCALE))
        cycle_deltas.append(cycle_delta)
        core_out_deltas.append(core_out_delta)
        core_out_mmu_deltas.append(core_out_mmu_delta)
        core_out_lsu_deltas.append(core_out_lsu_delta)
        core_out_miss_deltas.append(core_out_miss_delta)
        cache_evict_deltas.append(cache_evict_delta)
        cache_refill_deltas.append(cache_refill_delta)
        ipc.append(core_out_delta / cycle_delta)
        ipc_mmu.append(core_out_mmu_delta / cycle_delta)
        ipc_lsu.append(core_out_lsu_delta / cycle_delta)
        miss_rate.append(
            (core_out_miss_delta / core_out_delta * 100.0) if core_out_delta > 0 else 0.0
        )
        cache_evict_rate.append(cache_evict_delta / cycle_delta)
        cache_refill_rate.append(cache_refill_delta / cycle_delta)

    if not x_values:
        raise ValueError("No valid delta samples were produced from the counter log.")

    return {
        "x": x_values,
        "cycle_delta": cycle_deltas,
        "core_out_delta": core_out_deltas,
        "core_out_mmu_delta": core_out_mmu_deltas,
        "core_out_lsu_delta": core_out_lsu_deltas,
        "core_out_miss_delta": core_out_miss_deltas,
        "cache_evict_delta": cache_evict_deltas,
        "cache_refill_delta": cache_refill_deltas,
        "ipc": ipc,
        "ipc_mmu": ipc_mmu,
        "ipc_lsu": ipc_lsu,
        "miss_rate": miss_rate,
        "cache_evict_rate": cache_evict_rate,
        "cache_refill_rate": cache_refill_rate,
    }


def moving_average(values, radius=SMOOTH_RADIUS):
    if radius <= 0 or len(values) <= 1:
        return list(values)

    prefix_sum = [0.0]
    for value in values:
        prefix_sum.append(prefix_sum[-1] + value)

    smoothed = []
    last_index = len(values) - 1
    for index in range(len(values)):
        left = max(0, index - radius)
        right = min(last_index, index + radius)
        total = prefix_sum[right + 1] - prefix_sum[left]
        smoothed.append(total / (right - left + 1))
    return smoothed


def percentile(values, percent):
    if not values:
        return 0.0

    sorted_values = sorted(values)
    position = (len(sorted_values) - 1) * percent / 100.0
    lower = int(position)
    upper = min(lower + 1, len(sorted_values) - 1)
    weight = position - lower
    return sorted_values[lower] * (1.0 - weight) + sorted_values[upper] * weight


def axis_upper_bound(values, percent=99.9, padding=0.08):
    upper = percentile(values, percent)
    if upper <= 0.0:
        upper = max(values) if values else 1.0
    if upper <= 0.0:
        upper = 1.0
    return upper * (1.0 + padding)


def rounded_axis_limit(value):
    if value <= 0.0:
        return 1.0
    if value <= 5.0:
        return float(math.ceil(value))
    if value <= 10.0:
        return float(math.ceil(value / 2.0) * 2.0)
    magnitude = 10 ** math.floor(math.log10(value))
    scaled = value / magnitude
    if scaled <= 2.0:
        step = 0.2
    elif scaled <= 5.0:
        step = 0.5
    else:
        step = 1.0
    return math.ceil(scaled / step) * step * magnitude


def rounded_small_axis_limit(value, step=0.1):
    if value <= 0.0:
        return step
    return math.ceil(value / step) * step


def evenly_spaced_ticks(limit, tick_count):
    if tick_count < 2:
        return [0.0, limit]
    step = limit / (tick_count - 1)
    return [index * step for index in range(tick_count)]


def trim_series(x_values, *series_list, xmax=None):
    if xmax is None:
        return (x_values, *series_list)

    end = 0
    while end < len(x_values) and x_values[end] <= xmax:
        end += 1
    if end == 0:
        raise ValueError(f"No samples fall within x <= {xmax}.")
    trimmed = [x_values[:end]]
    trimmed.extend(series[:end] for series in series_list)
    return tuple(trimmed)


def aggregate_series(x_values, *series_list, group_size=AGGREGATION_POINTS):
    if group_size <= 1:
        return (x_values, *series_list)

    aggregated_x = []
    aggregated_series = [[] for _ in series_list]

    for start in range(0, len(x_values), group_size):
        end = min(start + group_size, len(x_values))
        chunk_x = x_values[start:end]
        aggregated_x.append(sum(chunk_x) / len(chunk_x))
        for index, series in enumerate(series_list):
            chunk = series[start:end]
            aggregated_series[index].append(sum(chunk) / len(chunk))

    return (aggregated_x, *aggregated_series)


def trim_data(data, xmax=None):
    if xmax is None:
        return data

    end = 0
    x_values = data["x"]
    while end < len(x_values) and x_values[end] <= xmax:
        end += 1
    if end == 0:
        raise ValueError(f"No samples fall within x <= {xmax}.")
    return {key: values[:end] for key, values in data.items()}


def ratio(numerator, denominator, scale=1.0):
    return (numerator / denominator * scale) if denominator > 0 else 0.0


def aggregate_data(data, group_size=AGGREGATION_POINTS):
    if group_size <= 1:
        return data

    aggregated = {
        "x": [],
        "ipc": [],
        "ipc_mmu": [],
        "ipc_lsu": [],
        "miss_rate": [],
        "cache_evict_rate": [],
        "cache_refill_rate": [],
    }

    for start in range(0, len(data["x"]), group_size):
        end = min(start + group_size, len(data["x"]))
        cycle_delta = sum(data["cycle_delta"][start:end])
        core_out_delta = sum(data["core_out_delta"][start:end])
        core_out_mmu_delta = sum(data["core_out_mmu_delta"][start:end])
        core_out_lsu_delta = sum(data["core_out_lsu_delta"][start:end])
        core_out_miss_delta = sum(data["core_out_miss_delta"][start:end])
        cache_evict_delta = sum(data["cache_evict_delta"][start:end])
        cache_refill_delta = sum(data["cache_refill_delta"][start:end])

        aggregated["x"].append(sum(data["x"][start:end]) / (end - start))
        aggregated["ipc"].append(ratio(core_out_delta, cycle_delta))
        aggregated["ipc_mmu"].append(ratio(core_out_mmu_delta, cycle_delta))
        aggregated["ipc_lsu"].append(ratio(core_out_lsu_delta, cycle_delta))
        aggregated["miss_rate"].append(ratio(core_out_miss_delta, core_out_delta, scale=100.0))
        aggregated["cache_evict_rate"].append(ratio(cache_evict_delta, cycle_delta))
        aggregated["cache_refill_rate"].append(ratio(cache_refill_delta, cycle_delta))

    return aggregated


def prepare_data(data, xmax=None):
    return aggregate_data(trim_data(data, xmax=xmax))


def plot_ipc_series(data, output_path, source_path, xmax=None):
    data = prepare_data(data, xmax=xmax)
    x_values = data["x"]
    ipc = data["ipc"]
    ipc_mmu = data["ipc_mmu"]
    ipc_lsu = data["ipc_lsu"]
    miss_rate = data["miss_rate"]
    ipc_smoothed = moving_average(ipc, radius=0)
    ipc_mmu_smoothed = moving_average(ipc_mmu, radius=0)
    ipc_lsu_smoothed = moving_average(ipc_lsu, radius=0)
    miss_rate_smoothed = moving_average(miss_rate, radius=0)

    fig, ax_left = plt.subplots(
        figsize=(FIGURE_WIDTH_INCHES, FIGURE_WIDTH_INCHES / FIGURE_WIDTH_HEIGHT_RATIO)
    )
    ax_right = ax_left.twinx()
    ax_left.set_zorder(ax_right.get_zorder() + 1)
    ax_left.patch.set_visible(False)
    ax_left.spines["top"].set_visible(False)
    ax_right.spines["top"].set_visible(False)

    miss_line = ax_left.plot(
        x_values,
        miss_rate_smoothed,
        color="#003f5c",
        linewidth=1.2,
        alpha=LINE_ALPHA,
        label="miss_rate",
    )[0]
    ipc_line = ax_right.plot(
        x_values,
        ipc_smoothed,
        color="#ff7c00",
        linewidth=0.8,
        alpha=LINE_ALPHA,
        label="IPC",
    )[0]
    ipc_mmu_line = ax_right.plot(
        x_values,
        ipc_mmu_smoothed,
        color="#006400",
        linewidth=0.8,
        alpha=LINE_ALPHA,
        label="IPC_mmu",
    )[0]
    ipc_lsu_line = ax_right.plot(
        x_values,
        ipc_lsu_smoothed,
        color="#6baed6",
        linewidth=0.8,
        alpha=LINE_ALPHA,
        label="IPC_lsu",
    )[0]

    ax_left.set_xlabel(r"Time / $\times 2^{16}$ cycles", fontsize=FONT_SIZE)
    ax_left.set_ylabel("Miss Rate (%)", color="black", fontsize=FONT_SIZE)
    ax_right.set_ylabel("IPC", color="black", fontsize=FONT_SIZE)
    ax_left.tick_params(axis="x", labelsize=FONT_SIZE)
    ax_left.tick_params(axis="y", colors="black", labelsize=FONT_SIZE)
    ax_right.tick_params(axis="y", colors="black", labelsize=FONT_SIZE)
    ax_left.grid(True, linewidth=0.4, alpha=0.35)

    left_upper = rounded_axis_limit(axis_upper_bound(miss_rate_smoothed))
    right_upper = rounded_small_axis_limit(
        axis_upper_bound(ipc_smoothed + ipc_mmu_smoothed + ipc_lsu_smoothed, padding=0.0),
        step=0.1,
    )

    ax_left.set_xlim(x_values[0], x_values[-1])
    ax_left.set_ylim(0, left_upper)
    ax_right.set_ylim(0, right_upper)
    ax_left.set_yticks(evenly_spaced_ticks(left_upper, int(left_upper) + 1))
    ax_right.set_yticks(evenly_spaced_ticks(right_upper, 5))
    ax_left.margins(x=0)

    lines = [miss_line, ipc_line, ipc_mmu_line, ipc_lsu_line]
    labels = [line.get_label() for line in lines]
    ax_left.legend(
        lines,
        labels,
        loc="lower left",
        bbox_to_anchor=(0.0, LEGEND_Y, 1.0, LEGEND_HEIGHT),
        mode="expand",
        ncol=4,
        frameon=False,
        fontsize=FONT_SIZE,
        columnspacing=LEGEND_COLUMN_SPACING,
        handlelength=LEGEND_HANDLE_LENGTH,
        handletextpad=LEGEND_HANDLE_TEXT_PAD,
        borderaxespad=0.0,
    )

    fig.subplots_adjust(
        left=LEFT_MARGIN,
        right=RIGHT_MARGIN,
        bottom=BOTTOM_MARGIN,
        top=TOP_MARGIN,
    )
    fig.savefig(output_path, format="pdf", bbox_inches="tight", pad_inches=0.01)
    plt.close(fig)


def plot_lsu_series(data, output_path, source_path, xmax=None):
    data = prepare_data(data, xmax=xmax)
    x_values = data["x"]
    ipc_lsu = data["ipc_lsu"]
    miss_rate = data["miss_rate"]
    ipc_lsu_smoothed = moving_average(ipc_lsu, radius=0)
    miss_rate_smoothed = moving_average(miss_rate, radius=0)

    fig, ax_left = plt.subplots(
        figsize=(FIGURE_WIDTH_INCHES, FIGURE_WIDTH_INCHES / FIGURE_WIDTH_HEIGHT_RATIO)
    )
    ax_right = ax_left.twinx()
    ax_left.set_zorder(ax_right.get_zorder() + 1)
    ax_left.patch.set_visible(False)
    ax_left.spines["top"].set_visible(False)
    ax_right.spines["top"].set_visible(False)

    miss_line = ax_left.plot(
        x_values,
        miss_rate_smoothed,
        color="#c0392b",
        linewidth=1.2,
        alpha=LINE_ALPHA,
        label="miss_rate",
    )[0]
    ipc_lsu_line = ax_right.plot(
        x_values,
        ipc_lsu_smoothed,
        color="#ff7f0e",
        linewidth=0.7,
        alpha=LINE_ALPHA,
        label="IPC_lsu",
    )[0]

    ax_left.set_xlabel(r"Time / $\times 2^{16}$ cycles", fontsize=FONT_SIZE)
    ax_left.set_ylabel("Miss Rate (%)", color="black", fontsize=FONT_SIZE)
    ax_right.set_ylabel("IPC_lsu", color="black", fontsize=FONT_SIZE)
    ax_left.tick_params(axis="x", labelsize=FONT_SIZE)
    ax_left.tick_params(axis="y", colors="black", labelsize=FONT_SIZE)
    ax_right.tick_params(axis="y", colors="black", labelsize=FONT_SIZE)
    ax_left.grid(True, linewidth=0.4, alpha=0.35)

    left_upper = rounded_axis_limit(axis_upper_bound(miss_rate_smoothed))
    right_upper = rounded_axis_limit(axis_upper_bound(ipc_lsu_smoothed))

    ax_left.set_xlim(x_values[0], x_values[-1])
    ax_left.set_ylim(0, left_upper)
    ax_right.set_ylim(0, right_upper)
    ax_left.set_yticks(evenly_spaced_ticks(left_upper, int(left_upper) + 1))
    ax_right.set_yticks(evenly_spaced_ticks(right_upper, 5))
    ax_left.margins(x=0)

    lines = [miss_line, ipc_lsu_line]
    labels = [line.get_label() for line in lines]
    ax_left.legend(
        lines,
        labels,
        loc="lower left",
        bbox_to_anchor=(0.0, LEGEND_Y, 1.0, LEGEND_HEIGHT),
        mode="expand",
        ncol=2,
        frameon=False,
        fontsize=FONT_SIZE,
        columnspacing=LEGEND_COLUMN_SPACING,
        handlelength=LEGEND_HANDLE_LENGTH,
        handletextpad=LEGEND_HANDLE_TEXT_PAD,
        borderaxespad=0.0,
    )

    fig.subplots_adjust(
        left=LEFT_MARGIN,
        right=RIGHT_MARGIN,
        bottom=BOTTOM_MARGIN,
        top=TOP_MARGIN,
    )
    fig.savefig(output_path, format="pdf", bbox_inches="tight", pad_inches=0.01)
    plt.close(fig)


def plot_cache_series(data, output_path, source_path, xmax=None):
    data = prepare_data(data, xmax=xmax)
    x_values = data["x"]
    miss_rate = data["miss_rate"]
    cache_evict_rate = data["cache_evict_rate"]
    cache_refill_rate = data["cache_refill_rate"]
    miss_rate_smoothed = moving_average(miss_rate, radius=0)
    cache_evict_smoothed = moving_average(cache_evict_rate, radius=0)
    cache_refill_smoothed = moving_average(cache_refill_rate, radius=0)

    fig, ax_left = plt.subplots(
        figsize=(FIGURE_WIDTH_INCHES, FIGURE_WIDTH_INCHES / FIGURE_WIDTH_HEIGHT_RATIO)
    )
    ax_right = ax_left.twinx()
    ax_left.set_zorder(ax_right.get_zorder() + 1)
    ax_left.patch.set_visible(False)
    ax_left.spines["top"].set_visible(False)
    ax_right.spines["top"].set_visible(False)

    miss_line = ax_left.plot(
        x_values,
        miss_rate_smoothed,
        color="#c0392b",
        linewidth=1.2,
        alpha=LINE_ALPHA,
        label="miss_rate",
    )[0]
    evict_line = ax_right.plot(
        x_values,
        cache_evict_smoothed,
        color="#1f77b4",
        linewidth=0.8,
        alpha=LINE_ALPHA,
        label="cache_evict",
    )[0]
    refill_line = ax_right.plot(
        x_values,
        cache_refill_smoothed,
        color="#2ca02c",
        linewidth=0.8,
        alpha=LINE_ALPHA,
        label="cache_refill",
    )[0]

    ax_left.set_xlabel(r"Time / $\times 2^{16}$ cycles", fontsize=FONT_SIZE)
    ax_left.set_ylabel("Miss Rate (%)", color="black", fontsize=FONT_SIZE)
    ax_right.set_ylabel("Cache Req / cycle", color="black", fontsize=FONT_SIZE)
    ax_left.tick_params(axis="x", labelsize=FONT_SIZE)
    ax_left.tick_params(axis="y", colors="black", labelsize=FONT_SIZE)
    ax_right.tick_params(axis="y", colors="black", labelsize=FONT_SIZE)
    ax_left.grid(True, linewidth=0.4, alpha=0.35)

    left_upper = rounded_axis_limit(axis_upper_bound(miss_rate_smoothed))
    right_upper = rounded_axis_limit(
        axis_upper_bound(cache_evict_smoothed + cache_refill_smoothed)
    )

    ax_left.set_xlim(x_values[0], x_values[-1])
    ax_left.set_ylim(0, left_upper)
    ax_right.set_ylim(0, right_upper)
    ax_left.set_yticks(evenly_spaced_ticks(left_upper, int(left_upper) + 1))
    ax_right.set_yticks(evenly_spaced_ticks(right_upper, 5))
    ax_left.margins(x=0)

    lines = [miss_line, evict_line, refill_line]
    labels = [line.get_label() for line in lines]
    ax_left.legend(
        lines,
        labels,
        loc="lower left",
        bbox_to_anchor=(0.0, LEGEND_Y, 1.0, LEGEND_HEIGHT),
        mode="expand",
        ncol=3,
        frameon=False,
        fontsize=FONT_SIZE,
        columnspacing=LEGEND_COLUMN_SPACING,
        handlelength=LEGEND_HANDLE_LENGTH,
        handletextpad=LEGEND_HANDLE_TEXT_PAD,
        borderaxespad=0.0,
    )

    fig.subplots_adjust(
        left=LEFT_MARGIN,
        right=RIGHT_MARGIN,
        bottom=BOTTOM_MARGIN,
        top=TOP_MARGIN,
    )
    fig.savefig(output_path, format="pdf", bbox_inches="tight", pad_inches=0.01)
    plt.close(fig)


def plot_ports_series(data_2port, data_3port, output_path, source_path_2port, source_path_3port, xmax=None):
    data_2port = prepare_data(data_2port, xmax=xmax)
    data_3port = prepare_data(data_3port, xmax=xmax)
    point_count = min(len(data_2port["x"]), len(data_3port["x"]))
    if point_count == 0:
        raise ValueError("No overlapping samples were found for the port comparison.")

    x_values = data_2port["x"][:point_count]
    miss_2port = data_2port["miss_rate"][:point_count]
    miss_3port = data_3port["miss_rate"][:point_count]
    ipc = data_2port["ipc"][:point_count]
    ipc_lsu = data_2port["ipc_lsu"][:point_count]

    fig, ax_left = plt.subplots(
        figsize=(FIGURE_WIDTH_INCHES, FIGURE_WIDTH_INCHES / FIGURE_WIDTH_HEIGHT_RATIO)
    )
    ax_right = ax_left.twinx()
    ax_left.set_zorder(ax_right.get_zorder() + 1)
    ax_left.patch.set_visible(False)
    ax_left.spines["top"].set_visible(False)
    ax_right.spines["top"].set_visible(False)

    miss_2port_line = ax_left.plot(
        x_values,
        miss_2port,
        color="#003f5c",
        linewidth=1.4,
        alpha=LINE_ALPHA,
        label="2-port",
    )[0]
    miss_3port_line = ax_left.plot(
        x_values,
        miss_3port,
        color="#ff7c00",
        linewidth=1.4,
        alpha=LINE_ALPHA,
        label="3-port",
    )[0]
    ipc_line = ax_right.plot(
        x_values,
        ipc,
        color="#74c476",
        linewidth=0.75,
        alpha=LINE_ALPHA,
        label="IPC",
    )[0]
    ipc_lsu_line = ax_right.plot(
        x_values,
        ipc_lsu,
        color="#6baed6",
        linewidth=0.75,
        alpha=LINE_ALPHA,
        label="IPC_lsu",
    )[0]

    ax_left.set_xlabel(r"Time / $\times 2^{16}$ cycles", fontsize=FONT_SIZE)
    ax_left.set_ylabel("Miss Rate (%)", color="black", fontsize=FONT_SIZE)
    ax_right.set_ylabel("IPC", color="black", fontsize=FONT_SIZE)
    ax_left.tick_params(axis="x", labelsize=FONT_SIZE)
    ax_left.tick_params(axis="y", colors="black", labelsize=FONT_SIZE)
    ax_right.tick_params(axis="y", colors="black", labelsize=FONT_SIZE)
    ax_left.grid(True, linewidth=0.4, alpha=0.35)

    left_upper = rounded_axis_limit(axis_upper_bound(miss_2port + miss_3port))
    right_upper = rounded_small_axis_limit(
        axis_upper_bound(ipc + ipc_lsu, padding=0.0),
        step=0.1,
    )

    ax_left.set_xlim(x_values[0], x_values[-1])
    ax_left.set_ylim(0, left_upper)
    ax_right.set_ylim(0, right_upper)
    ax_left.set_yticks(evenly_spaced_ticks(left_upper, int(left_upper) + 1))
    ax_right.set_yticks(evenly_spaced_ticks(right_upper, 5))
    ax_left.margins(x=0)

    lines = [miss_2port_line, miss_3port_line, ipc_line, ipc_lsu_line]
    labels = [line.get_label() for line in lines]
    ax_left.legend(
        lines,
        labels,
        loc="lower left",
        bbox_to_anchor=(0.0, LEGEND_Y, 1.0, LEGEND_HEIGHT),
        mode="expand",
        ncol=4,
        frameon=False,
        fontsize=FONT_SIZE,
        columnspacing=LEGEND_COLUMN_SPACING,
        handlelength=LEGEND_HANDLE_LENGTH,
        handletextpad=LEGEND_HANDLE_TEXT_PAD,
        borderaxespad=0.0,
    )

    fig.subplots_adjust(
        left=LEFT_MARGIN,
        right=RIGHT_MARGIN,
        bottom=BOTTOM_MARGIN,
        top=TOP_MARGIN,
    )
    fig.savefig(output_path, format="pdf", bbox_inches="tight", pad_inches=0.01)
    plt.close(fig)


def main():
    args = parse_args()
    source_path, samples = load_samples(args.logfile)
    data = build_series(samples)
    raw_points = len(data["x"])
    plotted_points = math.ceil(raw_points / AGGREGATION_POINTS)
    if args.mode == "ports":
        if args.other_logfile is None:
            raise ValueError("--mode ports requires --other-logfile.")
        source_path_other, samples_other = load_samples(args.other_logfile)
        data_other = build_series(samples_other)
        plot_ports_series(
            data,
            data_other,
            args.output,
            source_path,
            source_path_other,
            xmax=args.xmax,
        )
        other_raw_points = len(data_other["x"])
        other_plotted_points = math.ceil(other_raw_points / AGGREGATION_POINTS)
        print(f"Parsed {len(samples)} snapshots from {source_path}")
        print(f"Generated {raw_points} raw points")
        print(f"Plotted {plotted_points} aggregated points ({AGGREGATION_POINTS} raw points per group)")
        print(f"Parsed {len(samples_other)} snapshots from {source_path_other}")
        print(f"Generated {other_raw_points} raw points")
        print(f"Plotted {other_plotted_points} aggregated points ({AGGREGATION_POINTS} raw points per group)")
        print(f"Saved plot to {args.output}")
        return

    if args.mode == "cache":
        plot_cache_series(
            data,
            args.output,
            source_path,
            xmax=args.xmax,
        )
    elif args.mode == "lsu":
        plot_lsu_series(
            data,
            args.output,
            source_path,
            xmax=args.xmax,
        )
    else:
        plot_ipc_series(
            data,
            args.output,
            source_path,
            xmax=args.xmax,
        )

    print(f"Parsed {len(samples)} snapshots from {source_path}")
    print(f"Generated {raw_points} raw points")
    print(f"Plotted {plotted_points} aggregated points ({AGGREGATION_POINTS} raw points per group)")
    print(f"Saved plot to {args.output}")


if __name__ == "__main__":
    try:
        main()
    except (FileNotFoundError, ValueError) as error:
        print(f"Error: {error}", file=sys.stderr)
        sys.exit(1)
