import itertools
import os
import re

import matplotlib.pyplot as plt
import numpy as np
from reportlab.graphics import renderPDF
from reportlab.pdfgen import canvas
from svglib.svglib import svg2rlg


def convert2ms(value, unit):
    conversion_factors = {
        'ms': 1,
        's': 1000,
        'μs': 0.001
    }
    return value * conversion_factors[unit]


def extract_result(file_path):
    with open(file_path, 'r') as file:
        lines = file.read()

    # Use a regular expression to extract mean time and standard deviation
    pattern = re.compile(
        r'benchmarking \S+/(.+?)\n.*?mean\s+(\d+\.\d+)\s*(ms|s|μs).*?\nstd dev\s+(\d+\.\d+)\s*(ms|s|μs)', re.DOTALL)
    matches = pattern.findall(lines)

    if len(matches) == 0:
        raise Exception('No benchmark data point found in file: {}'.format(file_path))

    labels = []
    mean_times = []
    std_devs = []

    for match in matches:
        label = match[0]  # Extract the spec label
        mean_time = float(match[1])
        mean_unit = match[2]
        std_dev = float(match[3])
        std_unit = match[4]

        mean_time_ms = convert2ms(mean_time, mean_unit)
        std_dev_ms = convert2ms(std_dev, std_unit)

        labels.append(label)
        mean_times.append(mean_time_ms)
        std_devs.append(std_dev_ms)

    return labels, mean_times, std_devs


def plot_results(all_results):
    fig, ax = plt.subplots()

    backend2marker = {'CPU': 'o', 'PTX': 's', 'Vulkan': '^'}
    markers = itertools.cycle(('o', 's', '^', 'D', '*', 'x', 'P', 'v'))
    color_cycle = itertools.cycle(plt.cm.tab10.colors)  # Create an iterator for colors
    spec_colors = {}

    for spec, (labels, mean_times, std_devs, keyword) in all_results.items():
        color = next(color_cycle)  # Get a color from the colormap
        spec_colors[spec] = color  # Each benchmark spec has a unique color

        marker = backend2marker.get(keyword, next(markers))  # Get corresponding marker or next in cycle

        # Plot the line
        plt.plot(labels, mean_times, label=spec, marker=marker, color=color, alpha=0.7, clip_on=False)

        # Add the shade for std dev
        mean_times = np.array(mean_times)
        std_devs = np.array(std_devs)
        plt.fill_between(labels, mean_times - std_devs, mean_times + std_devs, color=color, alpha=0.3, clip_on=False)

    ax.set_xlabel('Spec', fontsize=14)
    ax.margins(x=0)
    ax.set_ylabel('Mean Time (ms, lower is better)', fontsize=14)
    ax.ticklabel_format(axis='y', style='sci', scilimits=(0, 0), useMathText=True, useOffset=False)
    ax.set_title('Benchmarking of Multigrid Method', fontsize=16)
    plt.xticks(rotation=90)
    ax.legend(title='Shade: std dev', loc='upper left')
    fig.tight_layout(pad=0)  # Remove extra padding

    svg_file = 'plot_MG.svg'
    pdf_file = 'plot_MG.pdf'

    # Save the figure as SVG
    fig.savefig(svg_file, format='svg')

    # Read saved SVG
    saved_svg = svg2rlg(svg_file)
    pdf_width = saved_svg.width
    pdf_height = saved_svg.height

    # Create a PDF
    c = canvas.Canvas(pdf_file, pagesize=(pdf_width, pdf_height))
    renderPDF.draw(saved_svg, c, 0, 0)
    c.showPage()
    c.save()


if __name__ == '__main__':
    # Fold of the benchmark results
    result_fold = './MG'

    all_results = {}  # Store all benchmark results
    for file_name in os.listdir(result_fold):
        if file_name.endswith('.out'):
            # Extract backend name from file name
            backend_match = re.search(r'(CPU|PTX|Vulkan)', file_name)
            if backend_match:
                backend = backend_match.group(1)
            else:
                raise Exception('Unknown backend: "{}"'.format(file_name))

            # Extract file name as the spec name, including backend name and fusion status
            spec = file_name.replace('.out', '')
            file_path = os.path.join(result_fold, file_name)
            labels, mean_times, std_devs = extract_result(file_path)
            all_results[spec] = (labels, mean_times, std_devs, backend)

    plot_results(dict(sorted(all_results.items())))
