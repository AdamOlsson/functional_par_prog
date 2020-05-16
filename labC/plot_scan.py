#!/usr/bin/env python

import json
import sys
import numpy as np

import matplotlib

SIZES = [100, 1000, 10000, 100000, 1000000, 5000000, 10000000]

matplotlib.use('Agg') # For headless use

import matplotlib.pyplot as plt

benchmark = "segscan"#sys.argv[1]
data_sizes = SIZES#list(map(int, sys.argv[2:]))

scan_filename = 'scan-opencl.json'
segscan_filename = 'segscan-opencl.json'

scan_json = json.load(open(scan_filename))
segscan_json = json.load(open(segscan_filename))

scan_measurements = scan_json['scan.fut']['datasets']
segscan_measurements = segscan_json['segscan.fut']['datasets']
# pair_%_i32_bools
scan_runtimes = [ np.mean(scan_measurements['pair_{}_i32_bools'.format(n)]['runtimes']) / 1000
                    for n in data_sizes ]
segscan_runtimes = [ np.mean(segscan_measurements['pair_{}_i32_bools'.format(n)]['runtimes']) / 1000
                    for n in data_sizes ]

speedups = list(map(lambda x, y: x / y, scan_runtimes, segscan_runtimes))

fig, ax1 = plt.subplots()
opencl_runtime_plot = ax1.plot(data_sizes, scan_runtimes, 'b-', label='scan runtime')
c_runtime_plot = ax1.plot(data_sizes, segscan_runtimes, 'g-', label='segscan runtime')
ax1.set_xlabel('Input size')
ax1.set_ylabel('Runtime (ms)', color='k')
ax1.tick_params('y', colors='k')
plt.xticks(data_sizes, rotation='vertical')
ax1.semilogx()
ax2 = ax1.twinx()
speedup_plot = ax2.plot(data_sizes, speedups, 'k-', label='segscan speedup')
ax2.set_ylabel('Speedup', color='k')
ax2.tick_params('y', colors='k')

plots = opencl_runtime_plot + c_runtime_plot + speedup_plot
labels = [p.get_label() for p in plots]
ax1.legend(plots, labels, loc=0)

fig.tight_layout()
# plt.show()

plt.rc('text')
plt.savefig('{}.png'.format(benchmark), bbox_inches='tight')
