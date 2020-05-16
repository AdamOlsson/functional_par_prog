#!/usr/bin/env python

import json
import sys
import numpy as np

import matplotlib

SIZES = [100, 1000, 10000, 100000, 1000000, 5000000, 10000000]

matplotlib.use('Agg') # For headless use

import matplotlib.pyplot as plt

benchmark = "segreduce"#sys.argv[1]
data_sizes = SIZES#list(map(int, sys.argv[2:]))

reduce_filename = 'reduce-opencl.json'
segreduce_filename = 'segreduce-opencl.json'

reduce_json = json.load(open(reduce_filename))
segreduce_json = json.load(open(segreduce_filename))

reduce_measurements = reduce_json['reduce.fut']['datasets']
segreduce_measurements = segreduce_json['segreduce.fut']['datasets']
# pair_%_i32_bools
reduce_runtimes = [ np.mean(reduce_measurements['pair_{}_i32_bools'.format(n)]['runtimes']) / 1000
                    for n in data_sizes ]
segreduce_runtimes = [ np.mean(segreduce_measurements['pair_{}_i32_bools'.format(n)]['runtimes']) / 1000
                    for n in data_sizes ]

speedups = list(map(lambda x, y: x / y, reduce_runtimes, segreduce_runtimes))

fig, ax1 = plt.subplots()
opencl_runtime_plot = ax1.plot(data_sizes, reduce_runtimes, 'b-', label='reduce runtime')
c_runtime_plot = ax1.plot(data_sizes, segreduce_runtimes, 'g-', label='segreduce runtime')
ax1.set_xlabel('Input size')
ax1.set_ylabel('Runtime (ms)', color='k')
ax1.tick_params('y', colors='k')
plt.xticks(data_sizes, rotation='vertical')
ax1.semilogx()
ax2 = ax1.twinx()
speedup_plot = ax2.plot(data_sizes, speedups, 'k-', label='segreduce speedup')
ax2.set_ylabel('Speedup', color='k')
ax2.tick_params('y', colors='k')

plots = opencl_runtime_plot + c_runtime_plot + speedup_plot
labels = [p.get_label() for p in plots]
ax1.legend(plots, labels, loc=0)

fig.tight_layout()
# plt.show()

plt.rc('text')
plt.savefig('{}.png'.format(benchmark), bbox_inches='tight')
