#!/usr/bin/env python

import json
import sys
import numpy as np

import matplotlib

SIZES = [100, 1000, 10000, 100000, 1000000, 5000000, 10000000]

matplotlib.use('Agg') # For headless use

import matplotlib.pyplot as plt

benchmark = "reduce_by_index"#sys.argv[1]
data_sizes = SIZES#list(map(int, sys.argv[2:]))

reduce_by_index_filename = 'reduce_by_index-opencl.json'
reduce_by_index_built_in_filename = 'reduce_by_index_built_in-opencl.json'

reduce_by_index_json = json.load(open(reduce_by_index_filename))
reduce_by_index_built_in_json = json.load(open(reduce_by_index_built_in_filename))

reduce_by_index_measurements = reduce_by_index_json['reduce_by_index.fut']['datasets']
reduce_by_index_built_in_measurements = reduce_by_index_built_in_json['reduce_by_index_built_in.fut']['datasets']
# pair_%_i32_bools
reduce_by_index_runtimes = [ np.mean(reduce_by_index_measurements['two_{}_i32s'.format(n)]['runtimes']) / 1000
                    for n in data_sizes ]
reduce_by_index_built_in_runtimes = [ np.mean(reduce_by_index_built_in_measurements['two_{}_i32s'.format(n)]['runtimes']) / 1000
                    for n in data_sizes ]

speedups = list(map(lambda x, y: x / y, reduce_by_index_built_in_runtimes, reduce_by_index_runtimes))

fig, ax1 = plt.subplots()
opencl_runtime_plot = ax1.plot(data_sizes, reduce_by_index_runtimes, 'b-', label='reduce_by_index runtime')
c_runtime_plot = ax1.plot(data_sizes, reduce_by_index_built_in_runtimes, 'g-', label='reduce_by_index_built_in runtime')
ax1.set_xlabel('Input size')
ax1.set_ylabel('Runtime (ms)', color='k')
ax1.tick_params('y', colors='k')
plt.xticks(data_sizes, rotation='vertical')
ax1.semilogx()
ax2 = ax1.twinx()
speedup_plot = ax2.plot(data_sizes, speedups, 'k-', label='reduce_by_index_built_in speedup')
ax2.set_ylabel('Speedup', color='k')
ax2.tick_params('y', colors='k')

plots = opencl_runtime_plot + c_runtime_plot + speedup_plot
labels = [p.get_label() for p in plots]
ax1.legend(plots, labels, loc=0)

fig.tight_layout()
# plt.show()

plt.rc('text')
plt.savefig('{}.png'.format(benchmark), bbox_inches='tight')
