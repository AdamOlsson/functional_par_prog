import json
import numpy as np
import matplotlib.pyplot as plt

opencl_filename = "ising_bench-opencl.json"
c_filename = "ising_bench-c.json"


opencl_data = {"opencl":
        {"#3":{ "params":"(\"20.0f32 20.0f32 200i32 200i32 50i32\")", "stderr":"","runtimes":[3155,3135,3112,3182,3156,3153,3179,2953,3026,2998]},
         "#0":{ "params":"(\"20.0f32 20.0f32 10i32 10i32 60i32\")",   "stderr":"","runtimes":[1041,1101,1373,1164,1121,1223,1300,1207,1280,1431]},
         "#4":{ "params":"(\"20.0f32 20.0f32 200i32 200i32 150i32\")","stderr":"","runtimes":[8417,8963,8372,8370,8721,8477,8422,8493,8407,8461]},
         "#2":{ "params":"(\"20.0f32 20.0f32 200i32 200i32 60i32\")", "stderr":"","runtimes":[3446,3445,3382,3399,3380,3408,3379,3410,3451,3454]},
         "#1":{ "params":"(\"20.0f32 20.0f32 100i32 100i32 60i32\")", "stderr":"","runtimes":[1606,1527,1567,1616,1692,1708,1801,1762,2107,1676]},
         "#5":{ "params":"(\"20.0f32 20.0f32 200i32 200i32 250i32\")","stderr":"","runtimes":[14288,14055,14037,14064,14052,14073,13942,14165,14033,14008]}}}

c_data = {"c":
        {"#3":{ "params":"(\"20.0f32 20.0f32 200i32 200i32 50i32\")" ,"stderr":"","runtimes":[53634,53200,53510,52804,52844,53265,53087,55063,52960,52721]},
         "#0":{ "params":"(\"20.0f32 20.0f32 10i32 10i32 60i32\")"   ,"stderr":"","runtimes":[125,123,123,131,125,124,124,124,125,125]},
         "#4":{ "params":"(\"20.0f32 20.0f32 200i32 200i32 150i32\")","stderr":"","runtimes":[159777,161636,160146,159297,158171,161299,158528,160884,160939,163039]},
         "#2":{ "params":"(\"20.0f32 20.0f32 200i32 200i32 60i32\")" ,"stderr":"","runtimes":[64034,63563,64476,63983,64195,64105,63740,66262,65144,63965]},
         "#1":{ "params":"(\"20.0f32 20.0f32 100i32 100i32 60i32\")" ,"stderr":"","runtimes":[16975,16399,16739,16082,16629,15975,15808,16425,16507,16103]},
         "#5":{ "params":"(\"20.0f32 20.0f32 200i32 200i32 250i32\")","stderr":"","runtimes":[267549,266114,262923,264448,262883,266695,263225,264714,262456,263818]}}}

# opencl_json = None
# with open(opencl_filename) as f:
    # opencl_json = json.load(f)
# 
# c_json = None
# with open("ising_bench-c.json") as f:
    # c_json = json.load(f)

# opencl_measurements = opencl_json[opencl_filename]['datasets']
# c_measurements = c_json[c_filename]['datasets']

opencl_runtimes = [ np.mean(opencl_data["opencl"]['#{}'.format(n)]['runtimes']) / 1000 for n in range(6) ]
c_runtimes      = [ np.mean(c_data["c"]['#{}'.format(n)]['runtimes']) / 1000 for n in range(6) ]
speedups = list(map(lambda x, y: x / y, c_runtimes, opencl_runtimes))

opencl_fixed_framerate = opencl_runtimes[0:3]
opencl_fixed_size = opencl_runtimes[3:6]
c_fixed_framerate = c_runtimes[0:3]
c_fixed_size = c_runtimes[3:6]
speedups_fixed_framerate = speedups[0:3]
speedups_fixed_size = speedups[3:6]


fig, ax1 = plt.subplots()
opencl_runtime_plot = ax1.plot([50,150,250], opencl_fixed_size, 'b-', label='OpenCL runtime')
c_runtime_plot = ax1.plot([50,150,250], c_fixed_size, 'g-', label='Sequential runtime')
ax1.set_xlabel('n')
ax1.set_ylabel('Runtime (ms)', color='k')

ax1.tick_params('y', colors='k')

plt.xticks([50,150,250], rotation='vertical')
ax1.semilogx()
ax2 = ax1.twinx()
speedup_plot = ax2.plot([50,150,250], speedups_fixed_size, 'k-', label='OpenCL speedup')
ax2.set_ylabel('Speedup', color='k')
ax2.tick_params('y', colors='k')

plots = opencl_runtime_plot + c_runtime_plot + speedup_plot
labels = [p.get_label() for p in plots]
ax1.legend(plots, labels, loc=0)

# ax1.title("Fixed n = 60")
fig.tight_layout()
# plt.show()

# plt.rc('text')
plt.savefig('{}.png'.format("fixed_size"), bbox_inches='tight')