.PHONY: data clean plot all

all: plot

plot: exercise_1_1.pdf exercise_1_3.pdf

two_%_i32s:
	futhark dataset -b --i32-bounds=-10000:10000 \
	                -g [$*]i32 -g [$*]i32 > $@

pair_%_i32_bools:
	futhark dataset -b --i32-bounds=-10000:10000 \
	                -g [$*]i32 -g [$*]bool > $@

two_%_f32s:
	futhark dataset -b --f32-bounds=-0:2 -g [$*]f32 -g [$*]f32 > $@


SIZES_PI = 100 10000 1000000
SIZES_INTEGRATE = 100 1000 10000 100000 1000000 

exercise_1_1.pdf: benchmark
	python plot.py exercise_1_1 $(SIZES)

exercise_1_3.pdf: benchmark
	python plot.py exercise_1_3 $(SIZES)

exercise_2.pdf: benchmark
	python plot.py exercise_2 $(SIZES)


estimate_pi.pdf: benchmark
	python plot_estimate_pi.py estimate_pi $(SIZES_PI)

monte_carlo_integration.pdf: benchmark
	python plot_estimate_pi.py monte_carlo_integration $(SIZES_INTEGRATE)


benchmark: exercise_1_1-opencl.json exercise_1_1-c.json exercise_1_3-opencl.json exercise_1_3-c.json

SIZES = 100 1000 10000 100000 1000000 5000000 10000000

exercise_1_1-opencl.json: $(SIZES:%=two_%_i32s) exercise_1_1.fut
	futhark bench --backend=opencl \
	              --json exercise_1_1-opencl.json \
	              exercise_1_1.fut

exercise_1_1-c.json: $(SIZES:%=two_%_i32s) exercise_1_1.fut
	futhark bench --backend=c \
	              --json exercise_1_1-c.json \
	              exercise_1_1.fut

exercise_1_3-opencl.json: $(SIZES:%=two_%_i32s) exercise_1_3.fut
	futhark bench --backend=opencl exercise_1_3.fut --json exercise_1_3-opencl.json

exercise_1_3-c.json: $(SIZES:%=two_%_i32s) exercise_1_3.fut
	futhark bench --backend=c exercise_1_3.fut --json exercise_1_3-c.json

exercise_2-c.json: $(SIZES:%=two_%_i32s) exercise_2.fut
	futhark bench --backend=c exercise_2.fut --json exercise_2-c.json exercise_2.fut

exercise_2-opencl.json: $(SIZES:%=two_%_i32s) exercise_2.fut
	futhark bench --backend=opencl exercise_2.fut --json exercise_2-opencl.json exercise_2.fut

reduce_by_index-opencl.json: $(SIZES:%=two_%_i32s) reduce_by_index.fut
	futhark bench --backend=opencl reduce_by_index.fut --json reduce_by_index-opencl.json

reduce_by_index_built_in-opencl.json: $(SIZES:%=two_%_i32s) reduce_by_index_built_in.fut
	futhark bench --backend=opencl reduce_by_index_built_in.fut --json reduce_by_index_built_in-opencl.json

segscan-opencl.json: $(SIZES:%=pair_%_i32_bools) segscan.fut
	futhark bench --backend=opencl segscan.fut --json segscan-opencl.json segscan.fut

scan-opencl.json: $(SIZES:%=pair_%_i32_bools) scan.fut
	futhark bench --backend=opencl scan.fut --json scan-opencl.json scan.fut

reduce-opencl.json: $(SIZES:%=pair_%_i32_bools) reduce.fut
	futhark bench --backend=opencl reduce.fut --json reduce-opencl.json

segreduce-opencl.json: $(SIZES:%=pair_%_i32_bools) segreduce.fut
	futhark bench --backend=opencl segreduce.fut --json segreduce-opencl.json

estimate_pi-opencl.json: $(SIZES_PI:%=two_%_f32s) estimate_pi.fut
	futhark bench --backend=opencl estimate_pi.fut --json estimate_pi-opencl.json

estimate_pi-c.json: $(SIZES_PI:%=two_%_f32s) estimate_pi.fut
	futhark bench --backend=c estimate_pi.fut --json estimate_pi-c.json

monte_carlo_integration-c.json: $(SIZES_INTEGRATE:%=two_%_f32s) monte_carlo_integration.fut
	futhark bench --backend=c monte_carlo_integration.fut --json monte_carlo_integration-c.json

monte_carlo_integration-opencl.json: $(SIZES_INTEGRATE:%=two_%_f32s) monte_carlo_integration.fut
	futhark bench --backend=opencl monte_carlo_integration.fut --json monte_carlo_integration-opencl.json

# ising_bench:  ising.fut
# 	futhark bench --backend=opencl ising.fut --json ising_bench-opencl.json


clean:
	rm -f two_*_i32s *.json *.pdf *.bin *.c
