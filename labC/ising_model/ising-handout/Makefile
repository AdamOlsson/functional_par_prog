PROGNAME=ising-gui
include lib/github.com/diku-dk/lys/common.mk


ising_bench: bench_input ising.fut
	futhark bench --backend=opencl ising.fut --json ising_bench-opencl.json
