bin_PROGRAMS = LESBENCHMARK

LESBENCHMARK_SOURCES =          \
	main.f90                    \
	module_allocate_memory.f90  \
	module_benchmark_core.f90   \
	module_mpi.f90              \
	module_output.f90           \
	module_param_namelist.f90   \
	module_read_namelist.f90    \
	module_var_output.f90

LESBENCHMARK_FFLAGS = \
	-g3 -O3 -traceback

LESBENCHMARK_LDFLAGS = \
	-lnetcdff -lnetcdf

main.o : main.f90 module_output.o module_benchmark_core.o module_allocate_memory.o module_var_output.o module_read_namelist.o module_mpi.o
module_allocate_memory.o : module_allocate_memory.f90 module_param_namelist.o module_var_output.o module_mpi.o
module_benchmark_core.o : module_benchmark_core.f90 module_mpi.o module_var_output.o module_param_namelist.o
module_mpi.o : module_mpi.f90 module_param_namelist.o
module_output.o : module_output.f90 module_mpi.o module_param_namelist.o
module_param_namelist.o : module_param_namelist.f90
module_read_namelist.o : module_read_namelist.f90 module_mpi.o module_param_namelist.o
module_var_output.o : module_var_output.f90
