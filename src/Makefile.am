bin_PROGRAMS = LESBENCHMARK

LESBENCHMARK_SOURCES =          \
	module_var_output.f90       \
	module_param_namelist.f90   \
	module_mpi.f90              \
	module_read_namelist.f90    \
	module_allocate_memory.f90  \
	module_benchmark_core.f90   \
	module_output.f90           \
	main.f90

LESBENCHMARK_LDADD = @NETCDFF_LIBS@ @NETCDF_LIBS@
AM_FCFLAGS = @NETCDFF_CFLAGS@ @NETCDF_CFLAGS@
