program LESBENCHMARK
	use netcdf
	use module_mpi
	use module_read_namelist
	use module_var_output
	use module_allocate_memory
	use module_benchmark_core
	use module_output
		
	!include 'mpif.h'

	implicit none
	
	! Elapsed Time variables
	integer :: ts, te, t_rate, t_max
	real(4) :: time_write, time_call
	real(4) :: maxtime_write, maxtime_call
	integer(8) :: datasize

	call mpi_init( ierr )
	call mpi_comm_size( MPI_COMM_WORLD, nprocs, ierr )
	call mpi_comm_rank( MPI_COMM_WORLD, myrank, ierr )
	
	call ReadNamelist( )
		
	call CheckProcessor( )	
	call SetRange( )
	call SelectPredictVar( )
	call SetBuilding( )
		
	call AllocateMemory( )
	call SetRandomValue( )
	
	! -- Benchmarks START
	if( myrank == 0 ) then
		call OutputStartInfo( )
	end if

	! -- Integral (write)	
	if( myrank == 0 ) then
		print *, "Integral (WRITE)"
	end if
	
	time_write = 0.0
	call system_clock(ts)
		call OutputIntegralNetCDF( datasize, time_write )
	call system_clock(te, t_rate, t_max)
	
	time_call = (te-ts)/real(t_rate)
	call MPI_Reduce(time_call, maxtime_call, 1, MPI_FLOAT, MPI_MAX, 0, MPI_COMM_WORLD, ierr)
	call MPI_Reduce(time_write, maxtime_write, 1, MPI_FLOAT, MPI_MAX, 0, MPI_COMM_WORLD, ierr)

	if( myrank == 0 ) then
		call OutputWriteResult( datasize, maxtime_call, maxtime_write )
	end if

	! -- Instant (write) 
	if( myrank == 0 ) then
		print *, "Instant (WRITE)"
	end if

	time_write = 0.0
	call system_clock(ts)
		call OutputInstantNetCDF( datasize, time_write )
	call system_clock(te, t_rate, t_max)
	
	time_call = (te-ts)/real(t_rate)
	call MPI_Reduce(time_call, maxtime_call, 1, MPI_FLOAT, MPI_MAX, 0, MPI_COMM_WORLD, ierr)
	call MPI_Reduce(time_write, maxtime_write, 1, MPI_FLOAT, MPI_MAX, 0, MPI_COMM_WORLD, ierr)

	if( myrank == 0 ) then
		call OutputWriteResult( datasize, maxtime_call, maxtime_write )
	end if

	! -- Average (write) 
	if( myrank == 0 ) then
		print *, "Average (WRITE)"
	end if

	time_write = 0.0
	call system_clock(ts)
		call OutputAverageNetCDF( datasize, time_write )
	call system_clock(te, t_rate, t_max)
	
	time_call = (te-ts)/real(t_rate)
	call MPI_Reduce(time_call, maxtime_call, 1, MPI_FLOAT, MPI_MAX, 0, MPI_COMM_WORLD, ierr)
	call MPI_Reduce(time_write, maxtime_write, 1, MPI_FLOAT, MPI_MAX, 0, MPI_COMM_WORLD, ierr)

	if( myrank == 0 ) then
		call OutputWriteResult( datasize, maxtime_call, maxtime_write )
	end if
	
	! -- Restart (write) 
	if( myrank == 0 ) then
		print *, "Restart (WRITE)"
	end if

	time_write = 0.0
	call system_clock(ts)
		call OutputRestartNetCDF( datasize, time_write )
	call system_clock(te, t_rate, t_max)
	
	time_call = (te-ts)/real(t_rate)
	call MPI_Reduce(time_call, maxtime_call, 1, MPI_FLOAT, MPI_MAX, 0, MPI_COMM_WORLD, ierr)
	call MPI_Reduce(time_write, maxtime_write, 1, MPI_FLOAT, MPI_MAX, 0, MPI_COMM_WORLD, ierr)

	if( myrank == 0 ) then
		call OutputWriteResult( datasize, maxtime_call, maxtime_write )
	end if

	! -- Restart (read) 
	if( myrank == 0 ) then
		print *, "Restart (READ)"
	end if

	time_write = 0.0
	call system_clock(ts)
		call ReadRestartNetCDF( datasize, time_write )
	call system_clock(te, t_rate, t_max)
	
	time_call = (te-ts)/real(t_rate)
	call MPI_Reduce(time_call, maxtime_call, 1, MPI_FLOAT, MPI_MAX, 0, MPI_COMM_WORLD, ierr)
	call MPI_Reduce(time_write, maxtime_write, 1, MPI_FLOAT, MPI_MAX, 0, MPI_COMM_WORLD, ierr)

	if( myrank == 0 ) then
		call OutputReadResult( datasize, maxtime_call, maxtime_write )
	end if

	! -- Benchmarks END
	
	if( myrank == 0 ) then
		call OutputEndInfo( )
	end if

	call mpi_finalize( ierr )

end program LESBENCHMARK
