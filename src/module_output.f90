module module_output
	contains
	subroutine OutputStartInfo()
		use module_param_namelist
		use module_mpi

		implicit none
		
		write(6, '(a49)') "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
		write(6, '(a49)') "***  LES NetCDF BENCHMARK 1.5  by KUSAKA LAB  ***"
		write(6, '(a49)') "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
		write(6, '(a21,i14,a3,i3,a3,i3,a2)') "Num of MPI process : ", nprocs, " ( ", iprocs, " x ", jprocs, " )"
		write(6, '(a21,i14,a3,i4,a3,i4)') "Domain Size        : ", (ide-ids)+1, " x ", (jde-jds)+1, " x ", (kde-kds)+1
		write(6, '(a21,i28)') "soil_model         : ", soil_model
		write(6, '(a21,i28)') "mp_model           : ", mp_model
		write(6, '(a49)') "-------------------------------------------------"	
		
		flush(6)

		return
	end subroutine OutputStartInfo
		
	subroutine OutputEndInfo()
		use module_param_namelist
		use module_mpi

		implicit none
		
		write(6, '(a49)') "-------------------------------------------------"	
		write(6, '(a9)')  "Complete!"
		write(6, '(a49)') "WEB : http://www.geoenv.tsukuba.ac.jp/~kusakaken/"
		write(6, '(a49)') "-------------------------------------------------"	
		
		

		flush(6)

		return
	end subroutine OutputEndInfo
	
	subroutine OutputWriteResult( datasize, maxtime_call, maxtime_write )
		use module_mpi
		implicit none
		
		integer(8), intent(in) :: datasize
		real(4), intent(in) :: maxtime_call, maxtime_write
		
		write(6, '(a28,f11.5,a10)') "   DataSize/process     :  ", dble(datasize)/1024.d0/1024.d0, "[MiB]"
		write(6, '(a28,f11.5,a10)') "   MaximumCallTime      :  ", maxtime_call, "[sec]"
		write(6, '(a28,f11.5,a10)') "   MaximumWriteTime     :  ", maxtime_write, "[sec]"
	        write(6, '(a28,f11.5,a10)') "   Bandwidth            :  ", &
                     ( (dble(datasize)/1024.d0/1024.d0) / dble(maxtime_write) ) * dble(nprocs), "[MiB/sec]"
		
		flush(6)
			
		return	
	end subroutine OutputWriteResult
	
	subroutine OutputReadResult( datasize, maxtime_call, maxtime_write )
		use module_mpi
		implicit none
		
		integer(8), intent(in) :: datasize
		real(4), intent(in) :: maxtime_call, maxtime_write

		write(6, '(a28,f11.5,a10)') "   DataSize/process     :  ", dble(datasize)/1024.d0/1024.d0, "[MiB]"
		write(6, '(a28,f11.5,a10)') "   MaximumCallTime      :  ", maxtime_call, "[sec]"
		write(6, '(a28,f11.5,a10)') "   MaximumReadTime      :  ", maxtime_write, "[sec]"
		write(6, '(a28,f11.5,a10)') "   Bandwidth            :  ", &
                     ( (dble(datasize)/1024.d0/1024.d0) / dble(maxtime_write) ) * dble(nprocs), "[MiB/sec]"
		
		flush(6)

		return
	end subroutine OutputReadResult
end module module_output
