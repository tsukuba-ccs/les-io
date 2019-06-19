module module_read_namelist
	contains
	
	subroutine ReadNamelist()
		use module_param_namelist
		use module_mpi
		implicit none
		
		open( 99, file='namelist.input',status='unknown')
		read( 99, nml=parallel )
		read( 99, nml=domain )	
		read( 99, nml=physics )
		close( 99 )

		return	
	end subroutine ReadNamelist
end module module_read_namelist
