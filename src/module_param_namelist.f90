module module_param_namelist
	implicit none

	integer :: iprocs	! division number of x-direction
	integer :: jprocs	! division number of y-direction
	integer :: ids      ! start index in x direction
	integer :: jds      ! start index in y direction
	integer :: kds      ! start index in z direction
	integer :: ide      ! end index in x direction
	integer :: jde      ! end index in y direction
	integer :: kde      ! end index in z direction
	integer :: num_soil
	integer :: soil_model
	integer :: mp_model

	namelist /parallel/ &
		iprocs, jprocs
		
	namelist /domain/ &
		ids, ide, jds, jde, kds, kde, num_soil
	
	namelist /physics/ &
		soil_model, mp_model
end module module_param_namelist
