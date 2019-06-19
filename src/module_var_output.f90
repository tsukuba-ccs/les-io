module module_var_output
	implicit none

	! -- Output and Restart --

	! Atmosphere
	real(8), allocatable, dimension(:,:,:) :: u
	real(8), allocatable, dimension(:,:,:) :: v
	real(8), allocatable, dimension(:,:,:) :: w
	real(8), allocatable, dimension(:,:,:) :: p
	real(8), allocatable, dimension(:,:,:) :: t
	real(8), allocatable, dimension(:,:,:) :: tke
	
	! Surface
	real(8), allocatable, dimension(:) :: tsfc
	real(8), allocatable, dimension(:,:) :: tg1D
	real(8), allocatable, dimension(:,:) :: rainncv
	real(8), allocatable, dimension(:,:) :: rainncv_int

	! Soil
	real(8), allocatable, dimension(:,:,:) :: smois

	! Moist
	real(8), allocatable, dimension(:,:,:,:) :: moist
	
	! Bin-model
	real(8), allocatable, dimension(:,:,:,:) :: binvar

	! Integral
	real(8), allocatable, dimension(:,:) :: sw_int
	real(8), allocatable, dimension(:,:) :: lw_int
	real(8), allocatable, dimension(:,:,:) :: sw_ra_heating_int
	real(8), allocatable, dimension(:,:,:) :: lw_ra_heating_int
	
	! -- Average --
	
	! Atmosphere
	real, allocatable, dimension(:,:,:) :: um
	real, allocatable, dimension(:,:,:) :: vm
	real, allocatable, dimension(:,:,:) :: wm
	real, allocatable, dimension(:,:,:) :: tm
	real, allocatable, dimension(:,:,:) :: tkem
	
	! ?
	real, allocatable, dimension(:,:,:) :: dm
	real, allocatable, dimension(:,:,:) :: am
	real, allocatable, dimension(:,:,:) :: bm
	
	! Surface
	real, allocatable, dimension(:,:) :: swm
	real, allocatable, dimension(:,:) :: lwm

	! Correlation
	real, allocatable, dimension(:,:,:) :: u2m
	real, allocatable, dimension(:,:,:) :: v2m
	real, allocatable, dimension(:,:,:) :: w2m
	real, allocatable, dimension(:,:,:) :: uvm
	real, allocatable, dimension(:,:,:) :: vwm
	real, allocatable, dimension(:,:,:) :: wum
	real, allocatable, dimension(:,:,:) :: t2m
	real, allocatable, dimension(:,:,:) :: tum
	real, allocatable, dimension(:,:,:) :: tvm
	real, allocatable, dimension(:,:,:) :: twm
	
	! Stress
	real, allocatable, dimension(:,:,:) :: tau11m
	real, allocatable, dimension(:,:,:) :: tau22m
	real, allocatable, dimension(:,:,:) :: tau33m
	real, allocatable, dimension(:,:,:) :: tau12m
	real, allocatable, dimension(:,:,:) :: tau23m
	real, allocatable, dimension(:,:,:) :: tau31m
	real, allocatable, dimension(:,:,:) :: t_flux1m
	real, allocatable, dimension(:,:,:) :: t_flux2m
	real, allocatable, dimension(:,:,:) :: t_flux3m
	
	! Gradient
	real, allocatable, dimension(:,:,:) :: uxm
	real, allocatable, dimension(:,:,:) :: uym
	real, allocatable, dimension(:,:,:) :: uzm
	real, allocatable, dimension(:,:,:) :: vxm
	real, allocatable, dimension(:,:,:) :: vym
	real, allocatable, dimension(:,:,:) :: vzm
	real, allocatable, dimension(:,:,:) :: wxm
	real, allocatable, dimension(:,:,:) :: wym
	real, allocatable, dimension(:,:,:) :: wzm
	real, allocatable, dimension(:,:,:) :: txm
	real, allocatable, dimension(:,:,:) :: tym
	real, allocatable, dimension(:,:,:) :: tzm
	
	! Moist
	real, allocatable, dimension(:,:,:,:) :: moistm
	real, allocatable, dimension(:,:,:,:) :: moist2m
	real, allocatable, dimension(:,:,:,:) :: moistum
	real, allocatable, dimension(:,:,:,:) :: moistvm
	real, allocatable, dimension(:,:,:,:) :: moistwm
	real, allocatable, dimension(:,:,:,:) :: moist_flux1m
	real, allocatable, dimension(:,:,:,:) :: moist_flux2m
	real, allocatable, dimension(:,:,:,:) :: moist_flux3m
	real, allocatable, dimension(:,:,:,:) :: moistxm
	real, allocatable, dimension(:,:,:,:) :: moistym
	real, allocatable, dimension(:,:,:,:) :: moistzm
	
	! Bin-model
	real, allocatable, dimension(:,:,:,:) :: binvarm
	real, allocatable, dimension(:,:,:,:) :: binvar2m
	real, allocatable, dimension(:,:,:,:) :: binvarum
	real, allocatable, dimension(:,:,:,:) :: binvarvm
	real, allocatable, dimension(:,:,:,:) :: binvarwm
	real, allocatable, dimension(:,:,:,:) :: binvar_flux1m
	real, allocatable, dimension(:,:,:,:) :: binvar_flux2m
	real, allocatable, dimension(:,:,:,:) :: binvar_flux3m
	real, allocatable, dimension(:,:,:,:) :: binvarxm
	real, allocatable, dimension(:,:,:,:) :: binvarym
	real, allocatable, dimension(:,:,:,:) :: binvarzm
end module module_var_output
