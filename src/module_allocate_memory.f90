module module_allocate_memory
	contains
	subroutine AllocateMemory()
		use module_mpi
		use module_var_output
		use module_param_namelist
		implicit none
		
		! -- Output and Restart --
		
		! Atmosphere
		allocate( u(ims:ime,jms:jme,kds:kde) )
		allocate( v(ims:ime,jms:jme,kds:kde) )
		allocate( w(ims:ime,jms:jme,kds:kde) )
		allocate( p(ims:ime,jms:jme,kds:kde) )
		allocate( t(ims:ime,jms:jme,kds:kde) )
		allocate( tke(ims:ime,jms:jme,kds:kde) )
		
		! Surface
		allocate( tsfc(1:im1d) )
		allocate( tg1D(1:im1d,1:nzg) )
		allocate( rainncv(ims:ime,jms:jme) )
		allocate( rainncv_int(ims:ime,jms:jme) )
		
		! Soil
		allocate( smois(ims:ime,jms:jme,1:num_soil) )
		
		! Moist
		allocate( moist(ims:ime,jms:jme,kds:kde,sspcs:espcs) )
		
		! Bin-model
		allocate( binvar(ims:ime,jms:jme,kds:kde,sbin:ebin) )
		
		! Integral
		allocate( sw_int(ims:ime,jms:jme) )	
		allocate( lw_int(ims:ime,jms:jme) )	
		allocate( sw_ra_heating_int(ims:ime,jms:jme,kds:kde) )
		allocate( lw_ra_heating_int(ims:ime,jms:jme,kds:kde) )
		
		! -- Average --
		
		! Atmosphere
		allocate( um(ims:ime,jms:jme,kds:kde) )
		allocate( vm(ims:ime,jms:jme,kds:kde) )
		allocate( wm(ims:ime,jms:jme,kds:kde) )
		allocate( tm(ims:ime,jms:jme,kds:kde) )
		allocate( tkem(ims:ime,jms:jme,kds:kde) )
		allocate( dm(ims:ime,jms:jme,kds:kde) )
		allocate( am(ims:ime,jms:jme,kds:kde) )
		allocate( bm(ims:ime,jms:jme,kds:kde) )
		
		! Surface
		allocate( swm(ims:ime,jms:jme) )
		allocate( lwm(ims:ime,jms:jme) )
		
		! Correlation
		allocate( u2m(ims:ime,jms:jme,kds:kde) )
		allocate( v2m(ims:ime,jms:jme,kds:kde) )
		allocate( w2m(ims:ime,jms:jme,kds:kde) )
		allocate( uvm(ims:ime,jms:jme,kds:kde) )
		allocate( vwm(ims:ime,jms:jme,kds:kde) )
		allocate( wum(ims:ime,jms:jme,kds:kde) )
		allocate( t2m(ims:ime,jms:jme,kds:kde) )
		allocate( tum(ims:ime,jms:jme,kds:kde) )
		allocate( tvm(ims:ime,jms:jme,kds:kde) )
		allocate( twm(ims:ime,jms:jme,kds:kde) )
		
		! Stress
		allocate( tau11m(ims:ime,jms:jme,kds:kde) )
		allocate( tau22m(ims:ime,jms:jme,kds:kde) )
		allocate( tau33m(ims:ime,jms:jme,kds:kde) )
		allocate( tau12m(ims:ime,jms:jme,kds:kde) )
		allocate( tau23m(ims:ime,jms:jme,kds:kde) )
		allocate( tau31m(ims:ime,jms:jme,kds:kde) )
		allocate( t_flux1m(ims:ime,jms:jme,kds:kde) )
		allocate( t_flux2m(ims:ime,jms:jme,kds:kde) )
		allocate( t_flux3m(ims:ime,jms:jme,kds:kde) )
		
		! Gradient
		allocate( uxm(ims:ime,jms:jme,kds:kde) )
		allocate( uym(ims:ime,jms:jme,kds:kde) )
		allocate( uzm(ims:ime,jms:jme,kds:kde) )
		allocate( vxm(ims:ime,jms:jme,kds:kde) )
		allocate( vym(ims:ime,jms:jme,kds:kde) )
		allocate( vzm(ims:ime,jms:jme,kds:kde) )
		allocate( wxm(ims:ime,jms:jme,kds:kde) )
		allocate( wym(ims:ime,jms:jme,kds:kde) )
		allocate( wzm(ims:ime,jms:jme,kds:kde) )
		allocate( txm(ims:ime,jms:jme,kds:kde) )
		allocate( tym(ims:ime,jms:jme,kds:kde) )
		allocate( tzm(ims:ime,jms:jme,kds:kde) )
		
		! Moist
		allocate( moistm(ims:ime,jms:jme,kds:kde,sspcs:espcs) )
		allocate( moist2m(ims:ime,jms:jme,kds:kde,sspcs:espcs) )
		allocate( moistum(ims:ime,jms:jme,kds:kde,sspcs:espcs) )
		allocate( moistvm(ims:ime,jms:jme,kds:kde,sspcs:espcs) )
		allocate( moistwm(ims:ime,jms:jme,kds:kde,sspcs:espcs) )
		allocate( moist_flux1m(ims:ime,jms:jme,kds:kde,sspcs:espcs) )
		allocate( moist_flux2m(ims:ime,jms:jme,kds:kde,sspcs:espcs) )
		allocate( moist_flux3m(ims:ime,jms:jme,kds:kde,sspcs:espcs) )
		allocate( moistxm(ims:ime,jms:jme,kds:kde,sspcs:espcs) )
		allocate( moistym(ims:ime,jms:jme,kds:kde,sspcs:espcs) )
		allocate( moistzm(ims:ime,jms:jme,kds:kde,sspcs:espcs) )
		
		! Bin-model
		allocate( binvarm(ims:ime,jms:jme,kds:kde,sbin:ebin) )
		allocate( binvar2m(ims:ime,jms:jme,kds:kde,sbin:ebin) )
		allocate( binvarum(ims:ime,jms:jme,kds:kde,sbin:ebin) )
		allocate( binvarvm(ims:ime,jms:jme,kds:kde,sbin:ebin) )
		allocate( binvarwm(ims:ime,jms:jme,kds:kde,sbin:ebin) )
		allocate( binvar_flux1m(ims:ime,jms:jme,kds:kde,sbin:ebin) )
		allocate( binvar_flux2m(ims:ime,jms:jme,kds:kde,sbin:ebin) )
		allocate( binvar_flux3m(ims:ime,jms:jme,kds:kde,sbin:ebin) )
		allocate( binvarxm(ims:ime,jms:jme,kds:kde,sbin:ebin) )
		allocate( binvarym(ims:ime,jms:jme,kds:kde,sbin:ebin) )
		allocate( binvarzm(ims:ime,jms:jme,kds:kde,sbin:ebin) )
		
		return
	end subroutine AllocateMemory

	subroutine SetRandomValue()
		use module_mpi
		use module_var_output
		use module_param_namelist
		implicit none

		integer :: i, j, k, l

		do k=kds, kde
			do j=jts, jte
				do i=its, ite
					u(i,j,k) = ransu()
					v(i,j,k) = ransu()
					w(i,j,k) = ransu()
					p(i,j,k) = ransu()
					t(i,j,k) = ransu()
					tke(i,j,k) = ransu()
					
					sw_ra_heating_int(i,j,k) = ransu()
					lw_ra_heating_int(i,j,k) = ransu()
					
					um(i,j,k) = ransu()
					vm(i,j,k) = ransu()
					wm(i,j,k) = ransu()
					tm(i,j,k) = ransu()
					tkem(i,j,k) = ransu()
					
					dm(i,j,k) = ransu()
					am(i,j,k) = ransu()
					bm(i,j,k) = ransu()
					
					u2m(i,j,k) = ransu()
					v2m(i,j,k) = ransu()
					w2m(i,j,k) = ransu()
					uvm(i,j,k) = ransu()
					vwm(i,j,k) = ransu()
					wum(i,j,k) = ransu()
					t2m(i,j,k) = ransu()
					tum(i,j,k) = ransu()
					tvm(i,j,k) = ransu()
					twm(i,j,k) = ransu()
					
					tau11m(i,j,k) = ransu()
					tau22m(i,j,k) = ransu()
					tau33m(i,j,k) = ransu()
					tau12m(i,j,k) = ransu()
					tau23m(i,j,k) = ransu()
					tau31m(i,j,k) = ransu()
					t_flux1m(i,j,k) = ransu()
					t_flux2m(i,j,k) = ransu()
					t_flux3m(i,j,k) = ransu()
					
					uxm(i,j,k) = ransu()
					uym(i,j,k) = ransu()
					uzm(i,j,k) = ransu()
					vxm(i,j,k) = ransu()
					vym(i,j,k) = ransu()
					vzm(i,j,k) = ransu()
					wxm(i,j,k) = ransu()
					wym(i,j,k) = ransu()
					wzm(i,j,k) = ransu()
					txm(i,j,k) = ransu()
					tym(i,j,k) = ransu()
					tzm(i,j,k) = ransu()
				end do
			end do
		end do
		
		do j=jts, jte
			do i=its, ite
				rainncv(i,j) = ransu()
				rainncv_int(i,j) = ransu()
				
				sw_int(i,j) = ransu()
				lw_int(i,j) = ransu()
				
				swm(i,j) = ransu()
				lwm(i,j) = ransu()
			end do
		end do
		
		do i=1, im1d
			tsfc(i) = ransu()
		end do
		
		do j=1, nzg
			do i=1, im1d
				tg1D(i,j) = ransu()
			end do
		end do
		
		do k=1, num_soil
			do j=jts, jte
				do i=its, ite
					smois(i,j,k) = ransu()
				end do
			end do
		end do
		
		do l=sspcs, espcs
			do k=kds, kde
				do j=jts, jte
					do i=its, ite
						moist(i,j,k,l) = ransu()
						
						moistm(i,j,k,l) = ransu()
						moist2m(i,j,k,l) = ransu()
						moistum(i,j,k,l) = ransu()
						moistvm(i,j,k,l) = ransu()
						moistwm(i,j,k,l) = ransu()
						moist_flux1m(i,j,k,l) = ransu()
						moist_flux2m(i,j,k,l) = ransu()
						moist_flux3m(i,j,k,l) = ransu()
						moistxm(i,j,k,l) = ransu()
						moistym(i,j,k,l) = ransu()
						moistzm(i,j,k,l) = ransu()
					end do
				end do
			end do
		end do
		
		do l=sbin, ebin
			do k=kds, kde
				do j=jts, jte
					do i=its, ite
						binvar(i,j,k,l) = ransu()
						
						binvarm(i,j,k,l) = ransu()
						binvar2m(i,j,k,l) = ransu()
						binvarum(i,j,k,l) = ransu()
						binvarvm(i,j,k,l) = ransu()
						binvarwm(i,j,k,l) = ransu()
						binvar_flux1m(i,j,k,l) = ransu()
						binvar_flux2m(i,j,k,l) = ransu()
						binvar_flux3m(i,j,k,l) = ransu()
						binvarxm(i,j,k,l) = ransu()
						binvarym(i,j,k,l) = ransu()
						binvarzm(i,j,k,l) = ransu()
					end do
				end do
			end do
		end do
		
		return
	end subroutine SetRandomValue

	function ransu( )
		implicit none

		integer, save :: ii
		real          :: ransu
		
		ii  = 843314861*ii+453816693
		ransu = dble(ii)*0.5d0**31
		
		return
	end function ransu
end module module_allocate_memory
