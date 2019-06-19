module module_benchmark_core
	contains
	subroutine ReadRestartNetCDF	(	&
			datasize, time_write		&
		)
		use module_param_namelist
		use module_var_output
		use module_mpi
		use netcdf
		
		implicit none
		
		integer(8), intent(inout) :: datasize
		real(4), intent(inout) :: time_write
		
		! Parallel NetCDF variables
		character (len = 30) :: FILE_NAME
		integer :: ncid_read, ret
		integer :: varid_read_u, varid_read_v, varid_read_w, varid_read_t, varid_read_p, varid_read_tke
		integer :: varid_read_tsfc, varid_read_tg1D, varid_read_rainncv, varid_read_rainncv_int
		integer :: varid_read_smois
		integer :: varid_read_binvar, varid_read_moist
		integer :: start2D(2), count2D(2), start3D(3), count3D(3), start4D(4), count4D(4)
		
		! Elapsed Time variables
		integer :: ts, te, t_rate, t_max
		
		FILE_NAME = "./output/lesrst_restart.nc"
		ret = nf90_open_par(FILE_NAME, IOR(NF90_NOWRITE, NF90_MPIIO), &
						MPI_COMM_WORLD, MPI_INFO_NULL, ncid_read)
		
		! Get the varid of the data variable, based on its name.
		! -- ATM --
		ret = nf90_inq_varid(ncid_read, "u", varid_read_u)
		ret = nf90_inq_varid(ncid_read, "v", varid_read_v)
		ret = nf90_inq_varid(ncid_read, "w", varid_read_w)
		ret = nf90_inq_varid(ncid_read, "t", varid_read_t)
		ret = nf90_inq_varid(ncid_read, "p", varid_read_p)
		ret = nf90_inq_varid(ncid_read, "tke", varid_read_tke)
		
		! -- SFC --
		ret = nf90_inq_varid(ncid_read, "tsfc", varid_read_tsfc)
		ret = nf90_inq_varid(ncid_read, "tg1D", varid_read_tg1D)
		ret = nf90_inq_varid(ncid_read, "raincnv", varid_read_rainncv)
		ret = nf90_inq_varid(ncid_read, "rainncv_int", varid_read_rainncv_int)
		
		! -- SOIL --
		if( soil_model .eq. 1 ) then
			ret = nf90_inq_varid(ncid_read, "smois", varid_read_smois)
		endif
		
		! -- BIN --
		ret = nf90_inq_varid(ncid_read, "moist", varid_read_moist)
		if ( opt_bin )  then
			ret = nf90_inq_varid(ncid_read, "binvar", varid_read_binvar)
		endif
		
		!----------------------------------------------------------------------------
		
		! -- ATM --
		start3D = (/ its, jts, kds /)
		count3D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1 /)
		
		call system_clock(ts)
			ret = nf90_get_var(ncid_read, varid_read_u, u, start3D, count3D)
			ret = nf90_get_var(ncid_read, varid_read_v, v, start3D, count3D)
			ret = nf90_get_var(ncid_read, varid_read_w, w, start3D, count3D)
			ret = nf90_get_var(ncid_read, varid_read_t, t, start3D, count3D)
			ret = nf90_get_var(ncid_read, varid_read_p, p, start3D, count3D)
			ret = nf90_get_var(ncid_read, varid_read_tke, tke, start3D, count3D)
		call system_clock(te, t_rate, t_max)
		time_write = time_write + (te-ts)
		
		! -- SFC --
		start2D = (/ 1, myrank+1 /)
		count2D = (/ im1d, 1 /)
		
		call system_clock(ts)
			ret = nf90_get_var(ncid_read, varid_read_tsfc, tsfc, start2D, count2D)
		call system_clock(te, t_rate, t_max)
		time_write = time_write + (te-ts)
		
		start3D = (/ 1, 1, myrank+1 /)
		count3D = (/ im1d, nzg, 1 /)
		
		call system_clock(ts)
			ret = nf90_get_var(ncid_read, varid_read_tg1D, tg1D, start3D, count3D)
		call system_clock(te, t_rate, t_max)
		time_write = time_write + (te-ts)
		
		start2D = (/ its, jts /)
		count2D = (/ (ite-its)+1, (jte-jts)+1 /)
		
		call system_clock(ts)
			ret = nf90_get_var(ncid_read, varid_read_rainncv, rainncv, start2D, count2D)
			ret = nf90_get_var(ncid_read, varid_read_rainncv_int, rainncv_int, start2D, count2D)
		call system_clock(te, t_rate, t_max)
		time_write = time_write + (te-ts)
		
		! -- SOIL --
		start3D = (/ its, jts, 1 /)
		count3D = (/ (ite-its)+1, (jte-jts)+1, num_soil /)
		
		if( soil_model .eq. 1 ) then
			call system_clock(ts)
				ret = nf90_get_var(ncid_read, varid_read_smois, smois, start3D, count3D)
			call system_clock(te, t_rate, t_max)
			time_write = time_write + (te-ts)
		end if
		
		! -- BIN --
		start4D = (/ its, jts, kds, sspcs /)
		count4D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1, (espcs-sspcs)+1 /)
		
		call system_clock(ts)
			ret = nf90_get_var(ncid_read, varid_read_moist, moist, start4D, count4D)
		call system_clock(te, t_rate, t_max)
		time_write = time_write + (te-ts)
		
		if( opt_bin ) then
			start4D = (/ its, jts, kds, sbin /)
			count4D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1, (ebin-sbin)+1 /)
			
			call system_clock(ts)
				ret = nf90_get_var(ncid_read, varid_read_binvar, binvar, start4D, count4D)
			call system_clock(te, t_rate, t_max)
		time_write = time_write + (te-ts)
		endif
		
		! Close the file, freeing all resources.
		ret = nf90_close(ncid_read)
		
		datasize = 8_8 * ( 2_8 * ( int8((ite-its)+1)*int8((jte-jts)+1) ) + &
				 6_8 * ( int8((ite-its)+1)*int8((jte-jts)+1)*int8((kde-kds)+1) ) + &
				 1_8 * ( int8(im1d) ) + &
				 1_8 * ( int8(im1d)*int8(nzg) ) + &
				 1_8 * ( int8((ite-its)+1)*int8((jte-jts)+1)*int8((kde-kds)+1)*int8((espcs-sspcs)+1) ) )
		if( soil_model .eq. 1 ) then
			datasize = datasize + (8_8 * ( int8((ite-its)+1)*int8((jte-jts)+1)*int8(num_soil) ))
		end if
		if( opt_bin ) then
			datasize = datasize + (8_8 * int8((ite-its)+1)*int8((jte-jts)+1)*int8((kde-kds)+1)*int8((ebin-sbin)+1))
		end if

		time_write = time_write/real(t_rate)
		
		return
	end subroutine ReadRestartNetCDF
	
	subroutine OutputIntegralNetCDF (	&
			datasize, time_write		&
		)
		use module_param_namelist
		use module_var_output
		use module_mpi
		use netcdf
		
		implicit none 
		
		integer(8), intent(inout) :: datasize
		real(4), intent(inout) :: time_write
		
		! Local variables
		integer :: i, j, k
		! Parallel NetCDF variables
		character (len = 30) :: FILE_NAME
		integer :: ncid_sum, ret
		integer :: dimid_sum_i, dimid_sum_j, dimid_sum_k
		integer :: dimid2D(2), dimid3D(3)
		integer :: start2D(2), count2D(2), start3D(3), count3D(3)
		integer :: chunksize2D(2), chunksize3D(3)
		integer :: varid_sum_sw, varid_sum_lw, varid_sum_sw_ra, varid_sum_lw_ra
		
		! Elappsed Time variables
		integer :: ts, te, t_rate, t_max
                integer :: fsize
		
		! Create NetCDF File Section - OUT
		FILE_NAME = "./output/lesout_integral.nc"
		ret = nf90_create_par(FILE_NAME, IOR(NF90_NETCDF4, NF90_MPIIO), MPI_COMM_WORLD, MPI_INFO_NULL, ncid_sum)
		
		! Dimension - atm - i, j, k
		ret = nf90_def_dim(ncid_sum, "i", (ide-ids)+1, dimid_sum_i)
		ret = nf90_def_dim(ncid_sum, "j", (jde-jds)+1, dimid_sum_j)
		ret = nf90_def_dim(ncid_sum, "k", (kde-kds)+1, dimid_sum_k)
		dimid2D = (/ dimid_sum_i, dimid_sum_j /)
		dimid3D = (/ dimid_sum_i, dimid_sum_j, dimid_sum_k /)
		
		! Chunksize
		chunksize2D = (/ (ite-its)+1, (jte-jts)+1 /)
		chunksize3D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1 /)
		
		! Variable - u v w t p tke
		ret = nf90_def_var(ncid_sum, "sw_int", NF90_DOUBLE, dimid2D, varid_sum_sw, chunksizes=chunksize2D)
		ret = nf90_def_var(ncid_sum, "lw_int", NF90_DOUBLE, dimid2D, varid_sum_lw, chunksizes=chunksize2D)
		ret = nf90_def_var(ncid_sum, "sw_ra_heating_int", NF90_DOUBLE, dimid3D, varid_sum_sw_ra, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_sum, "lw_ra_heating_int", NF90_DOUBLE, dimid3D, varid_sum_lw_ra, chunksizes=chunksize3D)
		
		! End of Define
		ret = nf90_enddef(ncid_sum)
		
		start2D = (/ its, jts /)
		count2D = (/ (ite-its)+1, (jte-jts)+1 /)	
		start3D = (/ its, jts, kds /)
		count3D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1 /)
		
		! Set Parallel Access Mode INDEPENDENT/COLLECTIVE
		ret = nf90_var_par_access(ncid_sum, varid_sum_sw, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_sum, varid_sum_lw, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_sum, varid_sum_sw_ra, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_sum, varid_sum_sw_ra, NF90_INDEPENDENT)
		
		! Write
		call system_clock(ts)
			ret = nf90_put_var(ncid_sum, varid_sum_sw, sw_int(its:ite,jts:jte), start2D, count2D)
			ret = nf90_put_var(ncid_sum, varid_sum_lw, lw_int(its:ite,jts:jte), start2D, count2D)
			ret = nf90_put_var(ncid_sum, varid_sum_sw_ra, sw_ra_heating_int(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_sum, varid_sum_lw_ra, lw_ra_heating_int(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_sync(ncid_sum)
		call system_clock(te, t_rate, t_max)
		
		ret = nf90_close(ncid_sum)
		
		datasize = 8_8 * ( 2_8*int8((ite-its)+1)*int8((jte-jts)+1) + 2_8*int8((ite-its)+1)*int8((jte-jts)+1)*int8((kde-kds)+1) )
		time_write = (te-ts)/real(t_rate)

		
		return
	end subroutine OutputIntegralNetCDF
	
	subroutine OutputInstantNetCDF (	&
			datasize, time_write		&
		)
		use module_param_namelist
		use module_var_output
		use module_mpi
		use netcdf
		
		implicit none

		integer(8), intent(inout) :: datasize
		real(4), intent(inout) :: time_write
		
		!local
		integer :: irank
		integer :: i, j, k, kg
		
		! Parallel NetCDF variables
		character (len = 30) :: FILE_NAME
		integer :: ncid_out, stat, ret, dimid2D(2), dimid3D(3), dimid4D(4)
		integer :: dimid_out_i, dimid_out_j, dimid_out_k, dimid_out_bin, dimid_out_spcs
		integer :: dimid_out_im1d, dimid_out_kg, dimid_out_nprocs
		integer :: varid_out_u, varid_out_v, varid_out_w, varid_out_t, varid_out_p, varid_out_tke
		integer :: varid_out_tsfc, varid_out_tg1D
		integer :: varid_out_binvar, varid_out_moist
		integer :: start2D(2), count2D(2), start3D(3), count3D(3), start4D(4), count4D(4)
		integer :: chunksize1D(1), chunksize2D(2), chunksize3D(3), chunksize4D(4)
	
		! Elappsed Time variables
		integer :: ts, te, t_rate, t_max
                integer :: fsize
	
		! Create NetCDF File - OUT
		FILE_NAME = "lesout_instant.nc"
		ret = nf90_create_par(FILE_NAME, IOR(NF90_NETCDF4, NF90_MPIIO), MPI_COMM_WORLD, MPI_INFO_NULL, ncid_out)

		! -- ATM --
		! Dimension - atm - i, j, k
		ret = nf90_def_dim(ncid_out, "i", (ide-ids)+1, dimid_out_i)
		ret = nf90_def_dim(ncid_out, "j", (jde-jds)+1, dimid_out_j)
		ret = nf90_def_dim(ncid_out, "k", (kde-kds)+1, dimid_out_k)
		dimid3D = (/ dimid_out_i, dimid_out_j, dimid_out_k /)

		! Chunksize
		chunksize3D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1 /)	
		
		! Variable - u v w t p tke
		ret = nf90_def_var(ncid_out, "u", NF90_FLOAT, dimid3D, varid_out_u, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "v", NF90_FLOAT, dimid3D, varid_out_v, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "w", NF90_FLOAT, dimid3D, varid_out_w, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "t", NF90_FLOAT, dimid3D, varid_out_t, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "p", NF90_FLOAT, dimid3D, varid_out_p, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "tke", NF90_FLOAT, dimid3D, varid_out_tke, chunksizes=chunksize3D)
		
		! -- SFC --
		! Dimension - sfc - im1d, kg, nprocs
		ret = nf90_def_dim(ncid_out, "im1d", im1d, dimid_out_im1d)
		ret = nf90_def_dim(ncid_out, "kg", nzg, dimid_out_kg)
		ret = nf90_def_dim(ncid_out, "nprocs", nprocs, dimid_out_nprocs)
		
		dimid2D = (/ dimid_out_im1d, dimid_out_nprocs /)
		chunksize2D = (/ im1d, 1 /)
		ret = nf90_def_var(ncid_out, "tsfc", NF90_FLOAT, dimid2D, varid_out_tsfc, chunksizes=chunksize2D)
		
		dimid3D = (/ dimid_out_im1d, dimid_out_kg, dimid_out_nprocs /)
		chunksize3D = (/ im1d, nzg, 1 /)
		ret = nf90_def_var(ncid_out, "tg1D", NF90_FLOAT, dimid3D, varid_out_tg1D, chunksizes=chunksize3D)
		
		! -- BIN --
		! Dimension - bin - spcs, bin
		ret = nf90_def_dim(ncid_out, "spcs", (espcs-sspcs)+1, dimid_out_spcs)
		dimid4D = (/ dimid_out_i, dimid_out_j, dimid_out_k, dimid_out_spcs /)
		chunksize4D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1, (espcs-sspcs)+1 /)	
		ret = nf90_def_var(ncid_out, "moist", NF90_FLOAT, dimid4D, varid_out_moist, chunksizes=chunksize4D)
		
		if( opt_bin ) then
			ret = nf90_def_dim(ncid_out, "bin", (ebin-sbin)+1, dimid_out_bin)
			dimid4D = (/ dimid_out_i, dimid_out_j, dimid_out_k, dimid_out_bin /)
			chunksize4D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1, (ebin-sbin)+1 /)	
			ret = nf90_def_var(ncid_out, "binvar", NF90_FLOAT, dimid4D, varid_out_binvar, chunksizes=chunksize4D)
		end if	
		
		! End of Define
		ret = nf90_enddef(ncid_out)
		
		!----------------------------------------------------------------------------
		
		! -- ATM --
		ret = nf90_var_par_access(ncid_out, varid_out_u, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_v, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_w, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_t, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_p, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_tke, NF90_INDEPENDENT)
		
		start3D = (/ its, jts, kds /)
		count3D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1 /)	
		
		call system_clock(ts)
			ret = nf90_put_var(ncid_out, varid_out_u, real(u(its:ite,jts:jte,kds:kde)), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_v, real(v(its:ite,jts:jte,kds:kde)), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_w, real(w(its:ite,jts:jte,kds:kde)), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_t, real(t(its:ite,jts:jte,kds:kde)), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_p, real(p(its:ite,jts:jte,kds:kde)), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_tke, real(tke(its:ite,jts:jte,kds:kde)), start3D, count3D)
			ret = nf90_sync(ncid_out)
		call system_clock(te, t_rate, t_max)
		time_write = time_write + (te-ts)

		! -- SFC --
		ret = nf90_var_par_access(ncid_out, varid_out_tsfc, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_tg1D, NF90_INDEPENDENT)
		
		start2D = (/ 1, myrank+1 /)
		count2D = (/ im1d, 1 /)
		start3D = (/ 1, 1, myrank+1 /)
		count3D = (/ im1d, nzg, 1 /)
		
		call system_clock(ts)
			ret = nf90_put_var(ncid_out, varid_out_tsfc, real(tsfc(1:im1d)), start2D, count2D)
			ret = nf90_put_var(ncid_out, varid_out_tg1D, real(tg1D(1:im1d,1:nzg)), start3D, count3D)
			ret = nf90_sync(ncid_out)
		call system_clock(te, t_rate, t_max)
		time_write = time_write + (te-ts)
		
		! -- BIN --
		ret = nf90_var_par_access(ncid_out, varid_out_moist, NF90_INDEPENDENT)
		
		start4D = (/ its, jts, kds, sspcs /)
		count4D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1, (espcs-sspcs)+1 /)
		
		call system_clock(ts)
			ret = nf90_put_var(ncid_out, varid_out_moist, real(moist(its:ite,jts:jte,kds:kde,sspcs:espcs)), &
                                           start4D, count4D)
			ret = nf90_sync(ncid_out)
		call system_clock(te, t_rate, t_max)
		time_write = time_write + (te-ts)
		
		if( opt_bin ) then
			ret = nf90_var_par_access(ncid_out, varid_out_binvar, NF90_INDEPENDENT)
			
			start4D = (/ its, jts, kds, sbin /)
			count4D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1, (ebin-sbin)+1 /)	
			
			call system_clock(ts)
			ret = nf90_put_var(ncid_out, varid_out_binvar, real(binvar(its:ite,jts:jte,kds:kde,sbin:ebin)), &
                                           start4D, count4D)
			ret = nf90_sync(ncid_out)
			call system_clock(te, t_rate, t_max)
			time_write = time_write + (te-ts)
		end if
		
		! Close the file. This frees up any internal netCDF resources, and flushes any buffers.
		ret = nf90_close(ncid_out)
		
		datasize = 4_8 * ( 6_8 * ( int8((ite-its)+1)*int8((jte-jts)+1)*int8((kde-kds)+1) ) + &
						 1_8 * ( int8(im1d) ) + &
						 1_8 * ( int8(im1d)*int8(nzg) ) + &
						 1_8 * ( int8((ite-its)+1)*int8((jte-jts)+1)*int8((kde-kds)+1)*int8((espcs-sspcs)+1) ) )
		if( opt_bin ) then
			datasize = datasize + (4_8 * int8((ite-its)+1)*int8((jte-jts)+1)*int8((kde-kds)+1)*int8((ebin-sbin)+1))
		end if

		time_write = time_write/real(t_rate)
		
		return 
	end subroutine OutputInstantNetCDF
	
	subroutine OutputAverageNetCDF (	&
			datasize, time_write		&
		)
		use module_param_namelist
		use module_var_output
		use module_mpi
		use netcdf
		
		implicit none 
		
		integer(8), intent(inout) :: datasize
		real(4), intent(inout) :: time_write
		
		! Parallel NetCDF variables
		character (len = 30) :: FILE_NAME
		integer :: ncid_out, stat, ret, dimid2D(2), dimid3D(3), dimid4D(4)
		integer :: dimid_out_i, dimid_out_j, dimid_out_k, dimid_out_bin, dimid_out_spcs
		integer :: dimid_out_im1d, dimid_out_kg, dimid_out_nprocs
		integer :: varid_out_um, varid_out_vm, varid_out_wm, varid_out_tm, varid_out_tkem
		integer :: varid_out_dm, varid_out_am, varid_out_bm
		integer :: varid_out_swm, varid_out_lwm
		integer :: varid_out_u2m, varid_out_v2m, varid_out_w2m
		integer :: varid_out_uvm, varid_out_vwm, varid_out_wum
		integer :: varid_out_t2m, varid_out_tum, varid_out_tvm, varid_out_twm
		integer :: varid_out_tau11m, varid_out_tau22m, varid_out_tau33m
		integer :: varid_out_tau12m, varid_out_tau23m, varid_out_tau31m
		integer :: varid_out_t_flux1m, varid_out_t_flux2m, varid_out_t_flux3m
		integer :: varid_out_uxm, varid_out_uym, varid_out_uzm
		integer :: varid_out_vxm, varid_out_vym, varid_out_vzm
		integer :: varid_out_wxm, varid_out_wym, varid_out_wzm
		integer :: varid_out_txm, varid_out_tym, varid_out_tzm
		integer :: varid_out_moistm, varid_out_moist2m
		integer :: varid_out_moistum, varid_out_moistvm, varid_out_moistwm
		integer :: varid_out_moist_flux1m, varid_out_moist_flux2m, varid_out_moist_flux3m
		integer :: varid_out_moistxm, varid_out_moistym, varid_out_moistzm
		integer :: varid_out_binvarm, varid_out_binvar2m
		integer :: varid_out_binvarum, varid_out_binvarvm, varid_out_binvarwm
		integer :: varid_out_binvar_flux1m, varid_out_binvar_flux2m, varid_out_binvar_flux3m
		integer :: varid_out_binvarxm, varid_out_binvarym, varid_out_binvarzm
		integer :: start2D(2), count2D(2), start3D(3), count3D(3), start4D(4), count4D(4)
		integer :: chunksize1D(1), chunksize2D(2), chunksize3D(3), chunksize4D(4)
		
		! Elappsed Time variables
		integer :: ts, te, t_rate, t_max
		
		! Create NetCDF File - OUT
		FILE_NAME = "./output/lesout_average.nc"
		ret = nf90_create_par(FILE_NAME, IOR(NF90_NETCDF4, NF90_MPIIO), MPI_COMM_WORLD, MPI_INFO_NULL, ncid_out)
	
                 u(:,:,:) = 1.d0; v(:,:,:) = 2.d0; w(:,:,:) = 3.d0
	
		! -- ATM --
		! Dimension - atm - i, j, k
		ret = nf90_def_dim(ncid_out, "i", (ide-ids)+1, dimid_out_i)
		ret = nf90_def_dim(ncid_out, "j", (jde-jds)+1, dimid_out_j)
		ret = nf90_def_dim(ncid_out, "k", (kde-kds)+1, dimid_out_k)
		dimid3D = (/ dimid_out_i, dimid_out_j, dimid_out_k /)

		! Chunksize
		chunksize3D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1 /)	
		
		! Variable - u v w t p tke
		ret = nf90_def_var(ncid_out, "um", NF90_FLOAT, dimid3D, varid_out_um, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "vm", NF90_FLOAT, dimid3D, varid_out_vm, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "wm", NF90_FLOAT, dimid3D, varid_out_wm, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "tm", NF90_FLOAT, dimid3D, varid_out_tm, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "tkem", NF90_FLOAT, dimid3D, varid_out_tkem, chunksizes=chunksize3D)
		
		! -- ? --
		ret = nf90_def_var(ncid_out, "dm", NF90_FLOAT, dimid3D, varid_out_dm, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "am", NF90_FLOAT, dimid3D, varid_out_am, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "bm", NF90_FLOAT, dimid3D, varid_out_bm, chunksizes=chunksize3D)
		
		! -- SFC --
		dimid2D = (/ dimid_out_i, dimid_out_j /)
		chunksize2D = (/ (ite-its)+1, (jte-jts)+1 /)	
		ret = nf90_def_var(ncid_out, "swm", NF90_FLOAT, dimid2D, varid_out_swm, chunksizes=chunksize2D)
		ret = nf90_def_var(ncid_out, "lwm", NF90_FLOAT, dimid2D, varid_out_lwm, chunksizes=chunksize2D)
		
		! -- CORRELATION --
		dimid3D = (/ dimid_out_i, dimid_out_j, dimid_out_k /)
		chunksize3D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1 /)	
		ret = nf90_def_var(ncid_out, "u2m", NF90_FLOAT, dimid3D, varid_out_u2m, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "v2m", NF90_FLOAT, dimid3D, varid_out_v2m, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "w2m", NF90_FLOAT, dimid3D, varid_out_w2m, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "uvm", NF90_FLOAT, dimid3D, varid_out_uvm, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "vwm", NF90_FLOAT, dimid3D, varid_out_vwm, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "wum", NF90_FLOAT, dimid3D, varid_out_wum, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "t2m", NF90_FLOAT, dimid3D, varid_out_t2m, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "tum", NF90_FLOAT, dimid3D, varid_out_tum, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "tvm", NF90_FLOAT, dimid3D, varid_out_tvm, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "twm", NF90_FLOAT, dimid3D, varid_out_twm, chunksizes=chunksize3D)
		
		! -- STRESS --
		dimid3D = (/ dimid_out_i, dimid_out_j, dimid_out_k /)
		chunksize3D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1 /)	
		ret = nf90_def_var(ncid_out, "tau11m", NF90_FLOAT, dimid3D, varid_out_tau11m, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "tau22m", NF90_FLOAT, dimid3D, varid_out_tau22m, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "tau33m", NF90_FLOAT, dimid3D, varid_out_tau33m, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "tau12m", NF90_FLOAT, dimid3D, varid_out_tau12m, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "tau23m", NF90_FLOAT, dimid3D, varid_out_tau23m, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "tau31m", NF90_FLOAT, dimid3D, varid_out_tau31m, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "t_flux1m", NF90_FLOAT, dimid3D, varid_out_t_flux1m, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "t_flux2m", NF90_FLOAT, dimid3D, varid_out_t_flux2m, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "t_flux3m", NF90_FLOAT, dimid3D, varid_out_t_flux3m, chunksizes=chunksize3D)
		
		! -- GRADIENT --
		dimid3D = (/ dimid_out_i, dimid_out_j, dimid_out_k /)
		chunksize3D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1 /)	
		ret = nf90_def_var(ncid_out, "uxm", NF90_FLOAT, dimid3D, varid_out_uxm, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "uym", NF90_FLOAT, dimid3D, varid_out_uym, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "uzm", NF90_FLOAT, dimid3D, varid_out_uzm, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "vxm", NF90_FLOAT, dimid3D, varid_out_vxm, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "vym", NF90_FLOAT, dimid3D, varid_out_vym, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "vzm", NF90_FLOAT, dimid3D, varid_out_vzm, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "wxm", NF90_FLOAT, dimid3D, varid_out_wxm, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "wym", NF90_FLOAT, dimid3D, varid_out_wym, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "wzm", NF90_FLOAT, dimid3D, varid_out_wzm, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "txm", NF90_FLOAT, dimid3D, varid_out_txm, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "tym", NF90_FLOAT, dimid3D, varid_out_tym, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_out, "tzm", NF90_FLOAT, dimid3D, varid_out_tzm, chunksizes=chunksize3D)
		
		! -- BIN --
		! Dimension - bin - spcs, bin
		ret = nf90_def_dim(ncid_out, "spcs", (espcs-sspcs)+1, dimid_out_spcs)
		dimid4D = (/ dimid_out_i, dimid_out_j, dimid_out_k, dimid_out_spcs /)
		chunksize4D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1, (espcs-sspcs)+1 /)	
		ret = nf90_def_var(ncid_out, "moistm", NF90_FLOAT, dimid4D, varid_out_moistm, chunksizes=chunksize4D)
		ret = nf90_def_var(ncid_out, "moist2m", NF90_FLOAT, dimid4D, varid_out_moist2m, chunksizes=chunksize4D)
		ret = nf90_def_var(ncid_out, "moistum", NF90_FLOAT, dimid4D, varid_out_moistum, chunksizes=chunksize4D)
		ret = nf90_def_var(ncid_out, "moistvm", NF90_FLOAT, dimid4D, varid_out_moistvm, chunksizes=chunksize4D)
		ret = nf90_def_var(ncid_out, "moistwm", NF90_FLOAT, dimid4D, varid_out_moistwm, chunksizes=chunksize4D)
		ret = nf90_def_var(ncid_out, "moist_flux1m", NF90_FLOAT, dimid4D, varid_out_moist_flux1m, chunksizes=chunksize4D)
		ret = nf90_def_var(ncid_out, "moist_flux2m", NF90_FLOAT, dimid4D, varid_out_moist_flux2m, chunksizes=chunksize4D)
		ret = nf90_def_var(ncid_out, "moist_flux3m", NF90_FLOAT, dimid4D, varid_out_moist_flux3m, chunksizes=chunksize4D)
		ret = nf90_def_var(ncid_out, "moistxm", NF90_FLOAT, dimid4D, varid_out_moistxm, chunksizes=chunksize4D)
		ret = nf90_def_var(ncid_out, "moistym", NF90_FLOAT, dimid4D, varid_out_moistym, chunksizes=chunksize4D)
		ret = nf90_def_var(ncid_out, "moistzm", NF90_FLOAT, dimid4D, varid_out_moistzm, chunksizes=chunksize4D)
		
		if( opt_bin ) then
			ret = nf90_def_dim(ncid_out, "bin", (ebin-sbin)+1, dimid_out_bin)
			dimid4D = (/ dimid_out_i, dimid_out_j, dimid_out_k, dimid_out_bin /)
			chunksize4D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1, (ebin-sbin)+1 /)	
			ret = nf90_def_var(ncid_out, "binvarm", NF90_FLOAT, dimid4D, varid_out_binvarm, chunksizes=chunksize4D)
			ret = nf90_def_var(ncid_out, "binvar2m", NF90_FLOAT, dimid4D, varid_out_binvar2m, chunksizes=chunksize4D)
			ret = nf90_def_var(ncid_out, "binvarum", NF90_FLOAT, dimid4D, varid_out_binvarum, chunksizes=chunksize4D)
			ret = nf90_def_var(ncid_out, "binvarvm", NF90_FLOAT, dimid4D, varid_out_binvarvm, chunksizes=chunksize4D)
			ret = nf90_def_var(ncid_out, "binvarwm", NF90_FLOAT, dimid4D, varid_out_binvarwm, chunksizes=chunksize4D)
			ret = nf90_def_var(ncid_out, "binvar_flux1m", NF90_FLOAT, dimid4D, varid_out_binvar_flux1m, chunksizes=chunksize4D)
			ret = nf90_def_var(ncid_out, "binvar_flux2m", NF90_FLOAT, dimid4D, varid_out_binvar_flux2m, chunksizes=chunksize4D)
			ret = nf90_def_var(ncid_out, "binvar_flux3m", NF90_FLOAT, dimid4D, varid_out_binvar_flux3m, chunksizes=chunksize4D)
			ret = nf90_def_var(ncid_out, "binvarxm", NF90_FLOAT, dimid4D, varid_out_binvarxm, chunksizes=chunksize4D)
			ret = nf90_def_var(ncid_out, "binvarym", NF90_FLOAT, dimid4D, varid_out_binvarym, chunksizes=chunksize4D)
			ret = nf90_def_var(ncid_out, "binvarzm", NF90_FLOAT, dimid4D, varid_out_binvarzm, chunksizes=chunksize4D)
		end if	
		
		! End of Define
		ret = nf90_enddef(ncid_out)
		
		!----------------------------------------------------------------------------
		
		! -- ATM --
		ret = nf90_var_par_access(ncid_out, varid_out_um, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_vm, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_wm, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_tm, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_tkem, NF90_INDEPENDENT)
		
		start3D = (/ its, jts, kds /)
		count3D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1 /)	
		
		call system_clock(ts)
			ret = nf90_put_var(ncid_out, varid_out_um, um(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_vm, vm(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_wm, wm(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_tm, tm(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_tkem, tkem(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_sync(ncid_out)
		call system_clock(te, t_rate, t_max)
		time_write = time_write + (te-ts)
		
		! -- ? --
		ret = nf90_var_par_access(ncid_out, varid_out_dm, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_am, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_bm, NF90_INDEPENDENT)
		
		start3D = (/ its, jts, kds /)
		count3D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1 /)	
		
		call system_clock(ts)
			ret = nf90_put_var(ncid_out, varid_out_dm, dm(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_am, am(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_bm, bm(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_sync(ncid_out)
		call system_clock(te, t_rate, t_max)
		time_write = time_write + (te-ts)
		
		! -- SFC --
		ret = nf90_var_par_access(ncid_out, varid_out_swm, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_lwm, NF90_INDEPENDENT)
		
		start2D = (/ its, jts /)
		count2D = (/ (ite-its)+1, (jte-jts)+1 /)	
		
		call system_clock(ts)
			ret = nf90_put_var(ncid_out, varid_out_swm, swm(its:ite,jts:jte), start2D, count2D)
			ret = nf90_put_var(ncid_out, varid_out_lwm, lwm(its:ite,jts:jte), start2D, count2D)
			ret = nf90_sync(ncid_out)
		call system_clock(te, t_rate, t_max)
		time_write = time_write + (te-ts)
		
		! -- CORRELATION --
		ret = nf90_var_par_access(ncid_out, varid_out_u2m, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_v2m, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_w2m, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_uvm, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_vwm, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_wum, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_t2m, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_tum, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_tvm, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_twm, NF90_INDEPENDENT)
		
		start3D = (/ its, jts, kds /)
		count3D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1 /)	
		
		call system_clock(ts)
			ret = nf90_put_var(ncid_out, varid_out_u2m, u2m(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_v2m, v2m(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_w2m, w2m(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_uvm, uvm(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_vwm, vwm(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_wum, wum(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_t2m, t2m(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_tum, tum(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_tvm, tvm(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_twm, twm(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_sync(ncid_out)
		call system_clock(te, t_rate, t_max)
		time_write = time_write + (te-ts)
		
		! -- STRESS --
		ret = nf90_var_par_access(ncid_out, varid_out_tau11m, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_tau22m, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_tau33m, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_tau12m, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_tau23m, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_tau31m, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_t_flux1m, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_t_flux2m, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_t_flux3m, NF90_INDEPENDENT)
		
		start3D = (/ its, jts, kds /)
		count3D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1 /)	
		
		call system_clock(ts)
			ret = nf90_put_var(ncid_out, varid_out_tau11m, tau11m(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_tau22m, tau22m(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_tau33m, tau33m(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_tau12m, tau12m(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_tau23m, tau23m(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_tau31m, tau31m(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_t_flux1m, t_flux1m(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_t_flux2m, t_flux2m(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_t_flux3m, t_flux3m(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_sync(ncid_out)
		call system_clock(te, t_rate, t_max)
		time_write = time_write + (te-ts)
		
		! -- GRADIENT --
		ret = nf90_var_par_access(ncid_out, varid_out_uxm, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_uym, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_uzm, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_vxm, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_vym, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_vzm, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_wxm, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_wym, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_wzm, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_txm, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_tym, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_tzm, NF90_INDEPENDENT)
		
		start3D = (/ its, jts, kds /)
		count3D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1 /)	
		
		call system_clock(ts)
			ret = nf90_put_var(ncid_out, varid_out_uxm, uxm(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_uym, uym(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_uzm, uzm(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_vxm, vxm(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_vym, vym(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_vzm, vzm(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_wxm, wxm(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_wym, wym(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_wzm, wzm(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_txm, txm(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_tym, tym(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_out, varid_out_tzm, tzm(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_sync(ncid_out)
		call system_clock(te, t_rate, t_max)
		time_write = time_write + (te-ts)
		
		! -- BIN --
		ret = nf90_var_par_access(ncid_out, varid_out_moistm, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_moist2m, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_moistum, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_moistvm, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_moistwm, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_moist_flux1m, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_moist_flux2m, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_moist_flux3m, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_moistxm, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_moistym, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_out, varid_out_moistzm, NF90_INDEPENDENT)
		
		start4D = (/ its, jts, kds, sspcs /)
		count4D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1, (espcs-sspcs)+1 /)
		
		call system_clock(ts)
			ret = nf90_put_var(ncid_out, varid_out_moistm, moistm(its:ite,jts:jte,kds:kde,sspcs:espcs), start4D, count4D)
			ret = nf90_put_var(ncid_out, varid_out_moist2m, moist2m(its:ite,jts:jte,kds:kde,sspcs:espcs), start4D, count4D)
			ret = nf90_put_var(ncid_out, varid_out_moistum, moistum(its:ite,jts:jte,kds:kde,sspcs:espcs), start4D, count4D)
			ret = nf90_put_var(ncid_out, varid_out_moistvm, moistvm(its:ite,jts:jte,kds:kde,sspcs:espcs), start4D, count4D)
			ret = nf90_put_var(ncid_out, varid_out_moistwm, moistwm(its:ite,jts:jte,kds:kde,sspcs:espcs), start4D, count4D)
			ret = nf90_put_var(ncid_out, varid_out_moist_flux1m, moist_flux1m(its:ite,jts:jte,kds:kde,sspcs:espcs), start4D, count4D)
			ret = nf90_put_var(ncid_out, varid_out_moist_flux2m, moist_flux2m(its:ite,jts:jte,kds:kde,sspcs:espcs), start4D, count4D)
			ret = nf90_put_var(ncid_out, varid_out_moist_flux3m, moist_flux3m(its:ite,jts:jte,kds:kde,sspcs:espcs), start4D, count4D)
			ret = nf90_put_var(ncid_out, varid_out_moistxm, moistxm(its:ite,jts:jte,kds:kde,sspcs:espcs), start4D, count4D)
			ret = nf90_put_var(ncid_out, varid_out_moistym, moistym(its:ite,jts:jte,kds:kde,sspcs:espcs), start4D, count4D)
			ret = nf90_put_var(ncid_out, varid_out_moistzm, moistzm(its:ite,jts:jte,kds:kde,sspcs:espcs), start4D, count4D)
			ret = nf90_sync(ncid_out)
		call system_clock(te, t_rate, t_max)
		time_write = time_write + (te-ts)
		
		if( opt_bin ) then
			ret = nf90_var_par_access(ncid_out, varid_out_binvarm, NF90_INDEPENDENT)
			ret = nf90_var_par_access(ncid_out, varid_out_binvar2m, NF90_INDEPENDENT)
			ret = nf90_var_par_access(ncid_out, varid_out_binvarum, NF90_INDEPENDENT)
			ret = nf90_var_par_access(ncid_out, varid_out_binvarvm, NF90_INDEPENDENT)
			ret = nf90_var_par_access(ncid_out, varid_out_binvarwm, NF90_INDEPENDENT)
			ret = nf90_var_par_access(ncid_out, varid_out_binvar_flux1m, NF90_INDEPENDENT)
			ret = nf90_var_par_access(ncid_out, varid_out_binvar_flux2m, NF90_INDEPENDENT)
			ret = nf90_var_par_access(ncid_out, varid_out_binvar_flux3m, NF90_INDEPENDENT)
			ret = nf90_var_par_access(ncid_out, varid_out_binvarxm, NF90_INDEPENDENT)
			ret = nf90_var_par_access(ncid_out, varid_out_binvarym, NF90_INDEPENDENT)
			ret = nf90_var_par_access(ncid_out, varid_out_binvarzm, NF90_INDEPENDENT)
			
			start4D = (/ its, jts, kds, sbin /)
			count4D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1, (ebin-sbin)+1 /)	
			
			call system_clock(ts)
				ret = nf90_put_var(ncid_out, varid_out_binvarm, binvarm(its:ite,jts:jte,kds:kde,sbin:ebin), start4D, count4D)
				ret = nf90_put_var(ncid_out, varid_out_binvar2m, binvar2m(its:ite,jts:jte,kds:kde,sbin:ebin), start4D, count4D)
				ret = nf90_put_var(ncid_out, varid_out_binvarum, binvarum(its:ite,jts:jte,kds:kde,sbin:ebin), start4D, count4D)
				ret = nf90_put_var(ncid_out, varid_out_binvarvm, binvarvm(its:ite,jts:jte,kds:kde,sbin:ebin), start4D, count4D)
				ret = nf90_put_var(ncid_out, varid_out_binvarwm, binvarwm(its:ite,jts:jte,kds:kde,sbin:ebin), start4D, count4D)
				ret = nf90_put_var(ncid_out, varid_out_binvar_flux1m, binvar_flux1m(its:ite,jts:jte,kds:kde,sbin:ebin), start4D, count4D)
				ret = nf90_put_var(ncid_out, varid_out_binvar_flux2m, binvar_flux2m(its:ite,jts:jte,kds:kde,sbin:ebin), start4D, count4D)
				ret = nf90_put_var(ncid_out, varid_out_binvar_flux3m, binvar_flux3m(its:ite,jts:jte,kds:kde,sbin:ebin), start4D, count4D)
				ret = nf90_put_var(ncid_out, varid_out_binvarxm, binvarxm(its:ite,jts:jte,kds:kde,sbin:ebin), start4D, count4D)
				ret = nf90_put_var(ncid_out, varid_out_binvarym, binvarym(its:ite,jts:jte,kds:kde,sbin:ebin), start4D, count4D)
				ret = nf90_put_var(ncid_out, varid_out_binvarzm, binvarzm(its:ite,jts:jte,kds:kde,sbin:ebin), start4D, count4D)
				ret = nf90_sync(ncid_out)
			call system_clock(te, t_rate, t_max)
			time_write = time_write + (te-ts)
		end if
		
		! Close the file. This frees up any internal netCDF resources, and flushes any buffers.
		ret = nf90_close(ncid_out)
		
		datasize = 4_8 * ( 2_8 * ( int8((ite-its)+1)*int8((jte-jts)+1) ) + &
				 39_8 * ( int8((ite-its)+1)*int8((jte-jts)+1)*int8((kde-kds)+1) ) + &
				 11_8 * ( int8((ite-its)+1)*int8((jte-jts)+1)*int8((kde-kds)+1)*int8((espcs-sspcs)+1) ) )

		if( opt_bin ) then
		datasize = datasize + (4_8 * 11_8 * ( int8((ite-its)+1)*int8((jte-jts)+1)*int8((kde-kds)+1)*int8((ebin-sbin)+1) ) )
		end if

		time_write = time_write/real(t_rate)
		
		return
	end subroutine OutputAverageNetCDF
	
	subroutine OutputRestartNetCDF (	&
			datasize, time_write		&
		)
		use module_param_namelist
		use module_var_output
		use module_mpi
		use netcdf
		
		implicit none 
		
		integer(8), intent(inout) :: datasize
		real(4), intent(inout) :: time_write
		
		! Parallel NetCDF variables
		character (len = 30) :: FILE_NAME
		integer :: ncid_rst, ret, dimid1D(1), dimid2D(2), dimid3D(3), dimid4D(4)
		integer :: dimid_rst_i, dimid_rst_j, dimid_rst_k, dimid_rst_bin, dimid_rst_spcs
		integer :: dimid_rst_im1d, dimid_rst_kg, dimid_rst_l
		integer :: dimid_rst_nprocs
		integer :: varid_rst_u, varid_rst_v, varid_rst_w, varid_rst_t, varid_rst_p, varid_rst_tke
		integer :: varid_rst_tsfc, varid_rst_tg1D, varid_rst_rainncv, varid_rst_rainncv_int
		integer :: varid_rst_smois
		integer :: varid_rst_binvar, varid_rst_moist
		integer :: start2D(2), count2D(2), start3D(3), count3D(3), start4D(4), count4D(4)
		integer :: chunksize1D(1), chunksize2D(2), chunksize3D(3), chunksize4D(4)
		
		! Elapsed Time variables
		integer :: ts, te, t_rate, t_max
		
		! Create NetCDF File - RST
		FILE_NAME = "./output/lesrst_restart.nc"
		ret = nf90_create_par(FILE_NAME, IOR(NF90_NETCDF4, NF90_MPIIO), MPI_COMM_WORLD, MPI_INFO_NULL, ncid_rst)
		
		! -- ATM --
		! Dimension - atm - i, j, k
		ret = nf90_def_dim(ncid_rst, "i", (ide-ids)+1, dimid_rst_i)
		ret = nf90_def_dim(ncid_rst, "j", (jde-jds)+1, dimid_rst_j)
		ret = nf90_def_dim(ncid_rst, "k", (kde-kds)+1, dimid_rst_k)
		dimid3D = (/ dimid_rst_i, dimid_rst_j, dimid_rst_k /)

		! Chunksize
		chunksize3D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1 /)	
		
		! Variable - u v w t p tke
		ret = nf90_def_var(ncid_rst, "u", NF90_DOUBLE, dimid3D, varid_rst_u, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_rst, "v", NF90_DOUBLE, dimid3D, varid_rst_v, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_rst, "w", NF90_DOUBLE, dimid3D, varid_rst_w, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_rst, "t", NF90_DOUBLE, dimid3D, varid_rst_t, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_rst, "p", NF90_DOUBLE, dimid3D, varid_rst_p, chunksizes=chunksize3D)
		ret = nf90_def_var(ncid_rst, "tke", NF90_DOUBLE, dimid3D, varid_rst_tke, chunksizes=chunksize3D)
		
		! -- SFC --
		! Dimension - sfc - im1d, kg, nprocs
		ret = nf90_def_dim(ncid_rst, "im1d", im1d, dimid_rst_im1d)
		ret = nf90_def_dim(ncid_rst, "kg", nzg, dimid_rst_kg)
		ret = nf90_def_dim(ncid_rst, "nprocs", nprocs, dimid_rst_nprocs)
		
		dimid2D = (/ dimid_rst_im1d, dimid_rst_nprocs /)
		chunksize2D = (/ im1d, 1 /)
		ret = nf90_def_var(ncid_rst, "tsfc", NF90_DOUBLE, dimid2D, varid_rst_tsfc, chunksizes=chunksize2D)
		
		dimid3D = (/ dimid_rst_im1d, dimid_rst_kg, dimid_rst_nprocs /)
		chunksize3D = (/ im1d, nzg, 1 /)
		ret = nf90_def_var(ncid_rst, "tg1D", NF90_DOUBLE, dimid3D, varid_rst_tg1D, chunksizes=chunksize3D)
		
		dimid2D = (/ dimid_rst_i, dimid_rst_j /)
		chunksize2D = (/ (ite-its)+1, (jte-jts)+1 /)
		ret = nf90_def_var(ncid_rst, "rainncv", NF90_DOUBLE, dimid2D, varid_rst_rainncv, chunksizes=chunksize2D)
		ret = nf90_def_var(ncid_rst, "rainncv_int", NF90_DOUBLE, dimid2D, varid_rst_rainncv_int, chunksizes=chunksize2D)
		
		! -- SOIL --
		! Dimension - soil - l
		if( soil_model .eq. 1 ) then
			ret = nf90_def_dim(ncid_rst, "l", num_soil, dimid_rst_l)
			dimid3D = (/ dimid_rst_i, dimid_rst_j, dimid_rst_l /)
			chunksize3D = (/ (ite-its)+1, (jte-jts)+1, num_soil /)
			ret = nf90_def_var(ncid_rst, "smois", NF90_DOUBLE, dimid3D, varid_rst_smois, chunksizes=chunksize3D)
		end if
		
		! -- BIN --
		! Dimension - bin - spcs, bin
		ret = nf90_def_dim(ncid_rst, "spcs", (espcs-sspcs)+1, dimid_rst_spcs)
		dimid4D = (/ dimid_rst_i, dimid_rst_j, dimid_rst_k, dimid_rst_spcs /)
		chunksize4D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1, (espcs-sspcs)+1 /)	
		ret = nf90_def_var(ncid_rst, "moist", NF90_DOUBLE, dimid4D, varid_rst_moist, chunksizes=chunksize4D)
		
		if( opt_bin ) then
			ret = nf90_def_dim(ncid_rst, "bin", (ebin-sbin)+1, dimid_rst_bin)
			dimid4D = (/ dimid_rst_i, dimid_rst_j, dimid_rst_k, dimid_rst_bin /)
			chunksize4D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1, (ebin-sbin)+1 /)	
			ret = nf90_def_var(ncid_rst, "binvar", NF90_DOUBLE, dimid4D, varid_rst_binvar, chunksizes=chunksize4D)
		end if
		
		! End of Define
		ret = nf90_enddef(ncid_rst)
		
		!----------------------------------------------------------------------------
		
		! -- ATM --
		ret = nf90_var_par_access(ncid_rst, varid_rst_u, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_rst, varid_rst_v, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_rst, varid_rst_w, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_rst, varid_rst_t, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_rst, varid_rst_p, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_rst, varid_rst_tke, NF90_INDEPENDENT)
		
		start3D = (/ its, jts, kds /)
		count3D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1 /)	
		
		call system_clock(ts)
			ret = nf90_put_var(ncid_rst, varid_rst_u, u(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_rst, varid_rst_v, v(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_rst, varid_rst_w, w(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_rst, varid_rst_t, t(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_rst, varid_rst_p, p(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_put_var(ncid_rst, varid_rst_tke, tke(its:ite,jts:jte,kds:kde), start3D, count3D)
			ret = nf90_sync(ncid_rst)
		call system_clock(te, t_rate, t_max)
		time_write = time_write + (te-ts)
		
		! -- SFC --
		ret = nf90_var_par_access(ncid_rst, varid_rst_tsfc, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_rst, varid_rst_tg1D, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_rst, varid_rst_rainncv, NF90_INDEPENDENT)
		ret = nf90_var_par_access(ncid_rst, varid_rst_rainncv_int, NF90_INDEPENDENT)
		
		start2D = (/ 1, myrank+1 /)
		count2D = (/ im1d, 1 /)
		
		call system_clock(ts)
			ret = nf90_put_var(ncid_rst, varid_rst_tsfc, tsfc(1:im1d), start2D, count2D)
			ret = nf90_sync(ncid_rst)
		call system_clock(te, t_rate, t_max)
		time_write = time_write + (te-ts)
		
		start3D = (/ 1, 1, myrank+1 /)
		count3D = (/ im1d, nzg, 1 /)
		
		call system_clock(ts)
			ret = nf90_put_var(ncid_rst, varid_rst_tg1D, tg1D(1:im1d,1:nzg), start3D, count3D)
			ret = nf90_sync(ncid_rst)
		call system_clock(te, t_rate, t_max)
		time_write = time_write + (te-ts)
		
		start2D = (/ its, jts /)
		count2D = (/ (ite-its)+1, (jte-jts)+1 /)
		
		call system_clock(ts)
			ret = nf90_put_var(ncid_rst, varid_rst_rainncv, rainncv(its:ite,jts:jte), start2D, count2D)
			ret = nf90_put_var(ncid_rst, varid_rst_rainncv_int, rainncv_int(its:ite,jts:jte), start2D, count2D)
			ret = nf90_sync(ncid_rst)
		call system_clock(te, t_rate, t_max)
		time_write = time_write + (te-ts)
		
		! -- SOIL --
		if( soil_model .eq. 1 ) then
			ret = nf90_var_par_access(ncid_rst, varid_rst_smois, NF90_INDEPENDENT)
			
			start3D = (/ its, jts, 1 /)
			count3D = (/ (ite-its)+1, (jte-jts)+1, num_soil /)
			
			call system_clock(ts)
				ret = nf90_put_var(ncid_rst, varid_rst_smois, smois(its:ite,jts:jte,1:num_soil), start3D, count3D)
				ret = nf90_sync(ncid_rst)
			call system_clock(te, t_rate, t_max)
			time_write = time_write + (te-ts)
		end if

		! -- BIN --
		ret = nf90_var_par_access(ncid_rst, varid_rst_moist, NF90_INDEPENDENT)
		
		start4D= (/ its, jts, kds, sspcs /)
		count4D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1, (espcs-sspcs)+1 /)
		
		call system_clock(ts)
			ret = nf90_put_var(ncid_rst, varid_rst_moist, moist(its:ite,jts:jte,kds:kde,sspcs:espcs), start4D, count4D)
			ret = nf90_sync(ncid_rst)
		call system_clock(te, t_rate, t_max)
		time_write = time_write + (te-ts)
		
		if( opt_bin ) then
			ret = nf90_var_par_access(ncid_rst, varid_rst_binvar, NF90_INDEPENDENT)
			
			start4D = (/ its, jts, kds, sbin /)
			count4D = (/ (ite-its)+1, (jte-jts)+1, (kde-kds)+1, (ebin-sbin)+1 /)	
			
			call system_clock(ts)
				ret = nf90_put_var(ncid_rst, varid_rst_binvar, binvar(its:ite,jts:jte,kds:kde,sbin:ebin), start4D, count4D)
				ret = nf90_sync(ncid_rst)
			call system_clock(te, t_rate, t_max)
			time_write = time_write + (te-ts)
		end if	
		
		! Close the file. This frees up any internal netCDF resources, and flushes any buffers.
		ret = nf90_close(ncid_rst)
		
		datasize = 8_8 * ( 2_8 * ( int8((ite-its)+1)*int8((jte-jts)+1) ) + &
				 6_8 * ( int8((ite-its)+1)*int8((jte-jts)+1)*int8((kde-kds)+1) ) + &
				 1_8 * ( int8(im1d) ) + &
				 1_8 * ( int8(im1d)*int8(nzg) ) + &
				 1_8 * ( int8((ite-its)+1)*int8((jte-jts)+1)*int8((kde-kds)+1)*int8((espcs-sspcs)+1) ) )
		if( soil_model .eq. 1 ) then
			datasize = datasize + (8_8 * ( int8((ite-its)+1)*int8((jte-jts)+1)*num_soil ))
		end if
		if( opt_bin ) then
			datasize = datasize + (8_8 * int8((ite-its)+1)*int8((jte-jts)+1)*int8((kde-kds)+1)*int8((ebin-sbin)+1))
		end if

		time_write = time_write/real(t_rate)
		
		return
	call system_clock(te, t_rate, t_max)
		
		return
	end subroutine OutputRestartNetCDF
end module module_benchmark_core
