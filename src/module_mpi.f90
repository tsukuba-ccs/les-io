module module_mpi
	use module_param_namelist

	include 'mpif.h'

	integer :: ierr, nprocs, myrank, myranki, myrankj
	integer :: its, ite, jts, jte
	integer :: ims, ime, jms, jme
	integer :: sbin, ebin, sspcs, espcs
	integer :: im1d, nzg
	logical :: opt_bin

	contains
	
	subroutine CheckProcessor()
		implicit none
		
		if( nprocs /= iprocs*jprocs ) then
			if( myrank == 0 ) print*, "=========== ERROR ============="
			if( myrank == 0 ) print*, "-np ", iprocs*jprocs
			call mpi_finalize( ierr )
			STOP
		end if
			
		return	
	end subroutine CheckProcessor

	subroutine SetRange()
		implicit none

		integer :: irank
		integer :: ista, iend, jsta, jend
		integer :: i, j
		integer, dimension(-1:iprocs,-1:jprocs) :: itable

		itable = MPI_PROC_NULL

		irank = 0
		do i=0, iprocs-1
			do j=0, jprocs-1
				itable(i,j) = irank
				if( myrank == irank ) then
					myranki = i
					myrankj = j
				end if
				irank = irank + 1
			end do
		end do

		call ParaRange1( 1, ide, iprocs, myranki, ista, iend )
		call ParaRange1( 1, jde, jprocs, myrankj, jsta, jend )
		its = ista
		ite = iend
		jts = jsta
		jte = jend
		ims = ista - 1
		ime = iend + 1
		jms = jsta - 1
		jme = jend + 1
		
		return
	end subroutine SetRange
	
	subroutine SelectPredictVar()
		implicit none
		
		microphysics_scheme : select case ( mp_model )
			case( 0 )
				sspcs = 1
				espcs = 1
				sbin  = 0
				ebin  = 0
				opt_bin = .false.
			case( 1 )
				sspcs = 1
				espcs = 3
				sbin  = 0
				ebin  = 0
				opt_bin = .false.
			case( 9 )
				sspcs = 1
				espcs = 3
				sbin  = 1
				ebin  = 12 
				opt_bin = .true.
		end select microphysics_scheme
	end subroutine SelectPredictVar
	
	subroutine SetBuilding()
		implicit none

		im1d = (ite-its+1)*(jte-jts+1)  !! I/O bench
		nzg  = 20
	end subroutine

	subroutine ParaRange1( n1, n2, nprocs, irank, ista, iend )
        ! Y.Aoyama, RIKEN, 4-38
		implicit none

		integer  :: n1, n2, nprocs, irank, ista, iend
		integer  :: iwork1, iwork2

		iwork1 = ( n2 - n1 + 1 ) / nprocs
		iwork2 = mod(n2-n1+1,nprocs)
		ista = irank*iwork1 + n1 + min(irank,iwork2)
		iend = ista + iwork1 - 1
		if( iwork2 > irank ) iend = iend + 1

		return
	end subroutine ParaRange1
end module module_mpi
