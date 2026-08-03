!----------------------------------------------------------------------------
!     CVS:$Id: eco_derivs.F90, added 15N+DDA $
!     增加STn15的数值同化，也就是DET15n
!     CVS:$Name: Yangchun Xu 20260526 
!----------------------------------------------------------------------------

module eco_derivs
  use common_mod, only : nsite,nz_max,nt_max
  use eco_params, only : numstatevar
  implicit none

  private

  save

  double precision, allocatable :: &
       bio0_glob(:,:,:)                 ! initial conditions for all sites

  double precision, allocatable :: bbcnsv_glob(:,:,:)
  integer, allocatable :: flag_bbcnsv_glob(:,:)
  
  public bioderivs
  public bioderivs_init
  public get_costpred
  public read_bio
  public setup_bio


contains

!-------------------------------------------------------------------------
! read_bio:  reads in initial conditions for the ecosystem state variables
! ***Required***
!-------------------------------------------------------------------------
  subroutine read_bio
    use common_mod, only : datdir,dat_prefix_glob,nsite
    use const, only : c0
    use eco_params, only : NumStateVar,iSPc,fname_bio_suffix,&
         iSPn, iTRn, iDDAn, iUNn, iBAn, &
         iSP15n, iTR15n, iDDA15n, iUN15n, &
         iPRTn, iMZn, iLDOMn, iSDOMn, &
         iDETn, iNH4, iNO3, iDET15n
   use grid, only : nz_glob,nt_glob,delt_glob
    implicit none

!-----------------------------------------------------------------------
! Arguments
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Local variables
!-----------------------------------------------------------------------

! depth  work variable for depth
    double precision :: depth 

! counters
    integer:: iz,isv,isite,it,it2

! unit number
    integer, parameter :: iun=30

    allocate (bio0_glob(nz_max+1,numstatevar,nsite))
    allocate (bbcnsv_glob(numstatevar,nt_max,nsite))
    allocate (flag_bbcnsv_glob(numstatevar,nsite))
    
    siteloop: do isite=1,nsite
       svloop: do isv=1,NumStateVar

! read in initial conditions for all state variables
          open(unit=iun, &
               status='old', &
               file=trim(datdir)//'/'//trim(dat_prefix_glob(isite))//'/'// &
               trim(dat_prefix_glob(isite))//'_'// &
               trim(fname_bio_suffix(isv))//'.init')
          do iz=1,nz_glob(isite)
! depth is not used - we assume it is correct and consistent
             read(iun,*)depth,bio0_glob(iz,isv,isite)
          end do
! read in bottom boundary conditions for all state variables
          open(unit=iun, &
               status='old', &
               file=trim(datdir)//'/'//trim(dat_prefix_glob(isite))//'/'// &
               trim(dat_prefix_glob(isite))//'_'// &
               trim(fname_bio_suffix(isv))//'_bbc.dat')
          read(iun,*) flag_bbcnsv_glob(isv,isite)
          it = 1
          read(iun,*)bbcnsv_glob(isv,it,isite)
          do it=2,nt_glob(isite)
             do it2 = 1, delt_glob(isite)/1800 
                 read(iun,*)bbcnsv_glob(isv,it,isite)
             enddo 
          end do
          close(iun)
       end do svloop
    end do siteloop

! conversion for iPhy from chlorophyll to nitrogen
!    bio0_glob(:,iPhy,:)=bio0_glob(:,iPhy,:)/n2chl
!    where (bbcnsv_glob(iPhy,:,:) .gt. c0)
!        bbcnsv_glob(iPhy,:,:)=bbcnsv_glob(iPhy,:,:)/n2chl
!    end Where

  end subroutine read_bio

  subroutine setup_bio(isite)
    use common_mod, only : bbcnsv,flag_bbcnsv,bio0,horiz_adv,lhoriz_adv
    use common_mod, only : datdir,dat_prefix_glob,nt_max, nz_max
    use eco_params, only : NumStateVar,fname_bio_suffix
    use grid, only : nz_glob,nt_glob,delt
    implicit none
    
    character(len=256) :: fname
    
     integer:: iz,isv,it,itemp
    
    integer, parameter :: iun=30

!-----------------------------------------------------------------------
! Arguments
!-----------------------------------------------------------------------

    integer, intent(in) :: &
         isite   ! location index

!double precision, dimension(:,:), intent(out) :: &
!     bio0    ! initial conditions 

    bio0 = bio0_glob(:,:,isite)

    bbcnsv = bbcnsv_glob(:,:,isite)
    flag_bbcnsv = flag_bbcnsv_glob(:,isite)
    
    ! read in horizontal advection forcing file, if it exists
    ! if doesn't exist, set horizontal advection flag to false
    ! files are expected to be named as:
    ! datdir/dat_prefix/dat_prefix_fname_bio_suffix_ha.dat
    svloop: do isv=1,NumStateVar      
         
          fname = trim(datdir)//'/'//trim(dat_prefix_glob(isite))//'/'// &
               trim(dat_prefix_glob(isite))//'_'// &
               trim(fname_bio_suffix(isv))//'_ha.dat'
          inquire(file=fname,exist=lhoriz_adv(isv))
          if (lhoriz_adv(isv)) then
	     IF (.NOT. ALLOCATED(horiz_adv)) THEN
	         ALLOCATE(horiz_adv(nz_max,numstatevar,nt_max))
             END IF
             open(unit=iun,file=fname,status='old')
             do it=1,nt_glob(isite)
	     do itemp = 1,nint(delt/1800)
                read(iun,*)(horiz_adv(iz,isv,it),iz=1,nz_glob(isite))
             enddo
	     enddo
             close(iun)
          end if
    
    end do svloop

  end subroutine setup_bio

!-------------------------------------------------------------------------
! ***Required*** - bioderivs: calculates ecosystem component 
! derivatives for given concentrations of state variables at the 
! indicated timestep
!-------------------------------------------------------------------------
  subroutine bioderivs(istep,par,par_ifc,bio_prev,dydt_bio,dydt_diag,bioparams)
    use eco_common, only : rlt
    use derivs_mod, only : derivs
    use grid, only : nz

    implicit none

!-----------------------------------------------------------------------
! Arguments
!-----------------------------------------------------------------------

! istep   current time step
    integer istep

! bio_prev(nz,nsv)    state variable profiles (various units)
! dydt_bio            time derivatives for bio_prev
! dydt_diag           time derivatives for diagnostics    
    double precision, dimension(:,:), intent(out) :: dydt_bio,dydt_diag
    double precision, dimension(:,:), intent(in) :: bio_prev

! bioparams           ecosystem model parameters
! par                 Photosynthetically active radiation
    double precision, dimension(:), intent(in) :: bioparams,par,par_ifc

!-----------------------------------------------------------------------
! Local variables
!-----------------------------------------------------------------------

! counters
    integer :: iz

    do iz=1,nz
       rlt = par(iz)
       call derivs(bio_prev(iz,:),dydt_bio(iz,:),dydt_diag(iz,:),istep, &
            iz,bioparams)
    enddo

  end subroutine bioderivs

!-------------------------------------------------------------------------
! ***Required*** 
! get_costpred:  returns an array of predictions needed to compare with
! observations to compute the cost function.  This routine converts from
! ecosystem state variables and diagnostics to cost function predictions.
!
! The output variable, cpred, is defined as a 3D array of
! dimension(nz+1,nt_max,ncnsv).  See common_mod for
! required indices for the ncnsv dimension.  The first two dimensions
! match those of the ecosystem state variables.
!-------------------------------------------------------------------------

subroutine get_costpred(cpred,csed_pred,bio,diag,bioparams)
use cost, only : ncnsv,xcppr,xcphy,xcchl,xczoo,xcnit,xcpo4,xcbac,xcbpr, &
xcdoc,xcdon,xcdop,xcpoc,xcpon,xcpon15,xcpop,xcnfr,xcstc,xcstn,xcstn15,xcstp !Luo_MB

    use cost, only : ndat_sed,z_csed,btime_csed,etime_csed,ndat_max
    use cost, only: ndat, time_cdat, z_cdat, a_cdat
    
use const, only : c0,c1,c2,SecPerHour,secperday
use eco_common, only : wnsvflag
use eco_params, only : NumStateVar,iPP,iDETc,mwC,mwN,mwP,&
                    iremin,iremin_prf_n,iremin_prf_p,iepsilon_remin,&
                    iMZc,iNO3,iNH4
use eco_params, only: iPO4,iBAc,iprBAc,iSDOMc,iSDOMn,iSDOMp,&
                    iSPc,iTRc,iDDAc,iUNc,iPRTc,iDETc, &
	            iSPn,iSPp,iTRn,iTRp,iDDAn,iDDAp, &
                    iUNn,iUNp,iPRTn,iPRTp,iDETn,&
                    iSP15n, iTR15n, iDDA15n, iUN15n, &
                    iPRT15n, iDET15n, &
                    igrowTRnf,igrowDDAnf,igrowUNnf,&
	           iDETp, iSPchl, iTRchl,iDDAchl,iUNchl !Luo_MB
use forcing, only : cdays
use grid, only : zifc,nz,nt,delt,ntsout
use eco_params, only: iwnsvo
    
!----------------------------------------------------------------------  
!     Arguments
!----------------------------------------------------------------------

! cpred          predicted values of cost variables
! csed_pred      predicted values of sediment trap fluxes
! bio            ecosystem scalars
! diag           ecosystem diagnostics
! bioparams      ecosystem parameters

    double precision, dimension(:,:), intent(out) :: cpred
    double precision, dimension(:,:), intent(out) :: csed_pred
    double precision, dimension(:,:,:), intent(in) :: bio, diag
    double precision, dimension(:), intent(in) :: bioparams

!----------------------------------------------------------------------
!     Local variables
!----------------------------------------------------------------------

! nsed_pred      work variable for use with sediment trap fluxes
    integer, dimension(ndat_max) :: nsed_pred

! z2trap         depth between lowest level and trap depth
! t2trap         time to reach the trap
    double precision :: z2trap,t2trap

! counters
    integer :: isv,it,ised,ised_loc,idat

! TAMC - needed for summations
    integer :: it2, it_temp

    integer :: &
         ntsperday   , & ! number of timesteps per day
         ntsperhalfday      ! number of timesteps per half day
    double precision, dimension(NumStateVar) :: wnsv
    
    ! ==========================================
    ! 🔍 在这里添加调试输出（初始化之前）
    ! ==========================================
    write(*,*) '========================================'
    write(*,*) 'DEBUG INFO in get_costpred:'
    write(*,*) 'nt = ', nt
    write(*,*) 'nz = ', nz
    write(*,*) 'delt = ', delt
    write(*,*) 'ntsout = ', ntsout
    write(*,*) 'size(bio,1) = ', size(bio,1)   ! 应该是 25 (深度层)
    write(*,*) 'size(bio,2) = ', size(bio,2)   ! 状态变量数
    write(*,*) 'size(bio,3) = ', size(bio,3)   ! 时间维（关键！）
    write(*,*) 'size(diag,1) = ', size(diag,1)
    write(*,*) 'size(diag,2) = ', size(diag,2)
    write(*,*) 'size(diag,3) = ', size(diag,3)
    write(*,*) 'NumStateVar = ', NumStateVar
    write(*,*) 'SecPerHour = ', SecPerHour
    write(*,*) 'secperday = ', secperday
    write(*,*) 'ntsperday (calculated) = ', nint(secperday/delt)
    write(*,*) '========================================'

    
    cpred = c0
    csed_pred = c0
    
     wnsv = bioparams(iwnsvo) * wnsvflag

! units on zoo in cost function are mmolC/m3
    do idat = 1, ndat(xczoo)
             cpred(xczoo, idat)  = &
             bio(z_cdat(xczoo,idat), iMZc, time_cdat(xczoo,idat))
    enddo 
    do idat = 1, ndat(xcnit)
             cpred(xcnit, idat)  = &
             bio(z_cdat(xcnit,idat), iNO3, time_cdat(xcnit,idat))
    enddo 
    

! WARNING: assumptions about delt are hardcoded into the following
! code.

! compute productivity - note that observed productivity uses a daily
! basis, so the predicted productivity should use the same.  If model
! estimates are not daily values, they need to be integrated accordingly.

! TAMC - can't seem to handle sum() functions properly.

! want to sum productivity over 24 hour periods
    ! compute number of points in a 24 hour period
    ntsperday = nint(secperday/delt)
    ntsperhalfday = nint(secperday/delt/c2)

! Daily mean volumetric total N2-fixation rate at observation depth.
! Units: mmol N m-3 d-1. No multiplication by layer thickness.

   do idat = 1,ndat(xcnfr)

      do it = time_cdat(xcnfr,idat)-ntsperhalfday, &
            time_cdat(xcnfr,idat)-ntsperhalfday+ntsperday-1

        it_temp = it

        cpred(xcnfr,idat) = cpred(xcnfr,idat) &
             + diag(z_cdat(xcnfr,idat),igrowTRnf,it_temp) &
             + diag(z_cdat(xcnfr,idat),igrowDDAnf,it_temp) &
             + diag(z_cdat(xcnfr,idat),igrowUNnf,it_temp)

      enddo

    enddo

     cpred(xcnfr,:) = cpred(xcnfr,:) * secperday / ntsperday

    do idat = 1,ndat(xcppr)
        do it = time_cdat(xcppr,idat)-ntsperhalfday, &
                   time_cdat(xcppr,idat)-ntsperhalfday+ntsperday-1
                   it_temp = it
                   cpred(xcppr, idat) =  cpred(xcppr, idat) &
                   + diag(z_cdat(xcppr, idat),iPP,it_temp)
        enddo 
    enddo
    cpred(xcppr,:) = cpred(xcppr,:) * secperday / ntsperday 
    
    do idat = 1,ndat(xcbpr)
        do it = time_cdat(xcbpr,idat)-ntsperhalfday, &
                    time_cdat(xcbpr,idat)-ntsperhalfday+ntsperday-1
                   it_temp = it
                   cpred(xcbpr, idat) =  cpred(xcbpr, idat) &
                       + diag(z_cdat(xcbpr, idat),iprBAc,it_temp)
        enddo 
    enddo
    cpred(xcbpr,:) = cpred(xcbpr,:) * secperday / ntsperday 

    do idat = 1, ndat(xcphy)
             cpred(xcphy, idat)  = &
                   bio(z_cdat(xcphy,idat), iSPn, time_cdat(xcphy,idat)) &
                   + bio(z_cdat(xcphy,idat), iUNn, time_cdat(xcphy,idat))
    enddo
    
    do idat = 1, ndat(xcchl)
             cpred(xcchl, idat)  = &
                   bio(z_cdat(xcchl,idat), iSPchl, time_cdat(xcchl,idat)) &
                   + bio(z_cdat(xcchl,idat), iTRchl, time_cdat(xcchl,idat)) &
                   + bio(z_cdat(xcchl,idat), iDDAchl, time_cdat(xcchl,idat)) &  
                   + bio(z_cdat(xcchl,idat), iUNchl, time_cdat(xcchl,idat))
enddo  
    
do idat = 1, ndat(xcpo4)
             cpred(xcpo4, idat)  = &
                  bio(z_cdat(xcpo4,idat), iPO4, time_cdat(xcpo4,idat))
enddo 
    
do idat = 1, ndat(xcbac)
             cpred(xcbac, idat)  = &
                  bio(z_cdat(xcbac,idat), iBAc, time_cdat(xcbac,idat))
enddo 

do idat = 1, ndat(xcdoc)
             cpred(xcdoc, idat)  = &
                  bio(z_cdat(xcdoc,idat), iSDOMc, time_cdat(xcdoc,idat))
enddo 
    
do idat = 1, ndat(xcdon)
             cpred(xcdon, idat)  = &
                  bio(z_cdat(xcdon,idat), iSDOMn, time_cdat(xcdon,idat))
enddo 

do idat = 1, ndat(xcdop)
             cpred(xcdop, idat)  = &
                  bio(z_cdat(xcdop,idat), iSDOMp, time_cdat(xcdop,idat))
enddo     
    
do idat = 1, ndat(xcpoc)
             cpred(xcpoc, idat)  = &
                  bio(z_cdat(xcpoc,idat), iSPc, time_cdat(xcpoc,idat)) &
                  +bio(z_cdat(xcpoc,idat), iTRc, time_cdat(xcpoc,idat)) &
                  +bio(z_cdat(xcpoc,idat), iDDAc, time_cdat(xcpoc,idat)) &
                  +bio(z_cdat(xcpoc,idat), iUNc, time_cdat(xcpoc,idat)) &
                  +bio(z_cdat(xcpoc,idat), iPRTc, time_cdat(xcpoc,idat)) &
                  +bio(z_cdat(xcpoc,idat), iDETc, time_cdat(xcpoc,idat)) 
    enddo 
    
    do idat = 1, ndat(xcpon)
             cpred(xcpon, idat)  = &
                  bio(z_cdat(xcpon,idat), iSPn, time_cdat(xcpon,idat)) &
                  +bio(z_cdat(xcpon,idat), iTRn, time_cdat(xcpon,idat)) &
                  +bio(z_cdat(xcpon,idat), iDDAn, time_cdat(xcpon,idat)) &
                  +bio(z_cdat(xcpon,idat), iUNn, time_cdat(xcpon,idat)) &
                  +bio(z_cdat(xcpon,idat), iPRTn, time_cdat(xcpon,idat)) &
                  +bio(z_cdat(xcpon,idat), iDETn, time_cdat(xcpon,idat)) 
enddo 
    
   do idat = 1, ndat(xcpon15)
             cpred(xcpon15, idat)  = &
                  bio(z_cdat(xcpon15,idat), iSP15n, time_cdat(xcpon15,idat)) &
                  +bio(z_cdat(xcpon15,idat), iTR15n, time_cdat(xcpon15,idat)) &
                  +bio(z_cdat(xcpon15,idat), iDDA15n, time_cdat(xcpon15,idat)) &
                  +bio(z_cdat(xcpon15,idat), iUN15n, time_cdat(xcpon15,idat)) &
                  +bio(z_cdat(xcpon15,idat), iPRT15n, time_cdat(xcpon15,idat)) &
                  +bio(z_cdat(xcpon15,idat), iDET15n, time_cdat(xcpon15,idat)) 
enddo 

do idat = 1, ndat(xcpop)
             cpred(xcpop, idat)  = &
                  bio(z_cdat(xcpop,idat), iSPp, time_cdat(xcpop,idat)) &
                  +bio(z_cdat(xcpop,idat), iTRp, time_cdat(xcpop,idat)) &
                  +bio(z_cdat(xcpop,idat), iDDAp, time_cdat(xcpop,idat)) &
                  +bio(z_cdat(xcpop,idat), iUNp, time_cdat(xcpop,idat)) &
                  +bio(z_cdat(xcpop,idat), iPRTp, time_cdat(xcpop,idat)) &
                  +bio(z_cdat(xcpop,idat), iDETp, time_cdat(xcpop,idat))
    enddo 
!------------------------------------------------------------
! DEBUG: print NFR observations and model predictions
!------------------------------------------------------------
write(6,*) '=========================================='
write(6,*) 'NFR UNIT AND VALUE CHECK'
write(6,*) 'ndat(NFR) = ', ndat(xcnfr)
write(6,*) 'ntsperday = ', ntsperday
write(6,*) 'secperday = ', secperday

if (ndat(xcnfr) > 0) then

   write(6,*) 'NFR obs min/max = ', &
        minval(a_cdat(xcnfr,1:ndat(xcnfr))), &
        maxval(a_cdat(xcnfr,1:ndat(xcnfr)))

   write(6,*) 'NFR pred min/max = ', &
        minval(cpred(xcnfr,1:ndat(xcnfr))), &
        maxval(cpred(xcnfr,1:ndat(xcnfr)))

   do idat = 1, min(ndat(xcnfr),30)
      write(6,'(A,I6,A,I10,A,I6,A,ES16.7,A,ES16.7,A,ES16.7)') &
           'id=', idat, &
           ' time=', time_cdat(xcnfr,idat), &
           ' z=', z_cdat(xcnfr,idat), &
           ' obs=', a_cdat(xcnfr,idat), &
           ' pred=', cpred(xcnfr,idat), &
           ' diff=', a_cdat(xcnfr,idat)-cpred(xcnfr,idat)
   enddo

endif

write(6,*) '=========================================='

! calculate sediment flux at trap times and depths
   csed_pred = c0
   nsed_pred = 0
   
      do it=1,nt
       it_temp = it
       ! for each export flux - determine contribution at trap
       do ised=1,ndat_sed(xcstc)
          ised_loc = nz+1
          if  (z_csed(xcstc,ised).lt.zifc(nz+1)) then
             do while (z_csed(xcstc,ised).lt.zifc(ised_loc))
                      ised_loc = ised_loc - 1
             end do
          endif 
          z2trap = z_csed(xcstc,ised)-zifc(ised_loc)
          t2trap = cdays(it)+(z2trap)/wnsv(iDETc)
          if ((t2trap .gt. btime_csed(xcstc,ised)) .and. &
               (t2trap .le. etime_csed(xcstc,ised))) then
             csed_pred(xcstc,ised) = csed_pred(xcstc,ised)+wnsv(iDETc)* &
                  mwC*bio(ised_loc-1,iDETc,it_temp)* &
                  exp(-z2trap*bioparams(iremin)/wnsv(iDETc))
             nsed_pred(ised) = nsed_pred(ised) + 1
          endif
       enddo
    enddo
! Note that where nsed_pred is zero, the flux will have a zero value.
!where (nsed_pred(1:ndat_sed) > c0) 
    where (nsed_pred(1:ndat_sed(xcstc)) > 0)
       csed_pred(xcstc,1:ndat_sed(xcstc)) = &
             csed_pred(xcstc,1:ndat_sed(xcstc))/nsed_pred(1:ndat_sed(xcstc))
    end where
    
    nsed_pred = 0
    do it=1,nt
       it_temp = it
       !for each export flux - determine contribution at trap
       do ised=1,ndat_sed(xcstn)
          ised_loc = nz+1
          if  (z_csed(xcstn,ised).lt.zifc(nz+1)) then
             do while (z_csed(xcstn,ised).lt.zifc(ised_loc))
                      ised_loc = ised_loc - 1
             end do
          endif 
          z2trap = z_csed(xcstn,ised)-zifc(ised_loc)
          t2trap = cdays(it)+(z2trap)/wnsv(iDETn)
          if ((t2trap .gt. btime_csed(xcstn,ised)) .and. &
               (t2trap .le. etime_csed(xcstn,ised))) then
             csed_pred(xcstn,ised) = csed_pred(xcstn,ised)+wnsv(iDETn)* &
                  mwN*bio(ised_loc-1,iDETn,it_temp)* &
                  exp(-z2trap*bioparams(iremin)*bioparams(iremin_prf_n)/wnsv(iDETn))
             nsed_pred(ised) = nsed_pred(ised) + 1
          endif
       enddo
    enddo
! Note that where nsed_pred is zero, the flux will have a zero value.
!where (nsed_pred(1:ndat_sed) > c0) 
    where (nsed_pred(1:ndat_sed(xcstn)) > 0)
       csed_pred(xcstn,1:ndat_sed(xcstn)) = &
             csed_pred(xcstn,1:ndat_sed(xcstn))/nsed_pred(1:ndat_sed(xcstn))
    end where
!-----------------------------------------------------------------------------------------
!  增加STn15的数值同化
!-----------------------------------------------------------------------------------------
     nsed_pred = 0 
     do it=1,nt
       !if (it .eq. 1) then
        !  write(*,*) 'STn15_TIME_CHECK: cdays(1)=', cdays(1), &
         !      ' cdays(nt)=', cdays(nt)
        !if (ndat_sed(xcstn15) .gt. 0) then
        !write(*,*) 'STn15_OBS_TIME_CHECK: first b/e=', &
        !           btime_csed(xcstn15,1), etime_csed(xcstn15,1)
        !write(*,*) 'STn15_OBS_TIME_CHECK: last b/e=', &
         !          btime_csed(xcstn15,ndat_sed(xcstn15)), &
          !         etime_csed(xcstn15,ndat_sed(xcstn15))
         !endif
        !endif

        it_temp = it 
     !for each export flux - determine contribution at trap 
        do ised=1,ndat_sed(xcstn15)
           ised_loc = nz+1
           if  (z_csed(xcstn15,ised).lt.zifc(nz+1)) then
              do while (z_csed(xcstn15,ised).lt.zifc(ised_loc))
                       ised_loc = ised_loc - 1
             end do
          endif 
          z2trap = z_csed(xcstn15,ised)-zifc(ised_loc)
          t2trap = cdays(it)+(z2trap)/wnsv(iDET15n)
          if ((t2trap .gt. btime_csed(xcstn15,ised)) .and. &
               (t2trap .le. etime_csed(xcstn15,ised))) then
               csed_pred(xcstn15,ised) = csed_pred(xcstn15,ised)+wnsv(iDET15n)* &
               mwN*bio(ised_loc-1,iDET15n,it_temp)* &
               exp(-z2trap*bioparams(iremin)*bioparams(iremin_prf_n)* &
               (1.0d0-bioparams(iepsilon_remin)/1000.0d0)/wnsv(iDET15n))
             nsed_pred(ised) = nsed_pred(ised) + 1
          endif
       enddo
    enddo
! Note that where nsed_pred is zero, the flux will have a zero value.
 where (nsed_pred(1:ndat_sed(xcstn15)) > 0)
           csed_pred(xcstn15,1:ndat_sed(xcstn15)) = &
           csed_pred(xcstn15,1:ndat_sed(xcstn15))/nsed_pred(1:ndat_sed(xcstn15))

    end where
    
    nsed_pred = 0 
    do it=1,nt
       it_temp =it
       !for each export flux - determine contribution at trap
       do ised=1,ndat_sed(xcstp)
          ised_loc = nz+1
          if  (z_csed(xcstp,ised).lt.zifc(nz+1)) then
             do while (z_csed(xcstp,ised).lt.zifc(ised_loc))
                      ised_loc = ised_loc - 1
             end do
          endif 
          z2trap = z_csed(xcstp,ised)-zifc(ised_loc)
          t2trap = cdays(it)+(z2trap)/wnsv(iDETp)
          if ((t2trap .gt. btime_csed(xcstp,ised)) .and. &
               (t2trap .le. etime_csed(xcstp,ised))) then
             csed_pred(xcstp,ised) = csed_pred(xcstp,ised)+wnsv(iDETp)* &
                  mwP*bio(ised_loc-1,iDETp,it_temp)* &
                  exp(-z2trap*bioparams(iremin)*bioparams(iremin_prf_p)/wnsv(iDETp))
!nsed_pred(ised) = nsed_pred(ised) + c1
             nsed_pred(ised) = nsed_pred(ised) + 1
          endif
       enddo
    enddo
! Note that where nsed_pred is zero, the flux will have a zero value.
!where (nsed_pred(1:ndat_sed) > c0) 
    where (nsed_pred(1:ndat_sed(xcstp)) > 0)
       csed_pred(xcstp,1:ndat_sed(xcstp)) = &
             csed_pred(xcstp,1:ndat_sed(xcstp))/nsed_pred(1:ndat_sed(xcstp))
    end where

  end subroutine get_costpred


!-------------------------------------------------------------------------
! ***Required***
! bioderivs_init: perform any necessary initialization required before
! calling bioderivs.
!-------------------------------------------------------------------------
  subroutine bioderivs_init(istep,par,par_ifc,bio_prev,diag_prev)
    use const, only : c0
    use eco_params, only : iSPchl, iTRchl, iDDAchl, iUNchl
    use eco_params, only : NumStateVar,NumDiagVar
    use forcing, only : qi
    use grid, only : nz

    use light, only : calc_light

    implicit none

!-----------------------------------------------------------------------
! Arguments
!-----------------------------------------------------------------------

! istep             current time step
    integer, intent(in) :: istep
! bio_prev(nz,nsv)    state variable profiles
! par   photosynthetically active radiation

    double precision, dimension(:,:), intent(in) :: bio_prev
    double precision, dimension(:), intent(out) :: par,par_ifc
    double precision, dimension(:,:), intent(in) :: diag_prev


!-----------------------------------------------------------------------
! Local variables
!-----------------------------------------------------------------------

    double precision, dimension(nz) :: chl
    
! guard against failure due to negative values
!chl=max(bio_prev(1:nz,iPhy)*n2chl,c0)
    chl=bio_prev(:,iSPchl) + bio_prev(:,iTRchl) + bio_prev(:,iDDAchl) + bio_prev(:,iUNchl)

    call calc_light(qi(istep),chl,par,par_ifc)

  end subroutine bioderivs_init


end module eco_derivs

