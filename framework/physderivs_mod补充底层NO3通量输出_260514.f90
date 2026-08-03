!----------------------------------------------------------------------------
!      CVS:$Id: physderivs_mod.F90,v 1.16 2005/04/28 17:34:20 duse Exp $
!      CVS:$Name:  $
!----------------------------------------------------------------------------
module physderivs_mod
  use const, only : c0
  
  ! 假设 bottom_flux_un 在 common_mod 或其他模块中定义，若无请自行定义单元号
  use common_mod, only : iotempflag, iotemp_un
  
  implicit none

  ! weight on horizontal advection
  double precision, parameter :: hadv_wt = 1.0

  ! smalln parameter for flux limited advection/sinking scheme
  double precision , parameter               :: smalln=2.23d-16

contains

  subroutine physderivs(istep,bio_prev,dydt,bioparams)
    use const, only : c0
    use eco_params, only : numstatevar
    use grid, only : nz,dzt,ntsout,zmid
    use forcing, only : rkz, wvel
    
    implicit none
    
    !-----------------------------------------------------------------------
    ! Arguments
    !-----------------------------------------------------------------------
    double precision, dimension(:,:) :: bio_prev
    double precision, dimension(:,:), intent(out) :: dydt
    double precision, dimension(:), intent(in) :: bioparams

    ! istep                current time step
    integer :: istep, ii, eu
    double precision, dimension(nz+1,numstatevar) :: dydt_tmp
    double precision, dimension(numstatevar) :: dydt_avg
    
    ! 用于验证的底部通量变量
    double precision, dimension(numstatevar) :: bottom_diff_flux
    double precision, dimension(numstatevar) :: bottom_adv_flux
    integer, parameter :: bottom_flux_un = 19 ! 假设底部通量输出单元号
    
    ! 初始化
    dydt = c0
    bottom_diff_flux = c0
    bottom_adv_flux = c0

    ! 1. 计算垂直扩散，然后强制清零导数
    dydt_tmp = c0
    call vertmix(bio_prev,dydt_tmp,istep)
    
    ! 计算底部扩散通量 (用于验证)
    ! 通量 = Kz * (C_nz - C_nz+1) / dz_interface
    do ii = 1, numstatevar
       bottom_diff_flux(ii) = rkz(nz+1,istep) * &
            (bio_prev(nz,ii) - bio_prev(nz+1,ii)) / (zmid(nz+1) - zmid(nz))
    end do
    
    dydt_tmp = c0 ! <--- 屏蔽扩散
    dydt = dydt + dydt_tmp

    ! 2. 计算水平平流，然后强制清零导数
    dydt_tmp = c0
    call horizadv(bio_prev,dydt_tmp,istep)
    dydt_tmp = c0 ! <--- 屏蔽水平平流
    dydt = dydt + dydt_tmp

    ! 3. 计算垂直平流，然后强制清零导数
    dydt_tmp = c0
    call vertadv(istep,bio_prev,dydt_tmp)
    
    ! 计算底部垂直平流通量 (用于验证)
    ! 通量 = w * C_bottom
    do ii = 1, numstatevar
       bottom_adv_flux(ii) = wvel(nz+1,istep) * bio_prev(nz+1,ii)
    end do
    
    dydt_tmp = c0 ! <--- 屏蔽垂直平流
    dydt = dydt + dydt_tmp

    ! 4. 计算表面通量，然后强制清零导数
    dydt_tmp = c0
    call set_sflux(bio_prev,dydt_tmp,istep)
    dydt_tmp = c0 ! <--- 屏蔽表面强迫
    dydt = dydt + dydt_tmp

    ! 5. 计算沉降项，然后强制清零导数
    dydt_tmp = c0
    call sink(istep,bio_prev,dydt_tmp,bioparams)
    dydt_tmp = c0 ! <--- 修改：现在内部沉降也被清零关闭
    dydt = dydt + dydt_tmp

    !-----------------------------------------------------------------------
    ! 输出部分
    !-----------------------------------------------------------------------
    eu = 18
    if ((mod(istep,ntsout).eq.0).and.(iotempflag)) then
        ! 原有 dydt 平均值输出
        do ii = 1,numstatevar
            dydt_avg(ii) = sum(dydt(1:nz, ii)*dzt(1:nz))/sum(dzt(1:nz))
        end do
        write(iotemp_un, '(50(1x, 1PG14.6))') dydt_avg
        
        do ii = 1,numstatevar
            dydt_avg(ii) = sum(dydt(1:eu, ii)*dzt(1:eu))/sum(dzt(1:eu))
        end do
        write(iotemp_un, '(50(1x, 1PG14.6))') dydt_avg
        
        ! 输出底部通量以便验证 (预期结果应为 0 或接近浮点数精度极小值)
        ! 注意：因为我们在 physderivs 里清空了 dydt_tmp，
        ! 这里的 bottom_diff_flux 反映的是“若不关物理模块，物理模块本应产生的通量”
        ! 如果你已经把驱动场 (rkz, wvel) 改为了 0，这里输出就是 0。
        ! 如果驱动场没改，这里输出本来的值，但 dydt = 0 保证了这些值没进入模型计算。
        write(bottom_flux_un, '(50(1x, 1PG14.6))') bottom_diff_flux
        write(bottom_flux_un, '(50(1x, 1PG14.6))') bottom_adv_flux
    end if

  end subroutine physderivs


  subroutine vertmix(bio_prev,dydt,istep)
    use eco_params, only : NumStateVar
    use grid, only : nz,delt,zmid,dzt
    use const, only : c0,c1,c2,p5
    use forcing, only : rkz
    use numeric_subs, only : tridag
    implicit none
    double precision, dimension(:,:) :: bio_prev,dydt
    integer :: istep
    double precision, dimension(nz) :: tria,trib,tric,rr,bio_new
    integer :: iz,isv

    dydt = c0
    iz=1
    tria(iz) = c0
    tric(iz) = -p5*delt*rkz(iz+1,istep)/(zmid(iz+1)-zmid(iz))/dzt(iz)
    trib(iz) = c1 - tric(iz)
    do iz=2,nz
       tria(iz) = -p5*delt*rkz(iz,istep)/(zmid(iz)-zmid(iz-1))/dzt(iz)
       tric(iz) = -p5*delt*rkz(iz+1,istep)/(zmid(iz+1)-zmid(iz))/dzt(iz)
       trib(iz) = c1-tria(iz)-tric(iz)
    end do
    trib(nz) = c1-tria(nz)

    do isv=1,NumStateVar
       iz=1
       rr(iz) = bio_prev(iz,isv) + &
            (p5*delt/(zmid(iz+1)-zmid(iz))/dzt(iz))*rkz(iz+1,istep) * &
            (bio_prev(iz+1,isv)-bio_prev(iz,isv))
       do iz=2,nz-1
          rr(iz) = bio_prev(iz,isv) + &
               (p5*delt/(zmid(iz)-zmid(iz-1))/dzt(iz))*rkz(iz,istep)* &
               (bio_prev(iz-1,isv)-bio_prev(iz,isv)) + &
               (p5*delt/(zmid(iz+1)-zmid(iz))/dzt(iz))*rkz(iz+1,istep)* &
               (bio_prev(iz+1,isv)-bio_prev(iz,isv))
       end do
       rr(nz) = bio_prev(nz,isv) + &
               (p5*delt/(zmid(nz)-zmid(nz-1))/dzt(nz))*rkz(nz,istep)* &
               (bio_prev(nz-1,isv)-bio_prev(nz,isv)) + &
               (delt/(zmid(nz+1)-zmid(nz))/dzt(nz))*rkz(nz+1,istep)* &
               (bio_prev(nz+1,isv)-bio_prev(nz,isv))

       call tridag(tria,trib,tric,rr,bio_new,nz)
       do iz=1,nz
          dydt(iz,isv)=(bio_new(iz)-bio_prev(iz,isv))/delt
       enddo
    enddo
  end subroutine vertmix


  subroutine horizadv(bio_prev,dydt,istep)
    use common_mod, only : lhoriz_adv,horiz_adv
    use const, only : c0
    use eco_params, only : NumStateVar
    use grid, only : nz
    implicit none
    double precision, dimension(:,:) :: bio_prev,dydt
    integer :: istep
    integer :: iz
    dydt = c0
    do iz=16,nz
       where(lhoriz_adv)
          dydt(iz,:) = -hadv_wt*horiz_adv(iz,:,istep)* &
                bio_prev(iz,:)
       end where
    end do
  end subroutine horizadv


  subroutine set_sflux(bio_prev,dydt,istep)
    use common_mod, only : aeoflux
    use const, only : c0,HourPerDay
    use eco_common, only : aeonsv
    use grid, only : dzt
    implicit none
    double precision, dimension(:,:) :: bio_prev,dydt
    integer :: istep
    dydt = c0
    where (aeonsv .ne. 0)
       dydt(1,:)=aeoflux(:,istep)/(dzt(1)*HourPerDay)
    endwhere
  end subroutine set_sflux


  subroutine sink(istep,bio_prev,dydt,bioparams)
    use const, only : c0,c1,c2,rc6,SecPerDay
    use eco_common, only : wnsvflag
    use eco_params, only : NumStateVar
    use grid, only : nz,rdzv,rdzt,delt
    use eco_params, only : iwnsvo
    implicit none
    double precision, dimension(:), intent(in) :: bioparams
    double precision, dimension(:,:) :: bio_prev,dydt
    integer :: istep
    integer :: k, km2, km1, kp1, isv
    double precision    :: Rjp, Rj, Rjm, wFld, wP, wM, cfl
    double precision    :: psiPRj, psiMRj, d0, d1
    double precision, dimension(nz+1) :: wFlux
    double precision :: wFluxkp1 
    double precision :: psi_work
    double precision, dimension(NumStateVar) :: wnsv

    wnsv = bioparams(iwnsvo) * wnsvflag
    dydt=c0
    do isv=1, numstatevar
       wFlux = c0
       wFluxkp1 = c0
       do k=nz+1,2,-1
          wFld = -wnsv(isv)/SecPerDay
          wP=wFld+abs(wFld); wM=wFld-abs(wFld)
          kp1=min(k+1,nz+1); km1=max(k-1,1); km2=max(k-2,1)
          Rjp=bio_prev(km2,isv)-bio_prev(km1,isv)
          Rj =bio_prev(km1,isv)-bio_prev(k,isv)
          Rjm=bio_prev(k,isv)-bio_prev(kp1,isv)
          cfl=abs(wFld*delt*rdzv(k))
          d0=(c2-cfl)*(c1-cfl)*rc6; d1=(c1-cfl*cfl)*rc6
          psiPRj=d0*Rj+d1*Rjm; psiMRj=d0*Rj+d1*Rjp
          if (Rj > c0) then
             psiPRj=min(Rj, max(c0, min(psiPRj, (c1-cfl)/(smalln+cfl)*Rjm)))
             psiMRj=min(Rj, max(c0, min(psiMRj, (c1-cfl)/(smalln+cfl)*Rjp)))
          else
             psiPRj=max(Rj, min(c0, max(psiPRj, (c1-cfl)/(smalln+cfl)*Rjm)))
             psiMRj=max(Rj, min(c0, max(psiMRj, (c1-cfl)/(smalln+cfl)*Rjp)))
          end if
          wflux(k)=( 0.5*wP*(bio_prev(k,isv)+psiPRj) + 0.5*wM*(bio_prev(km1,isv)-psiMRj) )
          if (k .le. nz) dydt(k,isv) = (wFluxkp1 - wflux(k))*rdzt(k)
          wfluxkp1 = wflux(k)
       end do
       k=1; dydt(k,isv) = (wFluxkp1-c0)*rdzt(k)
    end do
  end subroutine sink


  subroutine vertadv(istep,bio_prev,dydt)
    use const, only : c0,c1,c2,rc6,SecPerDay
    use eco_params, only : NumStateVar
    use forcing, only : wvel
    use grid, only : nz,rdzv,rdzt,delt
    implicit none
    double precision, dimension(:,:) :: bio_prev,dydt
    integer :: istep
    integer :: k, km2, km1, kp1, isv
    double precision    :: Rjp, Rj, Rjm, wFld, wP, wM, cfl
    double precision    :: psiPRj, psiMRj, d0, d1
    double precision, dimension(nz+1) :: wFlux
    double precision :: wFluxkp1 
    double precision :: psi_work

    dydt=c0
    do isv=1, numstatevar
       wFlux = c0; wFluxkp1 = c0
       do k=nz+1,1,-1
          wFld = -wvel(k,istep)
          wP=wFld+abs(wFld); wM=wFld-abs(wFld)
          kp1=min(k+1,nz+1); km1=max(k-1,1); km2=max(k-2,1)
          Rjp=bio_prev(km2,isv)-bio_prev(km1,isv)
          Rj =bio_prev(km1,isv)-bio_prev(k,isv)
          Rjm=bio_prev(k,isv)-bio_prev(kp1,isv)
          cfl=abs(wFld*delt*rdzv(k))
          d0=(c2-cfl)*(c1-cfl)*rc6; d1=(c1-cfl*cfl)*rc6
          psiPRj=d0*Rj+d1*Rjm; psiMRj=d0*Rj+d1*Rjp
          if (Rj > c0) then
             psiPRj=min(Rj, max(c0, min(psiPRj, (c1-cfl)/(smalln+cfl)*Rjm)))
             psiMRj=min(Rj, max(c0, min(psiMRj, (c1-cfl)/(smalln+cfl)*Rjp)))
          else
             psiPRj=max(Rj, min(c0, max(psiPRj, (c1-cfl)/(smalln+cfl)*Rjm)))
             psiMRj=max(Rj, min(c0, max(psiMRj, (c1-cfl)/(smalln+cfl)*Rjp)))
          end if
          wflux(k)=(0.5*wP*(bio_prev(k,isv)+psiPRj) + 0.5*wM*(bio_prev(km1,isv)-psiMRj))
          if (k .le. nz) dydt(k,isv) = (wFluxkp1 - wflux(k) + bio_prev(k,isv)*(wvel(kp1,istep)-wvel(k,istep)))*rdzt(k)
          wfluxkp1 = wflux(k)
       end do
    end do
  end subroutine vertadv

  subroutine physderivs_init(istep,bio_prev)
    use common_mod, only : bbcnsv, flag_bbcnsv
    use const, only : c0,c1,c2,mc1
    use grid, only : nz
    implicit none
    double precision, dimension(:,:) :: bio_prev
    integer :: istep
    where(bbcnsv(:,istep) .lt. mc1)
       bio_prev(nz+1,:)=bio_prev(nz,:)-(bio_prev(nz-1,:)-bio_prev(nz,:))
       where (bio_prev(nz+1,:) < c0) bio_prev(nz+1,:) = c0
    elsewhere
       where(flag_bbcnsv(:) .eq. c1)
               bio_prev(nz+1,:)=bbcnsv(:,istep-1)
       elsewhere(flag_bbcnsv(:) .ge.c2)
               bio_prev(nz+1,:)=bio_prev(nz,:)+bbcnsv(:,istep-1)
       endwhere
       where (bio_prev(nz+1,:) < c0) bio_prev(nz+1,:) = c0
    endwhere
  end subroutine physderivs_init

end module physderivs_mod