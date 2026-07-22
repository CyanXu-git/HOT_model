!----------------------------------------------------------------------------
!     CVS:$Id: eco_common.F90, added DDA+15N $
!     CVS:$Name:  $
!     Yangchun Xu revised, 2026/03/22
!   Modified the epsilon of LDON(1-0)
!----------------------------------------------------------------------------
module eco_common
  use eco_params, only : numstatevar,numdiagvar

  use types, only : ncvar_attrib,ncf_float

  implicit none
  save


! wnsv - flag sinking rate for each state variable (m/day) 
! 1.0 use wnsvo as sinking rate
! 0.0 disable sinking rate
   double precision, dimension(NumStateVar), parameter :: &
        wnsvflag=(/ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
                0.0, 0.0, 0.0, 1.0, 1.0, 1.0, &
                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, &
                ! --- 新增 15N 沉降标记 --- 
                0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, & ! 38-44  (SP to MZ 15n)
                0.0, 0.0, 1.0, 0.0, 0.0 /) ! 45-49 (LDOM, SDOM, DET15n=1.0, NH4, NO3)

! aeonsv - Aeolean flux flag,  0=off or 1=on
  integer, dimension(NumStateVar), parameter :: &
       aeonsv=0                               ! **Required**   (but not used)

! irradiance
  double precision :: rlt


!-----------------------------------------------------------------------
! ecosystem model parameters and namelist
!-----------------------------------------------------------------------
  double precision :: ae,mu_SP,alpha_SP,a_SP,v_SPn,k_nh4SP,k_no3SP,&
      v_SPp,k_po4SP, zeta, theta, r_excrSP_1,r_excrSP_2,r_pomSP,&
      mu_TR,alpha_TR,a_TR,k_nh4TR,v_TRn,k_no3TR,&
      v_TRp, k_po4TR,mu_pickTRpo4, zeta_nf, &
      r_excrTR_1,r_excrTR_n,r_excrTR_2,r_pomTR,&
      mu_DDA,alpha_DDA,a_DDA,k_nh4DDA,v_DDAn,k_no3DDA,&
      v_DDAp, k_po4DDA,mu_pickDDApo4, zeta_nf_DDA, &
      r_excrDDA_1,r_excrDDA_n,r_excrDDA_2,r_pomDDA,&
      mu_UN,alpha_UN,k_DOM,r_SDOM,mu_BA,& !b_SDONlabi,b_SDOPlabi
      b_BAresp,r_BAadju,r_BAremi,r_BArefr,&
      f_BAslct,r_BAresp_1,r_BAresp_min, &
      r_BAresp_max, r_BAmort, mu_PRT,g_sp,&
      g_ba,r_PRTex,f_exPRTldom,r_PRTresp_1,&
      r_PRTresp_2,r_PRTadju,r_PRTremi,&
      r_pomPRT,mu_MZ,g_prt,g_tr,g_dda,r_MZex,&
      f_exMZldom,r_MZresp_1,r_MZresp_2,r_MZadju,r_MZremi,&
      r_MZpom,r_MZremv,f_HZsdom,f_HZpom,q_refrDOM_n,q_refrDOM_p,&
      q_POM_n,q_POM_p,r_nitrf, remin_prf_n,remin_prf_p,wnsvo,remin, &
      R_std,epsilon_anh4,epsilon_ano3,epsilon_zexrt,epsilon_nitrf, &
      epsilon_denit,epsilon_remin,epsilon_nf,epsilon_g,epsilon_exnh4, &
      epsilon_BAdon,delta_no3,delta_nh4,R_inno3,R_innh4 !新增15个15N生态系数

     namelist /ecosys_parms_nml/ &
       ae, &
       mu_SP, &
       alpha_SP, &
       a_SP, &
       v_SPn, &
       k_nh4SP, &
       k_no3SP, &
       v_SPp, &
       k_po4SP, &
       zeta, &
       theta, &
       r_excrSP_1, &
       r_excrSP_2, &
       r_pomSP, &
       mu_TR, &
       alpha_TR, &
       a_TR, &
       v_TRn, &
       k_nh4TR, &
       k_no3TR, &
       v_TRp, &
       k_po4TR, &
       mu_pickTRpo4, &
       zeta_nf, &
       r_excrTR_1, &
       r_excrTR_n, &
       r_excrTR_2, &
       r_pomTR, &
       mu_DDA, &
       alpha_DDA, &
       a_DDA, &
       v_DDAn, &
       k_nh4DDA, &
       k_no3DDA, &
       v_DDAp, &
       k_po4DDA, &
       mu_pickDDApo4, &
       zeta_nf_DDA, &
       r_excrDDA_1, &
       r_excrDDA_n, &
       r_excrDDA_2, &
       r_pomDDA, &
       mu_UN, &
       alpha_UN, &
       k_DOM, &
       !b_SDONlabi, &
       !b_SDOPlabi, &
       r_SDOM , &
       mu_BA, &
       b_BAresp, &
       r_BAadju, &
       r_BAremi, &
       r_BArefr, &
       f_BAslct, &
       r_BAresp_1, &
       r_BAresp_min, &
       r_BAresp_max, &
       r_BAmort, &
       mu_PRT, &
       g_sp, &
       g_ba, &
       r_PRTex, &
       f_exPRTldom, &
       r_PRTresp_1, &
       r_PRTresp_2, &
       r_PRTadju, &
       r_PRTremi, &
       r_pomPRT, &
       mu_MZ, &
       g_prt, &
       g_tr, &
       g_dda, &
       r_MZex, &
       f_exMZldom, &
       r_MZresp_1, &
       r_MZresp_2, &
       r_MZadju, &
       r_MZremi, &
       r_MZpom, &
!       r_MZrefr, &
       r_MZremv, &
       f_HZsdom, &
       f_HZpom, &
!       r_SDOMrefr, &
       q_refrDOM_n, &
       q_refrDOM_p, &
       q_POM_n, &
       q_POM_p, &
       r_nitrf, &
       remin_prf_n, &
       remin_prf_p, &
       wnsvo, &
       remin, &
       R_std, &
       epsilon_anh4, &
       epsilon_ano3, &
       epsilon_zexrt, &
       epsilon_nitrf, & 
       epsilon_denit, &
       epsilon_remin, &
       epsilon_nf, &
       epsilon_g, &
       epsilon_exnh4, &
       epsilon_BAdon, &
       delta_no3, & 
       delta_nh4, & 
       R_inno3, & 
       R_innh4

! unit number for ecosystem parameters I/O
  integer, parameter :: ecopar_unit=58 

!-------------------------------------------------------------------------
! NetCDF variable attributes for biological scalars
! ***Required*** for netcdf output of state variables and diagnostics
!-------------------------------------------------------------------------
  type(ncvar_attrib), dimension(NumStateVar) :: bio_ncvaratts=(/ &
       ncvar_attrib('SPc','phytoplankton C','mmol/m3',-1,ncf_float), &
       ncvar_attrib('SPn','phytoplankton N','mmol/m3',-1,ncf_float), &
       ncvar_attrib('SPp','phytoplankton P','mmol/m3',-1,ncf_float), &
       ncvar_attrib('TRc','Trichodesmium C','mmol/m3',-1,ncf_float), &
       ncvar_attrib('TRn','Trichodesmium N','mmol/m3',-1,ncf_float), &
       ncvar_attrib('TRp','Trichodesmium P','mmol/m3',-1,ncf_float), &
       ncvar_attrib('DDAc','Diatom-associations C','mmol/m3',-1,ncf_float), &
       ncvar_attrib('DDAn','Diatom-associations N','mmol/m3',-1,ncf_float), &
       ncvar_attrib('DDAp','Diatom-associations P','mmol/m3',-1,ncf_float), &
       ncvar_attrib('UNc','Unicellular N2-fixers C','mmol/m3',-1,ncf_float), &
       ncvar_attrib('UNn','Unicellular N2-fixers N','mmol/m3',-1,ncf_float), &
       ncvar_attrib('UNp','Unicellular N2-fixers P','mmol/m3',-1,ncf_float), &
       ncvar_attrib('BAc','bacterial C','mmol/m3',-1,ncf_float), &
       ncvar_attrib('BAn','bacterial N','mmol/m3',-1,ncf_float), &
       ncvar_attrib('BAp','bacterial P','mmol/m3',-1,ncf_float), &
       ncvar_attrib('PRTc','protozoan C','mmol/m3',-1,ncf_float), &
       ncvar_attrib('PRTn','protozoan N','mmol/m3',-1,ncf_float), &
       ncvar_attrib('PRTp','protozoan P','mmol/m3',-1,ncf_float), &
       ncvar_attrib('MZc','microzooplankton C','mmol/m3',-1,ncf_float), &
       ncvar_attrib('MZn','microzooplankton N','mmol/m3',-1,ncf_float), &
       ncvar_attrib('MZp','microzooplankton P','mmol/m3',-1,ncf_float), &
       ncvar_attrib('LDOMc','labile DOM C','mmol/m3',-1,ncf_float), &
       ncvar_attrib('LDOMn','labile DOM N','mmol/m3',-1,ncf_float), &
       ncvar_attrib('LDOMp','labile DOM P','mmol/m3',-1,ncf_float), &
       ncvar_attrib('SDOMc','semi-labile DOM C','mmol/m3',-1,ncf_float), &
       ncvar_attrib('SDOMn','semi-labile DOM N','mmol/m3',-1,ncf_float), &
       ncvar_attrib('SDOMp','semi-labile DOM P','mmol/m3',-1,ncf_float), &
       ncvar_attrib('DETc','detritus C','mmol/m3',-1,ncf_float), &
       ncvar_attrib('DETn','detritus N','mmol/m3',-1,ncf_float), &
       ncvar_attrib('DETp','detritus P','mmol/m3',-1,ncf_float), &
       ncvar_attrib('NH4','ammonium','mmol/m3',-1,ncf_float), &
       ncvar_attrib('NO3','nitrate','mmol/m3',-1,ncf_float), &
       ncvar_attrib('PO4','phosphate','mmol/m3',-1,ncf_float), &
       ncvar_attrib('SPchl','SP chlorophyll a','mg/m3',-1,ncf_float), &
       ncvar_attrib('TRchl','TR chlorophyll a','mg/m3',-1,ncf_float), &
       ncvar_attrib('DDAchl','DDA chlorophyll a','mg/m3',-1,ncf_float), &
       ncvar_attrib('UNchl','UN chlorophyll','mg/m3',-1,ncf_float), &
       ! ===== 15N =====
        ncvar_attrib('SP15n','phytoplankton 15N','mmol/m3',-1,ncf_float), &
        ncvar_attrib('TR15n','Trichodesmium 15N','mmol/m3',-1,ncf_float), &
        ncvar_attrib('DDA15n','DDA 15N','mmol/m3',-1,ncf_float), &
        ncvar_attrib('UN15n','unicellular 15N','mmol/m3',-1,ncf_float), &
        ncvar_attrib('BA15n','bacterial 15N','mmol/m3',-1,ncf_float), &
        ncvar_attrib('PRT15n','protozoan 15N','mmol/m3',-1,ncf_float), &
        ncvar_attrib('MZ15n','microzooplankton 15N','mmol/m3',-1,ncf_float), &
        ncvar_attrib('LDOM15n','LDOM 15N','mmol/m3',-1,ncf_float), &
        ncvar_attrib('SDOM15n','SDOM 15N','mmol/m3',-1,ncf_float), &
        ncvar_attrib('DET15n','detritus 15N','mmol/m3',-1,ncf_float), &
        ncvar_attrib('15NH4','ammonium 15N','mmol/m3',-1,ncf_float), &
        ncvar_attrib('15NO3','nitrate 15N','mmol/m3',-1,ncf_float) &
/)

  type(ncvar_attrib), dimension(NumDiagVar) :: diag_ncvaratts=(/ &
       ncvar_attrib('pp','primary productivity','mgC/m3/sec',-1,ncf_float),&
       ncvar_attrib('prBAc','prBAc','mmol C/m3/sec',-1,ncf_float), &
       ncvar_attrib('growSPc','growSPc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growSPnh4','growSPnh4','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growSPno3','growSPno3','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growSPn','growSPn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growSPp','growSPp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrSP_1c','excrSP_1c','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrSP_1n','excrSP_1n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrSP_1p','excrSP_1p','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrSP_2c','excrSP_2c','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrSP_2n','excrSP_2n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrSP_2p','excrSP_2p','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('pomSPc','pomSPc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('pomSPn','pomSPn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('pomSPp','pomSPp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('grazSPc','grazSPc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('grazSPn','grazSPn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('grazSPp','grazSPp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growTRc','growTRc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growTRnh4','growTRnh4','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growTRno3','growTRno3','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growTRnf','growTRnf','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growTRn','growTRn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growTRpo4','growTRpo4','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('pickTRpo4','pickTRpo4','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growTRp','growTRp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrTR_1c','excrTR_1c','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrTR_1n','excrTR_1n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrTR_1p','excrTR_1p','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrTR_nh4','excrTR_nh4','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrTR_2c','excrTR_2c','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrTR_2n','excrTR_2n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrTR_2p','excrTR_2p','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('pomTRc','pomTRc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('pomTRn','pomTRn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('pomTRp','pomTRp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('grazTRc','grazTRc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('grazTRn','grazTRn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('grazTRp','grazTRp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growDDAc','growDDAc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growDDAnh4','growDDAnh4','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growDDAno3','growDDAno3','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growDDAnf','growDDAnf','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growDDAn','growDDAn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growDDApo4','growDDApo4','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('pickDDApo4','pickDDApo4','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growDDAp','growDDAp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrDDA_1c','excrDDA_1c','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrDDA_1n','excrDDA_1n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrDDA_1p','excrDDA_1p','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrDDA_nh4','excrDDA_nh4','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrDDA_2c','excrDDA_2c','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrDDA_2n','excrDDA_2n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrDDA_2p','excrDDA_2p','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('pomDDAc','pomDDAc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('pomDDAn','pomDDAn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('pomDDAp','pomDDAp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('grazDDAc','grazDDAc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('grazDDAn','grazDDAn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('grazDDAp','grazDDAp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growUNc','growUNc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growUNnh4','growUNnh4','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growUNno3','growUNno3','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growUNnf','growUNnf','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growUNn','growUNn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growUNp','growUNp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrUN_1c','excrUN_1c','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrUN_1n','excrUN_1n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrUN_1p','excrUN_1p','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrUN_nh4','excrUN_nh4','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrUN_2c','excrUN_2c','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrUN_2n','excrUN_2n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrUN_2p','excrUN_2p','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('pomUNc','pomUNc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('pomUNn','pomUNn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('pomUNp','pomUNp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('grazUNc','grazUNc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('grazUNn','grazUNn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('grazUNp','grazUNp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growBAldoc','growBAldoc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growBAldon','growBAldon','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growBAldop','growBAldop','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growBAsdoc','growBAsdoc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growBAsdon','growBAsdon','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growBAsdop','growBAsdop','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growBAnh4','growBAnh4','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growBAno3','growBAno3','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growBApo4','growBApo4','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growBAc','growBAc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growBAn','growBAn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growBAp','growBAp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('respBA','respBA','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('refrBAc','refrBAc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('refrBAn','refrBAn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('refrBAp','refrBAp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrBAc','excrBAc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrBAn','excrBAn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrBAp','excrBAp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('remiBAn','remiBAn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('remiBAp','remiBAp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('grazBAc','grazBAc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('grazBAn','grazBAn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('grazBAp','grazBAp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('mortBAc','mortBAc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('mortBAn','mortBAn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('mortBAp','mortBAp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('fluxBAnh4','fluxBAnh4','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('fluxBApo4','fluxBApo4','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growPRTc','growPRTc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growPRTn','growPRTn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growPRTp','growPRTp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('respPRT','respPRT','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrPRTldomc','excrPRTldomc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrPRTldomn','excrPRTldomn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrPRTldomp','excrPRTldomp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrPRTsdomc','excrPRTsdomc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrPRTsdomn','excrPRTsdomn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrPRTsdomp','excrPRTsdomp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrPRTsdom2c','excrPRTsdom2c','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrPRTsdom2n','excrPRTsdom2n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrPRTsdom2p','excrPRTsdom2p','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('remiPRTn','remiPRTn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('remiPRTp','remiPRTp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('pomPRTc','pomPRTc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('pomPRTn','pomPRTn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('pomPRTp','pomPRTp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('grazPRTc','grazPRTc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('grazPRTn','grazPRTn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('grazPRTp','grazPRTp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growMZc','growMZc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growMZn','growMZn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growMZp','growMZp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('respSP','respSP','mmol C/m3/sec',-1,ncf_float), &
       ncvar_attrib('respTR','respTR','mmol C/m3/sec',-1,ncf_float), &
       ncvar_attrib('respDDA','respDDA','mmol C/m3/sec',-1,ncf_float), &
       ncvar_attrib('respUN','respUN','mmol C/m3/sec',-1,ncf_float), & 
       ncvar_attrib('respMZ','respMZ','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrMZldomc','excrMZldomc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrMZldomn','excrMZldomn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrMZldomp','excrMZldomp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrMZsdomc','excrMZsdomc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrMZsdomn','excrMZsdomn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrMZsdomp','excrMZsdomp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrMZsdom2c','excrMZsdom2c','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrMZsdom2n','excrMZsdom2n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrMZsdom2p','excrMZsdom2p','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('remiMZn','remiMZn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('remiMZp','remiMZp','mmol/m3/sec',-1,ncf_float), &
!       ncvar_attrib('refrMZc','refrMZc','mmol/m3/sec',-1,ncf_float), &
!       ncvar_attrib('refrMZn','refrMZn','mmol/m3/sec',-1,ncf_float), &
!       ncvar_attrib('refrMZp','refrMZp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('pomMZc','pomMZc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('pomMZn','pomMZn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('pomMZp','pomMZp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('remvMZc','remvMZc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('remvMZn','remvMZn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('remvMZp','remvMZp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('pomHZc','pomHZc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('pomHZn','pomHZn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('pomHZp','pomHZp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrHZsdomc','excrHZsdomc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrHZsdomn','excrHZsdomn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrHZsdomp','excrHZsdomp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('remiHZn','remiHZn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('remiHZp','remiHZp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('disDETc','disDETc','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('disDETn','disDETn','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('disDETp','disDETp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('nitrf','nitrf','mmol/m3/sec',-1,ncf_float), &
!       ncvar_attrib('refrSDOMc','refrSDOMc','mmol/m3/sec',-1,ncf_float), &
!       ncvar_attrib('refrSDOMn','refrSDOMn','mmol/m3/sec',-1,ncf_float), &
!       ncvar_attrib('refrSDOMp','refrSDOMp','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('exportc','C export flux','mmol C/m2/sec',-1,ncf_float), &
       ncvar_attrib('exportn','N export flux','mmol C/m2/sec',-1,ncf_float), &
       ncvar_attrib('exportp','P export flux','mmol C/m2/sec',-1,ncf_float), &

      ! ===== 15N variables =====
       ncvar_attrib('igrowSP15nh4','igrowSP15nh4','mmol/m3/sec',-1,ncf_float),&
       ncvar_attrib('igrowSP15no3','igrowSP15no3','mmol/m3/sec',-1,ncf_float),&
       ncvar_attrib('iexcrSP_1_15n','iexcrSP_1_15n','mmol/m3/sec',-1,ncf_float),&
       ncvar_attrib('iexcrSP_2_15n','iexcrSP_2_15n','mmol/m3/sec',-1,ncf_float),&
       ncvar_attrib('ipomSP15n','ipomSP15n','mmol/m3/sec',-1,ncf_float),&
       ncvar_attrib('igrazSP15n','igrazSP15n','mmol/m3/sec',-1,ncf_float),&
   
       ncvar_attrib('igrowTR15nh4','igrowTR15nh4','mmol/m3/sec',-1,ncf_float),&
       ncvar_attrib('igrowTR15no3','igrowTR15no3','mmol/m3/sec',-1,ncf_float),&
       ncvar_attrib('igrowTR15nf ','igrowTR15nf ','mmol/m3/sec',-1,ncf_float),&
       ncvar_attrib('iexcrTR_1_15n','iexcrTR_1_15n','mmol/m3/sec',-1,ncf_float),&
       ncvar_attrib('iexcrTR_2_15n','iexcrTR_2_15n','mmol/m3/sec',-1,ncf_float),&
       ncvar_attrib('iexcrTR_15nh4','iexcrTR_15nh4','mmol/m3/sec',-1,ncf_float),&
       ncvar_attrib('ipomTR15n','ipomTR15n','mmol/m3/sec',-1,ncf_float),&
       ncvar_attrib('igrazTR15n','igrazTR15n','mmol/m3/sec',-1,ncf_float),&
   
       ncvar_attrib('igrowDDA15nh4','igrowDDA15nh4','mmol/m3/sec',-1,ncf_float),&
       ncvar_attrib('igrowDDA15no3','igrowDDA15no3','mmol/m3/sec',-1,ncf_float),&
       ncvar_attrib('igrowDDA15nf ','igrowDDA15nf ','mmol/m3/sec',-1,ncf_float),&
       ncvar_attrib('iexcrDDA_1_15n','iexcrDDA_1_15n','mmol/m3/sec',-1,ncf_float),&
       ncvar_attrib('iexcrDDA_2_15n','iexcrDDA_2_15n','mmol/m3/sec',-1,ncf_float),&
       ncvar_attrib('iexcrDDA_15nh4','iexcrDDA_15nh4','mmol/m3/sec',-1,ncf_float),&
       ncvar_attrib('ipomDDA15n','ipomDDA15n','mmol/m3/sec',-1,ncf_float),&
       ncvar_attrib('igrazDDA15n','igrazDDA15n','mmol/m3/sec',-1,ncf_float),&
   
       ncvar_attrib('igrowUN15nh4','igrowUN15nh4','mmol/m3/sec',-1,ncf_float),&
       ncvar_attrib('igrowUN15no3','igrowUN15no3','mmol/m3/sec',-1,ncf_float),&
       ncvar_attrib('igrowUN15nf ','igrowUN15nf ','mmol/m3/sec',-1,ncf_float),&
       ncvar_attrib('iexcrUN_1_15n','iexcrUN_1_15n','mmol/m3/sec',-1,ncf_float),&
       ncvar_attrib('iexcrUN_2_15n','iexcrUN_2_15n','mmol/m3/sec',-1,ncf_float),&
       ncvar_attrib('iexcrUN_15nh4','iexcrUN_15nh4','mmol/m3/sec',-1,ncf_float),&
       ncvar_attrib('ipomUN15n','ipomUN15n','mmol/m3/sec',-1,ncf_float),&
       ncvar_attrib('igrazUN15n','igrazUN15n','mmol/m3/sec',-1,ncf_float), &

       ncvar_attrib('growBA15ldon','growBA15ldon','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growBA15sdon','growBA15sdon','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growBA15nh4','growBA15nh4','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growBA15no3','growBA15no3','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growBA15n','growBA15n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('refrBA15n','refrBA15n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrBA15n','excrBA15n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('remiBA15n','remiBA15n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('grazBA15n','grazBA15n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('mortBA15n','mortBA15n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('fluxBA15nh4','fluxBA15nh4','mmol/m3/sec',-1,ncf_float), &
     
       ncvar_attrib('growPRT15n','growPRT15n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrPRTldom15n','excrPRTldom15n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrPRTsdom15n','excrPRTsdom15n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrPRTsdom2_15n','excrPRTsdom2_15n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('remiPRT15n','remiPRT15n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('pomPRT15n','pomPRT15n','mmol/m3/sec',-1,ncf_float), &     
       ncvar_attrib('grazPRT15n','grazPRT15n','mmol/m3/sec',-1,ncf_float), &

       ncvar_attrib('growMZ15n','growMZ15n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrMZldom15n','excrMZldom15n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrMZsdom15n','excrMZsdom15n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrMZsdom2_15n','excrMZsdom2_15n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('remiMZ15n','remiMZ15n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('pomMZ15n','pomMZ15n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('remvMZ15n','remvMZ15n','mmol/m3/sec',-1,ncf_float), &

       ncvar_attrib('pomHZ15n','pomHZ15n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('excrHZsdom15n','excrHZsdom15n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('remiHZ15n','remiHZ15n','mmol/m3/sec',-1,ncf_float), &

       ncvar_attrib('disDET15n','disDET15n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('15nitrf','15nitrf','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('export15n','export15n','mmol/m3/sec',-1,ncf_float), &

       ncvar_attrib('growSP15n','growSP15n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growTR15n',' growTR15n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growDDA15n','growDDA15n','mmol/m3/sec',-1,ncf_float), &
       ncvar_attrib('growUN15n','growUN15n','mmol/m3/sec',-1,ncf_float) &
    /)      
    contains


! ***Required*** 
  subroutine read_eco_params(bioparams_default,bioparams)

    use common_mod, only : ecopar_fname
    use eco_params, only : iae, imu_SP,ialpha_SP,&
          ia_SP,iv_SPn,ik_nh4SP,ik_no3SP,&
          iv_SPp,ik_po4SP,izeta,itheta,&
          ir_excrSP_1,ir_excrSP_2,ir_pomSP,&
	  imu_TR, ialpha_TR,ia_TR,iv_TRn, ik_nh4TR,ik_no3TR,&
	  iv_TRp,ik_po4TR,imu_pickTRpo4, izeta_nf,&
          ir_excrTR_1,ir_excrTR_n,ir_excrTR_2,ir_pomTR,imu_DDA,&
          ialpha_DDA,ia_DDA,iv_DDAn, ik_nh4DDA,ik_no3DDA,&
	  iv_DDAp,ik_po4DDA,imu_pickDDApo4, izeta_nf_DDA,&
          ir_excrDDA_1,ir_excrDDA_n,ir_excrDDA_2,ir_pomDDA,&
          imu_UN,ialpha_UN,ik_DOM,ir_SDOM,imu_BA,& 
          ib_BAresp,ir_BAadju,ir_BAremi,ir_BArefr,&
          if_BAslct,ir_BAresp_1, ir_BAresp_min,ir_BAresp_max,& 
          ir_BAmort,imu_PRT,ig_sp,ig_ba,&
          ir_PRTex,if_exPRTldom,ir_PRTresp_1,&
          ir_PRTresp_2,ir_PRTadju,ir_PRTremi,ir_pomPRT,&
          imu_MZ,ig_prt,ig_tr, ig_dda,ir_MZex,&
          if_exMZldom,ir_MZresp_1,ir_MZresp_2,ir_MZadju,&
          ir_MZremi,ir_MZpom,ir_MZremv,if_HZsdom,if_HZpom,&
          iq_refrDOM_n, iq_refrDOM_p,iq_POM_n,iq_POM_p,&
          ir_nitrf,iremin_prf_n,iremin_prf_p,iwnsvo,iremin, &
          iR_std, iepsilon_anh4, iepsilon_ano3, iepsilon_zexrt,&
          iepsilon_nitrf, iepsilon_denit, iepsilon_remin,&
          iepsilon_nf, iepsilon_g, iepsilon_exnh4, &
          iepsilon_BAdon, idelta_no3, idelta_nh4,&
          iR_inno3, iR_innh4

    implicit none

!-----------------------------------------------------------------------
! Arguments
!-----------------------------------------------------------------------
    double precision, dimension(:), intent(out) :: &
         bioparams,bioparams_default

!-----------------------------------------------------------------------
! Local variables
!-----------------------------------------------------------------------
    integer, parameter :: ecopar_unit=43

!-----------------------------------------------------------------------
!   default namelist settings
!-----------------------------------------------------------------------
 AE = 4000.0
 MU_SP =  2.9458!2.6437 !1.7096560001498988 !
 ALPHA_SP =6.2404!5.8562 !5.51273526304891 ! 
 A_SP = 5.0E-4
 V_SPN = 0.3!0.7248 
 K_NH4SP = 0.01
 K_NO3SP = 0.1
 V_SPP =0.0090 !0.0106 !0.010133492869505855! 
 K_PO4SP = 4.8E-3
 ZETA = 2.0
 THETA = 3.2893 !3.1457 !2.1288532650970557!
 R_EXCRSP_1 = 0.05
 R_EXCRSP_2 = 0.05
 R_POMSP = 0.038
 MU_TR = 4.3366 !3.0
 ALPHA_TR = 0.25
 A_TR = 2.0E-3
 V_TRN =0.4 ! 2.1101 !0.4
 K_NH4TR = 0.05
 K_NO3TR = 0.5
 V_TRP = 0.02
 K_PO4TR = 0.047
 MU_PICKTRPO4 = 1.0
 ZETA_NF = 2.5
 R_EXCRTR_1 = 0.02
 R_EXCRTR_N = 0.36
 R_EXCRTR_2 = 0.05
 R_POMTR =0.6032! 1.3344 !0.08
 MU_DDA = 2.5
 ALPHA_DDA = 0.15
 A_DDA = 2.0E-3
 V_DDAN =0.1 
 K_NH4DDA = 0.1
 K_NO3DDA = 1
 V_DDAP = 0.02
 K_PO4DDA = 0.047
 MU_PICKDDAPO4 = 1.0
 ZETA_NF_DDA = 2.5
 R_EXCRDDA_1 = 0.02
 R_EXCRDDA_N = 0.36
 R_EXCRDDA_2 = 0.05
 R_POMDDA =0.6032! 1.3344 !0.08
 MU_UN = 2.5885 !1.6
 ALPHA_UN = 0.5
 K_DOM = 0.5
 R_SDOM =0.0016 !9.586773754551453E-4! 
 MU_BA = 2.0
 B_BARESP = 0.28
 R_BAADJU = 2.0
 R_BAREMI = 6.0
 R_BAREFR = 0.018
 F_BASLCT = 0.2
 R_BARESP_1 = 0.01
 R_BARESP_MIN = 0.35
 R_BARESP_MAX = 0.5527! 0.6734616399721387!
 R_BAMORT = 0.1
 MU_PRT = 6.2569 !4.0
 G_SP = 0.7512749623651648! 0.7699 !
 G_BA = 2.05!1.4299980380426615
 R_PRTEX = 0.2
 F_EXPRTLDOM = 0.8195! 0.9
 R_PRTRESP_1 = 0.01
 R_PRTRESP_2 = 0.7529! 0.7584721354023397
 R_PRTADJU = 2.0
 R_PRTREMI = 4.7
 R_POMPRT = 0.027
 MU_MZ = 1.3 !3.6981 !1.3
 G_PRT = 1.4
 G_TR = 0.1626 !0.20072614502968935!
 G_DDA = 0.1626 
 R_MZEX = 0.3
 F_EXMZLDOM = 0.75
 R_MZRESP_1 = 0.03
 R_MZRESP_2 = 0.22
 R_MZADJU = 2.0
 R_MZREMI = 4.0
 R_MZPOM = 0.15
 R_MZREMV = 0.81 !0.2302 !
 F_HZSDOM = 0.1
 F_HZPOM = 0.14
 Q_REFRDOM_N = 0.05
 Q_REFRDOM_P = 7.0E-4
 Q_POM_N = 0.12
 Q_POM_P = 4.5E-3
 R_NITRF = 0.1
 REMIN_PRF_N = 1.1
 REMIN_PRF_P = 4.0
 WNSVO = 3.9061
 REMIN = 0.011
 R_STD = 0.0036765
 EPSILON_ANH4   = 5.0     ! NH4+ 吸收分馏 (5‰)
 EPSILON_ANO3   = 5.0      ! NO3- 吸收分馏 (5‰)
 EPSILON_ZEXRT  = 6.0    ! Zooplankton excretion (~1‰)
 EPSILON_NITRF  = 8.0     ! Nitrification (~15‰, 较强分馏)
 EPSILON_DENIT  = 25.0     ! Denitrification (~20‰, 高分馏)
 EPSILON_REMIN  = 1      ! Remineralization (~1‰)
 EPSILON_NF     = 1.5        ! Nitrogen fixation (~1.5‰)
 EPSILON_G      = -3.5      ! Grazing (~1‰)
 EPSILON_EXNH4  = 5.0      ! NH4 excretion (~1‰)
 EPSILON_BADON  = 2.0     ! Bacterial uptake / DON (~2‰)
 DELTA_NO3      = 5.0     ! NO3 δ15N (~5‰, 深海典型值)
 DELTA_NH4      = 0.0         ! NH4 δ15N (~0‰ 或接近源值)
 R_INNO3        = R_STD * (1.0 + DELTA_NO3/1000)
 R_INNH4        = R_STD * (1.0 + DELTA_NH4/1000)
 
 
 
!-----------------------------------------------------------------------
! set default parameter values
!-----------------------------------------------------------------------
    bioparams_default(iae          )=ae
    bioparams_default(imu_SP          )=mu_SP
    bioparams_default(ialpha_SP       )=alpha_SP
    bioparams_default(ia_SP       )=a_SP
    bioparams_default(iv_SPn       )=v_SPn
    bioparams_default(ik_nh4SP        )=k_nh4SP
    bioparams_default(ik_no3SP        )=k_no3SP
    bioparams_default(iv_SPp       )=v_SPp
    bioparams_default(ik_po4SP        )=k_po4SP
    bioparams_default(izeta       )=zeta
    bioparams_default(itheta       )=theta
    bioparams_default(ir_excrSP_1     )=r_excrSP_1
    bioparams_default(ir_excrSP_2     )=r_excrSP_2
    bioparams_default(ir_pomSP        )=r_pomSP
    bioparams_default(imu_TR          )=mu_TR
    bioparams_default(ialpha_TR       )=alpha_TR
    bioparams_default(ia_TR       )=a_TR
    bioparams_default(iv_TRn       )=v_TRn
    bioparams_default(ik_nh4TR        )=k_nh4TR
    bioparams_default(ik_no3TR        )=k_no3TR
    bioparams_default(iv_TRp       )=v_TRp
    bioparams_default(ik_po4TR        )=k_po4TR
    bioparams_default(imu_pickTRpo4) = mu_pickTRpo4
    bioparams_default(izeta_nf       )=zeta_nf
    bioparams_default(ir_excrTR_1     )=r_excrTR_1
    bioparams_default(ir_excrTR_n   )=r_excrTR_n
    bioparams_default(ir_excrTR_2     )=r_excrTR_2
    bioparams_default(ir_pomTR        )=r_pomTR
    bioparams_default(imu_DDA          )=mu_DDA
    bioparams_default(ialpha_DDA       )=alpha_DDA
    bioparams_default(ia_DDA       )=a_DDA
    bioparams_default(iv_DDAn       )=v_DDAn
    bioparams_default(ik_nh4DDA        )=k_nh4DDA
    bioparams_default(ik_no3DDA        )=k_no3DDA
    bioparams_default(iv_DDAp       )=v_DDAp
    bioparams_default(ik_po4DDA        )=k_po4DDA
    bioparams_default(imu_pickDDApo4) = mu_pickDDApo4
    bioparams_default(izeta_nf_DDA       )=zeta_nf_DDA
    bioparams_default(ir_excrDDA_1     )=r_excrDDA_1
    bioparams_default(ir_excrDDA_n   )=r_excrDDA_n
    bioparams_default(ir_excrDDA_2     )=r_excrDDA_2
    bioparams_default(ir_pomDDA        )=r_pomDDA
    bioparams_default(imu_UN          )=mu_UN
    bioparams_default(ialpha_UN       )=alpha_UN
    bioparams_default(ik_DOM        )=k_DOM
    !bioparams_default(ib_SDONlabi     )=b_SDONlabi
    !bioparams_default(ib_SDOPlabi     )=b_SDOPlabi
    bioparams_default(ir_SDOM     )=r_SDOM
    bioparams_default(imu_BA          )=mu_BA
    bioparams_default(ib_BAresp       )=b_BAresp
    bioparams_default(ir_BAadju       )=r_BAadju
    bioparams_default(ir_BAremi       )=r_BAremi
    bioparams_default(ir_BArefr       )=r_BArefr
    bioparams_default(if_BAslct) = f_BAslct
    bioparams_default(ir_BAresp_1) = r_BAresp_1
    bioparams_default(ir_BAresp_min) = r_BAresp_min
    bioparams_default(ir_BAresp_max) = r_BAresp_max
    bioparams_default(ir_BAmort) = r_BAmort
    bioparams_default(imu_PRT       )=mu_PRT
    bioparams_default(ig_sp        )=g_sp
    bioparams_default(ig_ba        )=g_ba
    bioparams_default(ir_PRTex        )=r_PRTex
    bioparams_default(if_exPRTldom    )=f_exPRTldom
    bioparams_default(ir_PRTresp_1    )=r_PRTresp_1
    bioparams_default(ir_PRTresp_2    )=r_PRTresp_2
    bioparams_default(ir_PRTadju      )=r_PRTadju
    bioparams_default(ir_PRTremi      )=r_PRTremi
    bioparams_default(ir_pomPRT       )=r_pomPRT
    bioparams_default(imu_MZ       )=mu_MZ
    bioparams_default(ig_prt        )=g_prt
    bioparams_default(ig_tr         )=g_tr
    bioparams_default(ig_dda         )=g_dda
    bioparams_default(ir_MZex         )=r_MZex
    bioparams_default(if_exMZldom     )=f_exMZldom
    bioparams_default(ir_MZresp_1     )=r_MZresp_1
    bioparams_default(ir_MZresp_2     )=r_MZresp_2
    bioparams_default(ir_MZadju       )=r_MZadju
    bioparams_default(ir_MZremi       )=r_MZremi
    bioparams_default(ir_MZpom        )=r_MZpom
!    bioparams_default(ir_MZrefr       )=r_MZrefr
    bioparams_default(ir_MZremv       )=r_MZremv
    bioparams_default(if_HZsdom       )=f_HZsdom
    bioparams_default(if_HZpom        )=f_HZpom
!    bioparams_default(ir_SDOMrefr     )=r_SDOMrefr
    bioparams_default(iq_refrDOM_n) = q_refrDOM_n
    bioparams_default(iq_refrDOM_p) = q_refrDOM_p
    bioparams_default(iq_POM_n        )=q_POM_n
    bioparams_default(iq_POM_p        )=q_POM_p
    bioparams_default(ir_nitrf        )=r_nitrf
    bioparams_default(iremin_prf_n    )=remin_prf_n
    bioparams_default(iremin_prf_p    )=remin_prf_p
    bioparams_default(iwnsvo         )=wnsvo
    bioparams_default(iremin          )=remin

    !-----------------------------------------------------------------------
    ! 15N isotope-related parameters (新增部分)
    !-----------------------------------------------------------------------
    bioparams_default(iR_std)        = R_std
    bioparams_default(iepsilon_anh4) = epsilon_anh4
    bioparams_default(iepsilon_ano3) = epsilon_ano3
    bioparams_default(iepsilon_zexrt)= epsilon_zexrt
    bioparams_default(iepsilon_nitrf) = epsilon_nitrf
    bioparams_default(iepsilon_denit) = epsilon_denit
    bioparams_default(iepsilon_remin) = epsilon_remin
    bioparams_default(iepsilon_nf)    = epsilon_nf
    bioparams_default(iepsilon_g)     = epsilon_g
    bioparams_default(iepsilon_exnh4) = epsilon_exnh4
    bioparams_default(iepsilon_BAdon) = epsilon_BAdon
    bioparams_default(idelta_no3)    = delta_no3
    bioparams_default(idelta_nh4)    = delta_nh4
    bioparams_default(iR_inno3)      = R_inno3
    bioparams_default(iR_innh4)      = R_innh4

!-----------------------------------------------------------------------
!   read in parameter values
!-----------------------------------------------------------------------
    open(unit=ecopar_unit, file=trim(ecopar_fname), status='old')
    read(unit=ecopar_unit, nml=ecosys_parms_nml)
    close(unit=ecopar_unit)

    bioparams(iae          )=ae
    bioparams(imu_SP          )=mu_SP
    bioparams(ialpha_SP       )=alpha_SP
    bioparams(ia_SP       )=a_SP
    bioparams(iv_SPn       )=v_SPn
    bioparams(ik_nh4SP        )=k_nh4SP
    bioparams(ik_no3SP        )=k_no3SP
    bioparams(iv_SPp       )=v_SPp
    bioparams(ik_po4SP        )=k_po4SP
    bioparams(izeta       )=zeta
    bioparams(itheta       )=theta
    bioparams(ir_excrSP_1     )=r_excrSP_1
    bioparams(ir_excrSP_2     )=r_excrSP_2
    bioparams(ir_pomSP        )=r_pomSP
    bioparams(imu_TR          )=mu_TR
    bioparams(ialpha_TR       )=alpha_TR
    bioparams(ia_TR       )=a_TR
    bioparams(iv_TRn       )=v_TRn
    bioparams(ik_nh4TR        )=k_nh4TR
    bioparams(ik_no3TR        )=k_no3TR
    bioparams(iv_TRp       )=v_TRp
    bioparams(ik_po4TR        )=k_po4TR
    bioparams(imu_pickTRpo4) = mu_pickTRpo4
    bioparams(izeta_nf       )=zeta_nf
    bioparams(ir_excrTR_1     )=r_excrTR_1
    bioparams(ir_excrTR_n   )=r_excrTR_n
    bioparams(ir_excrTR_2     )=r_excrTR_2
    bioparams(ir_pomTR        )=r_pomTR
    bioparams(imu_DDA          )=mu_DDA
    bioparams(ialpha_DDA       )=alpha_DDA
    bioparams(ia_DDA       )=a_DDA
    bioparams(iv_DDAn       )=v_DDAn
    bioparams(ik_nh4DDA        )=k_nh4DDA
    bioparams(ik_no3DDA        )=k_no3DDA
    bioparams(iv_DDAp       )=v_DDAp
    bioparams(ik_po4DDA        )=k_po4DDA
    bioparams(imu_pickDDApo4) = mu_pickDDApo4
    bioparams(izeta_nf_DDA       )=zeta_nf_DDA
    bioparams(ir_excrDDA_1     )=r_excrDDA_1
    bioparams(ir_excrDDA_n   )=r_excrDDA_n
    bioparams(ir_excrDDA_2     )=r_excrDDA_2
    bioparams(ir_pomDDA        )=r_pomDDA
    bioparams(imu_UN          )=mu_UN
    bioparams(ialpha_UN       )=alpha_UN
    bioparams(ik_DOM        )=k_DOM
    !bioparams(ib_SDONlabi     )=b_SDONlabi
    !bioparams(ib_SDOPlabi     )=b_SDOPlabi
    bioparams(ir_SDOM     )=r_SDOM
    bioparams(imu_BA          )=mu_BA
    bioparams(ib_BAresp       )=b_BAresp
    bioparams(ir_BAadju       )=r_BAadju
    bioparams(ir_BAremi       )=r_BAremi
    bioparams(ir_BArefr       )=r_BArefr
    bioparams(if_BAslct) = f_BAslct
    bioparams(ir_BAresp_1) = r_BAresp_1
    bioparams(ir_BAresp_min) = r_BAresp_min
    bioparams(ir_BAresp_max) = r_BAresp_max
    bioparams(ir_BAmort) = r_BAmort
    bioparams(imu_PRT       )=mu_PRT
    bioparams(ig_sp        )=g_sp
    bioparams(ig_ba        )=g_ba
    bioparams(ir_PRTex        )=r_PRTex
    bioparams(if_exPRTldom    )=f_exPRTldom
    bioparams(ir_PRTresp_1    )=r_PRTresp_1
    bioparams(ir_PRTresp_2    )=r_PRTresp_2
    bioparams(ir_PRTadju      )=r_PRTadju
    bioparams(ir_PRTremi      )=r_PRTremi
    bioparams(ir_pomPRT       )=r_pomPRT
    bioparams(imu_MZ       )=mu_MZ
    bioparams(ig_prt        )=g_prt
    bioparams(ig_tr         )=g_tr
    bioparams(ig_dda         )=g_dda
    bioparams(ir_MZex         )=r_MZex
    bioparams(if_exMZldom     )=f_exMZldom
    bioparams(ir_MZresp_1     )=r_MZresp_1
    bioparams(ir_MZresp_2     )=r_MZresp_2
    bioparams(ir_MZadju       )=r_MZadju
    bioparams(ir_MZremi       )=r_MZremi
    bioparams(ir_MZpom        )=r_MZpom
!    bioparams(ir_MZrefr       )=r_MZrefr
    bioparams(ir_MZremv       )=r_MZremv
    bioparams(if_HZsdom       )=f_HZsdom
    bioparams(if_HZpom        )=f_HZpom
!    bioparams(ir_SDOMrefr     )=r_SDOMrefr
    bioparams(iq_refrDOM_n) = q_refrDOM_n
    bioparams(iq_refrDOM_p) = q_refrDOM_p
    bioparams(iq_POM_n        )=q_POM_n
    bioparams(iq_POM_p        )=q_POM_p
    bioparams(ir_nitrf        )=r_nitrf
    bioparams(iremin_prf_n    )=remin_prf_n
    bioparams(iremin_prf_p    )=remin_prf_p
    bioparams(iwnsvo          )=wnsvo
    bioparams(iremin          )=remin
    
    !-----------------------------------------
    ! 15N isotope-related parameters (新增)
    !-----------------------------------------
    bioparams(iR_std)        = R_std
    bioparams(iepsilon_anh4) = epsilon_anh4
    bioparams(iepsilon_ano3) = epsilon_ano3
    bioparams(iepsilon_zexrt)= epsilon_zexrt
    bioparams(iepsilon_nitrf) = epsilon_nitrf
    bioparams(iepsilon_denit) = epsilon_denit
    bioparams(iepsilon_remin) = epsilon_remin
    bioparams(iepsilon_nf)    = epsilon_nf
    bioparams(iepsilon_g)     = epsilon_g
    bioparams(iepsilon_exnh4) = epsilon_exnh4
    bioparams(iepsilon_BAdon) = epsilon_BAdon
    bioparams(idelta_no3)    = delta_no3
    bioparams(idelta_nh4)    = delta_nh4
    bioparams(iR_inno3)      = R_inno3
    bioparams(iR_innh4)      = R_innh4
    
  end subroutine read_eco_params

  subroutine write_eco_params(bioparams,filename)
    use eco_params
    implicit none

!-----------------------------------------------------------------------
! Arguments
!-----------------------------------------------------------------------
    double precision, dimension(:), intent(in) :: bioparams
    character(len=*) :: filename

!-----------------------------------------------------------------------
! Local variables
!-----------------------------------------------------------------------
    ae          = bioparams(iae         )
    mu_SP          = bioparams(imu_SP          )
    alpha_SP       = bioparams(ialpha_SP       )
    a_SP       = bioparams(ia_SP       )
    v_SPn        = bioparams(iv_SPn        )
    k_nh4SP        = bioparams(ik_nh4SP        )
    k_no3SP        = bioparams(ik_no3SP        )
    v_SPp           = bioparams(iv_SPp       )
    k_po4SP        = bioparams(ik_po4SP        )
    zeta              = bioparams(izeta       )
    theta            = bioparams(itheta       )
    r_excrSP_1     = bioparams(ir_excrSP_1     )
    r_excrSP_2     = bioparams(ir_excrSP_2     )
    r_pomSP        = bioparams(ir_pomSP        )
    mu_TR          = bioparams(imu_TR          )
    alpha_TR       = bioparams(ialpha_TR       )
    a_TR           = bioparams(ia_TR       )
    v_TRn        = bioparams(iv_TRn        )
    k_nh4TR        = bioparams(ik_nh4TR        )
    k_no3TR        = bioparams(ik_no3TR        )
    v_TRp        = bioparams(iv_TRp        )
    k_po4TR        = bioparams(ik_po4TR        )
    mu_pickTRpo4 = bioparams(imu_pickTRpo4)
    zeta_nf        = bioparams(izeta_nf        )
    r_excrTR_1     = bioparams(ir_excrTR_1     )
    r_excrTR_n   = bioparams(ir_excrTR_n   )
    r_excrTR_2     = bioparams(ir_excrTR_2     )
    r_pomTR        = bioparams(ir_pomTR        )
    mu_DDA          = bioparams(imu_DDA          )
    alpha_DDA       = bioparams(ialpha_DDA       )
    a_DDA           = bioparams(ia_DDA       )
    v_DDAn        = bioparams(iv_DDAn        )
    k_nh4DDA        = bioparams(ik_nh4DDA        )
    k_no3DDA        = bioparams(ik_no3DDA        )
    v_DDAp        = bioparams(iv_DDAp        )
    k_po4DDA        = bioparams(ik_po4DDA        )
    mu_pickDDApo4 = bioparams(imu_pickDDApo4)
    zeta_nf_DDA     = bioparams(izeta_nf_DDA     )
    r_excrDDA_1     = bioparams(ir_excrDDA_1     )
    r_excrDDA_n   = bioparams(ir_excrDDA_n   )
    r_excrDDA_2     = bioparams(ir_excrDDA_2     )
    r_pomDDA        = bioparams(ir_pomDDA        )
    mu_UN          = bioparams(imu_UN          )
    alpha_UN       = bioparams(ialpha_UN       )
    k_DOM        = bioparams(ik_DOM        )
    !b_SDONlabi     = bioparams(ib_SDONlabi     )
    !b_SDOPlabi     = bioparams(ib_SDOPlabi     )
    r_SDOM     = bioparams(ir_SDOM     )
    mu_BA          = bioparams(imu_BA          )
    b_BAresp       = bioparams(ib_BAresp       )
    r_BAadju       = bioparams(ir_BAadju       )
    r_BAremi       = bioparams(ir_BAremi       )
    r_BArefr       = bioparams(ir_BArefr       )
    f_BAslct = bioparams(if_BAslct)
    r_BAresp_1 = bioparams(ir_BAresp_1)
    r_BAresp_min = bioparams(ir_BAresp_min)
    r_BAresp_max = bioparams(ir_BAresp_max)
    r_BAmort = bioparams(ir_BAmort)    
    mu_PRT       = bioparams(imu_PRT       )
    g_sp        = bioparams(ig_sp        )
    g_ba        = bioparams(ig_ba        )
    r_PRTex        = bioparams(ir_PRTex        )
    f_exPRTldom    = bioparams(if_exPRTldom    )
    r_PRTresp_1    = bioparams(ir_PRTresp_1    )
    r_PRTresp_2    = bioparams(ir_PRTresp_2    )
    r_PRTadju      = bioparams(ir_PRTadju      )
    r_PRTremi      = bioparams(ir_PRTremi      )
    r_pomPRT       = bioparams(ir_pomPRT       )
    mu_MZ       = bioparams(imu_MZ       )
    g_prt        = bioparams(ig_prt        )
    g_tr         = bioparams(ig_tr         )
    g_dda         = bioparams(ig_dda         )
    r_MZex         = bioparams(ir_MZex         )
    f_exMZldom     = bioparams(if_exMZldom     )
    r_MZresp_1     = bioparams(ir_MZresp_1     )
    r_MZresp_2     = bioparams(ir_MZresp_2     )
    r_MZadju       = bioparams(ir_MZadju       )
    r_MZremi       = bioparams(ir_MZremi       )
    r_MZpom        = bioparams(ir_MZpom        )
!    r_MZrefr       = bioparams(ir_MZrefr       )
    r_MZremv       = bioparams(ir_MZremv       )
    f_HZsdom       = bioparams(if_HZsdom       )
    f_HZpom        = bioparams(if_HZpom        )
!    r_SDOMrefr     = bioparams(ir_SDOMrefr     )
    q_refrDOM_n = bioparams(iq_refrDOM_n) 
    q_refrDOM_p = bioparams(iq_refrDOM_p)
    q_POM_n        = bioparams(iq_POM_n        )
    q_POM_p        = bioparams(iq_POM_p        )
    r_nitrf        = bioparams(ir_nitrf        )
    remin_prf_n    = bioparams(iremin_prf_n    )
    remin_prf_p    = bioparams(iremin_prf_p    )
    wnsvo          = bioparams(iwnsvo          )
    remin          = bioparams(iremin          )
    
    !----------------------------------------------
    ! 15N isotope-related local variables (新增同步)
    !----------------------------------------------

    R_std        = bioparams(iR_std        )
    epsilon_anh4 = bioparams(iepsilon_anh4 )
    epsilon_ano3 = bioparams(iepsilon_ano3 )
    epsilon_zexrt= bioparams(iepsilon_zexrt)
    epsilon_nitrf= bioparams(iepsilon_nitrf)
    epsilon_denit= bioparams(iepsilon_denit)
    epsilon_remin= bioparams(iepsilon_remin)
    epsilon_nf   = bioparams(iepsilon_nf   )
    epsilon_g    = bioparams(iepsilon_g    )
    epsilon_exnh4= bioparams(iepsilon_exnh4)
    epsilon_BAdon= bioparams(iepsilon_BAdon)
    delta_no3    = bioparams(idelta_no3    )
    delta_nh4    = bioparams(idelta_nh4    )
    R_inno3      = bioparams(iR_inno3      )
    R_innh4      = bioparams(iR_innh4      )


    open(unit=ecopar_unit, file=trim(filename), status='replace')
    write(unit=ecopar_unit, nml=ecosys_parms_nml)
    close(unit=ecopar_unit)
    
  end subroutine write_eco_params

end module eco_common
