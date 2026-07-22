!----------------------------------------------------------------------------
!     CVS:$Id: eco_params.F90, added DDA+15N$
!             Yangchun Xu revised, 2026/03/22
!     CVS:$Name:  $
!----------------------------------------------------------------------------

module eco_params

  implicit none


!-------------------------------------------------------------------------
! indices for state variables
!-------------------------------------------------------------------------
  integer, parameter :: iSPc=1, iSPn=2, iSPp=3, &
    iTRc=4, iTRn=5, iTRp=6, &
    iDDAc=7, iDDAn=8, iDDAp=9, &
    iUNc=10, iUNn=11, iUNp=12, &
    iBAc=13, iBAn=14, iBAp=15, &
    iPRTc=16, iPRTn=17, iPRTp=18, &
    iMZc=19, iMZn=20, iMZp=21, &
    iLDOMc=22, iLDOMn=23, iLDOMp=24, &
    iSDOMc=25, iSDOMn=26, iSDOMp=27, &
    iDETc=28, iDETn=29, iDETp=30, &
    iNH4=31, iNO3=32, iPO4=33, iSPchl=34, &
    iTRchl=35, iDDAchl=36, iUNchl=37, &
    iSP15n=38, iTR15n=39, iDDA15n=40, iUN15n=41, &
    iBA15n=42, iPRT15n=43, iMZ15n=44, &
    iLDOM15n=45, iSDOM15n=46, iDET15n=47, &
    i15NH4=48, i15NO3=49

!-------------------------------------------------------------------------
! ***Required*** numstatevar - number of ecosystem scalars.  Should be 
! equal to maximum index above.
!-------------------------------------------------------------------------
  integer, parameter :: numstatevar=49

!-------------------------------------------------------------------------
! indices for diagnostic variables
!-------------------------------------------------------------------------
  integer, parameter :: iPP=1, iprBAc=2, &
				igrowSPc=3, igrowSPnh4=4, igrowSPno3=5, &
				igrowSPn=6, igrowSPp=7, &
				iexcrSP_1c=8, iexcrSP_1n=9, iexcrSP_1p=10, &
				iexcrSP_2c=11, iexcrSP_2n=12, iexcrSP_2p=13, &
				ipomSPc=14, ipomSPn=15, ipomSPp=16, &
				igrazSPc=17, igrazSPn=18, igrazSPp=19, & 
				igrowTRc=20, igrowTRnh4=21, igrowTRno3=22, igrowTRnf=23, &
				igrowTRn=24, igrowTRpo4=25, ipickTRpo4=26, igrowTRp=27, &
				iexcrTR_1c=28, iexcrTR_1n=29, iexcrTR_1p=30, iexcrTR_nh4=31, &
				iexcrTR_2c=32, iexcrTR_2n=33, iexcrTR_2p=34, &
				ipomTRc=35, ipomTRn=36, ipomTRp=37, &
				igrazTRc=38, igrazTRn=39, igrazTRp=40, &
                                  igrowDDAc=41, igrowDDAnh4=42, igrowDDAno3=43, &
                          	igrowDDAnf=44, igrowDDAn=45, igrowDDApo4=46, &
                          	ipickDDApo4=47, igrowDDAp=48, iexcrDDA_1c=49, &         
                          	iexcrDDA_1n=50, iexcrDDA_1p=51, iexcrDDA_nh4=52, &
				iexcrDDA_2c=53, iexcrDDA_2n=54, iexcrDDA_2p=55, &
				ipomDDAc=56, ipomDDAn=57, ipomDDAp=58, &
				igrazDDAc=59, igrazDDAn=60, igrazDDAp=61, &
				igrowUNc=62, igrowUNnh4=63, igrowUNno3=64, igrowUNnf=65, &
				igrowUNn=66, igrowUNp=67, &
			  	iexcrUN_1c=68, iexcrUN_1n=69, iexcrUN_1p=70, &
                           	iexcrUN_nh4=71, iexcrUN_2c=72, iexcrUN_2n=73, &
                           	iexcrUN_2p=74, ipomUNc=75, ipomUNn=76, ipomUNp=77, &
				igrazUNc=78, igrazUNn=79, igrazUNp=80, &
				igrowBAldoc=81, igrowBAldon=82, igrowBAldop=83, &
				igrowBAsdoc=84, igrowBAsdon=85, igrowBAsdop=86, &
				igrowBAnh4=87, igrowBAno3=88, igrowBApo4=89, &
				igrowBAc=90, igrowBAn=91, igrowBAp=92, irespBA=93, &
				irefrBAc=94, irefrBAn=95, irefrBAp=96, &
				iexcrBAc=97, iexcrBAn=98, iexcrBAp=99, &
				iremiBAn=100, iremiBAp=101, &
				igrazBAc=102, igrazBAn=103, igrazBAp=104, &
				imortBAc=105, imortBAn=106, imortBAp=107, &
				ifluxBAnh4=108, ifluxBApo4=109, &
				igrowPRTc=110, igrowPRTn=111, igrowPRTp=112, irespPRT=113, &
				iexcrPRTldomc=114, iexcrPRTldomn=115, iexcrPRTldomp=116, &
				iexcrPRTsdomc=117, iexcrPRTsdomn=118, iexcrPRTsdomp=119, &
				iexcrPRTsdom2c=120, iexcrPRTsdom2n=121, &
                          	iexcrPRTsdom2p=122, iremiPRTn=123, &
				iremiPRTp=124, ipomPRTc=125, &
				ipomPRTn=126, ipomPRTp=127, &
				igrazPRTc=128, igrazPRTn=129, igrazPRTp=130, &
				igrowMZc=131, igrowMZn=132, igrowMZp=133, &
				irespSP=134, irespTR=135, irespDDA=136, &
                          	irespUN=137, irespMZ=138, &
				iexcrMZldomc=139, iexcrMZldomn=140, iexcrMZldomp=141, &
				iexcrMZsdomc=142, iexcrMZsdomn=143, iexcrMZsdomp=144, &
				iexcrMZsdom2c=145, iexcrMZsdom2n=146, iexcrMZsdom2p=147, &
				iremiMZn=148, iremiMZp=149, ipomMZc=150, ipomMZn=151, &
                           	ipomMZp=152, iremvMZc=153, iremvMZn=154, iremvMZp=155, &
				ipomHZc=156, ipomHZn=157, ipomHZp=158, &
				iexcrHZsdomc=159, iexcrHZsdomn=160, iexcrHZsdomp=161, &
				iremiHZn=162, iremiHZp=163, &
				idisDETc=164, idisDETn=165, idisDETp=166, &
				initrf=167, iexportc=168, iexportn=169, iexportp=170, & 
                          !===================================================== 
                          ! 新增15N相关的诊断通量
                          !=====================================================
                         	 igrowSP15nh4=171, igrowSP15no3=172, &
                          iexcrSP_1_15n=173, iexcrSP_2_15n=174, &
                          ipomSP15n=175, igrazSP15n=176, &
                          igrowTR15nh4=177, igrowTR15no3=178,&
                          igrowTR15nf=179, iexcrTR_1_15n=180, &
                          iexcrTR_2_15n=181, iexcrTR_15nh4=182, &
                          ipomTR15n=183, igrazTR15n=184, &
                          igrowDDA15nh4=185, igrowDDA15no3=186, &
                          igrowDDA15nf=187, iexcrDDA_1_15n=188, &
                          iexcrDDA_2_15n=189, iexcrDDA_15nh4=190, & 
                          ipomDDA15n=191, igrazDDA15n=192, & 
                          igrowUN15nh4=193, igrowUN15no3=194, & 
                          igrowUN15nf=195, iexcrUN_1_15n=196, &
                          iexcrUN_2_15n=197, iexcrUN_15nh4=198, &
                          ipomUN15n=199, igrazUN15n=200, &
                          igrowBA15ldon=201, igrowBA15sdon=202, &
                          igrowBA15nh4=203, igrowBA15no3=204, & 
                          igrowBA15n=205, irefrBA15n=206, iexcrBA15n=207, &
                          iremiBA15n=208, igrazBA15n=209, &
                          imortBA15n=210, ifluxBA15nh4=211, &
                          igrowPRT15n=212, iexcrPRTldom15n=213, &
                          iexcrPRTsdom15n=214, iexcrPRTsdom2_15n=215, &
                          iremiPRT15n=216, ipomPRT15n=217, igrazPRT15n=218, & 
                          igrowMZ15n=219, iexcrMZldom15n=220, &
                          iexcrMZsdom15n=221, iexcrMZsdom2_15n=222, &
                          iremiMZ15n=223, ipomMZ15n=224, iremvMZ15n=225, &
                          ipomHZ15n=226, iexcrHZsdom15n=227, iremiHZ15n=228, &
                          idisDET15n=229, initrf15=230, iexport15n=231,&
                          igrowSP15n=232, igrowTR15n=233, igrowDDA15n=234,igrowUN15n=235
                                                              
!-------------------------------------------------------------------------
! ***Required*** numdiagvar - number of diagnostic variables.  
! Must be equal to the maximum index above
!-------------------------------------------------------------------------
  integer, parameter :: numdiagvar=235

!-------------------------------------------------------------------------
! indices for ecosystem parameters
!-------------------------------------------------------------------------
  integer, parameter :: &
       iae          = 1, &
       imu_SP          = iae    + 1, &
       ialpha_SP       = imu_SP          + 1, &
       ia_SP       = ialpha_SP          + 1, &
       iv_SPn     = ia_SP             + 1, &
       ik_nh4SP        = iv_SPn       + 1, &
       ik_no3SP        = ik_nh4SP        + 1, &
       iv_SPp            = ik_no3SP        + 1, &
       ik_po4SP        = iv_SPp        + 1, &
       izeta               = ik_po4SP     + 1, &
       itheta             = izeta        + 1, &
       ir_excrSP_1     = itheta        + 1, &
       ir_excrSP_2     = ir_excrSP_1     + 1, &
       ir_pomSP        = ir_excrSP_2     + 1, &
       imu_TR          = ir_pomSP        + 1, &
       ialpha_TR       = imu_TR          + 1, &
       ia_TR       = ialpha_TR         + 1, &
       iv_TRn     = ia_TR              + 1, &
       ik_nh4TR        = iv_TRn       + 1, &
       ik_no3TR        = ik_nh4TR        + 1, &
       iv_TRp             = ik_no3TR       + 1, &
       ik_po4TR        = iv_TRp        + 1, &
       imu_pickTRpo4 = ik_po4TR + 1, &
       izeta_nf            = imu_pickTRpo4        + 1, &
       ir_excrTR_1     = izeta_nf        + 1, &
       ir_excrTR_n   = ir_excrTR_1     + 1, &
       ir_excrTR_2     = ir_excrTR_n   + 1, &
       ir_pomTR        = ir_excrTR_2     + 1, &
       imu_DDA          = ir_pomTR        + 1, &
       ialpha_DDA       = imu_DDA          + 1, &
       ia_DDA       = ialpha_DDA         + 1, &
       iv_DDAn     = ia_DDA              + 1, &
       ik_nh4DDA        = iv_DDAn       + 1, &
       ik_no3DDA        = ik_nh4DDA        + 1, &
       iv_DDAp             = ik_no3DDA       + 1, &
       ik_po4DDA        = iv_DDAp        + 1, &
       imu_pickDDApo4 = ik_po4DDA + 1, &
       izeta_nf_DDA            = imu_pickDDApo4        + 1, &
       ir_excrDDA_1     = izeta_nf_DDA        + 1, &
       ir_excrDDA_n   = ir_excrDDA_1     + 1, &
       ir_excrDDA_2     = ir_excrDDA_n   + 1, &
       ir_pomDDA        = ir_excrDDA_2     + 1, &
       imu_UN          = ir_pomDDA        + 1, &
       ialpha_UN       = imu_UN          + 1, &
       ik_DOM        = ialpha_UN        + 1, &
       ir_SDOM       = ik_DOM + 1, &
       imu_BA          = ir_SDOM     + 1, &
       ib_BAresp       = imu_BA       + 1, &
       ir_BAadju       = ib_BAresp       + 1, &
       ir_BAremi       = ir_BAadju       + 1, &
       ir_BArefr       = ir_BAremi       + 1, &
       if_BAslct = ir_BArefr + 1, &
       ir_BAresp_1 = if_BAslct + 1, &
       ir_BAresp_min = ir_BAresp_1 + 1, &
       ir_BAresp_max = ir_BAresp_min  + 1, &
       ir_BAmort = ir_BAresp_max  + 1, &
       imu_PRT       = ir_BAmort        + 1, &
       ig_sp        = imu_PRT       + 1, &
       ig_ba        = ig_sp       + 1, &
       ir_PRTex        = ig_ba        + 1, &
       if_exPRTldom    = ir_PRTex      + 1, &
       ir_PRTresp_1    = if_exPRTldom    + 1, &
       ir_PRTresp_2    = ir_PRTresp_1    + 1, &
       ir_PRTadju      = ir_PRTresp_2    + 1, &
       ir_PRTremi      = ir_PRTadju      + 1, &
       ir_pomPRT       = ir_PRTremi      + 1, &
       imu_MZ       = ir_pomPRT       + 1, &
       ig_prt        = imu_MZ       + 1, &
       ig_tr         = ig_prt        + 1, &
       ig_dda         = ig_tr        + 1, &
       ir_MZex         = ig_dda     + 1, &
       if_exMZldom     = ir_MZex         + 1, &
       ir_MZresp_1     = if_exMZldom     + 1, &
       ir_MZresp_2     = ir_MZresp_1     + 1, &
       ir_MZadju       = ir_MZresp_2     + 1, &
       ir_MZremi       = ir_MZadju       + 1, &
       ir_MZpom        = ir_MZremi       + 1, &
       ir_MZremv       = ir_MZpom        + 1, &
       if_HZsdom       = ir_MZremv       + 1, &
       if_HZpom        = if_HZsdom       + 1, &
       iq_refrDOM_n = if_HZpom        + 1, &
       iq_refrDOM_p = iq_refrDOM_n + 1, &
       iq_POM_n        = iq_refrDOM_p     + 1, &
       iq_POM_p        = iq_POM_n        + 1, &
       ir_nitrf        = iq_POM_p        + 1, &
       iremin_prf_n    = ir_nitrf        + 1, &
       iremin_prf_p    = iremin_prf_n    + 1, &
       iwnsvo    = iremin_prf_p    + 1, &
       iremin          = iwnsvo    + 1, &
       !==============================================
       ! 新增15N相关的生态系统参数
       !==============================================
       iR_std         = iremin    + 1, &  ! standard 14N/15N
       iepsilon_anh4    = iR_std    + 1, &  ! fraction of no3 anmonium
       iepsilon_ano3  = iepsilon_anh4 + 1, & ! fraction of no3 assimilation 
       iepsilon_zexrt  = iepsilon_ano3 + 1, &  ! fraction of zooplankton excretion
       iepsilon_nitrf  = iepsilon_zexrt + 1, & !fraction of nitrification   
       iepsilon_denit  = iepsilon_nitrf + 1, &  ! fraction of denitrification
       iepsilon_remin  = iepsilon_denit + 1, &     ! fraction of remineralization
       iepsilon_nf  = iepsilon_remin + 1, &  ! fraction of n2 fixation
       iepsilon_g  = iepsilon_nf + 1, &  ! fraction of grazing
       iepsilon_exnh4  = iepsilon_g + 1, &  ! fraction of nh4 excretion
       iepsilon_BAdon  = iepsilon_exnh4 + 1, &  !  fraction of DON utilization of BA
       idelta_no3  = iepsilon_BAdon + 1, &  !  delta of NO3
       idelta_nh4  = idelta_no3 + 1, &  !  delta of NO3
       iR_inno3= idelta_nh4  +1, &     ! initial R of NO3
       iR_innh4= iR_inno3  +1      ! initial R of NH4
!-------------------------------------------------------------------------
! ***Required*** nparams_bio - number of optimizable parameters.  
! Should be equal to largest index above or total number of parameters
!-------------------------------------------------------------------------
  integer, parameter :: nparams_bio= iR_innh4

!-------------------------------------------------------------------------
! ***Required*** param_names - names of parameters, will be used as column
! headers is ASCII output.
!-------------------------------------------------------------------------
  character(len=20), dimension(nparams_bio), parameter :: param_names = (/ &
       "ae             ", &
       "mu_SP          ", &
       "alpha_SP       ", &
       "a_SP           ", &
       "v_SPn          ", &
       "k_nh4SP        ", &
       "k_no3SP        ", &
       "v_SPp          ", &
       "k_po4SP        ", &
       "zeta           ", &
       "theta          ", &
       "r_excrSP_1     ", &
       "r_excrSP_2     ", &
       "r_pomSP        ", &
       "mu_TR          ", &
       "alpha_TR       ", &
       "a_TR           ", &
       "v_TRn          ", &
       "k_nh4TR        ", &
       "k_no3TR        ", &
       "v_TRp         ", &
       "k_po4TR        ", &
       "mu_pickTRpo4   ", &
       "zeta_nf        ", &
       "r_excrTR_1     ", &
       "r_excrTR_n     ", &
       "r_excrTR_2     ", &
       "r_pomTR        ", &
       "mu_DDA        ", &
       "alpha_DDA       ", &
       "a_DDA           ", &
       "v_DDAn          ", &
       "k_nh4DDA        ", &
       "k_no3DDA        ", &
       "v_DDAp          ", &
       "k_po4DDA        ", &
       "mu_pickDDApo4   ", &
       "zeta_nf_DDA      ", &
       "r_excrDDA_1     ", &
       "r_excrDDA_n     ", &
       "r_excrDDA_2     ", &
       "r_pomDDA       ", &
       "mu_UN          ", &
       "alpha_UN       ", &
       "k_DOM          ", &
       !"b_SDONlabi     ", &
       !"b_SDOPlabi     ", &
       "r_SDOM         ", &
       "mu_BA          ", &
       "b_BAresp       ", &
       "r_BAadju       ", &
       "r_BAremi       ", &
       "r_BArefr       ", &
       "f_BAslct       ", &
       "r_BAresp_1     ", &
       "r_BAresp_min   ", &
       "r_BAresp_max   ", &
       "r_BAmort       ", &
       "mu_PRT         ", &
       "g_sp           ", &
       "g_ba           ", &
       "r_PRTex        ", &
       "f_exPRTldom    ", &
       "r_PRTresp_1    ", &
       "r_PRTresp_2    ", &
       "r_PRTadju      ", &
       "r_PRTremi      ", &
       "r_pomPRT       ", &
       "mu_MZ          ", &
       "g_prt          ", &
       "g_tr           ", &
       "g_dda          ", &
       "r_MZex         ", &
       "f_exMZldom     ", &
       "r_MZresp_1     ", &
       "r_MZresp_2     ", &
       "r_MZadju       ", &
       "r_MZremi       ", &
       "r_MZpom        ", &
!       "r_MZrefr       ", &
       "r_MZremv       ", &
       "f_HZsdom       ", &
       "f_HZpom        ", &
!       "r_SDOMrefr     ", &
       "q_refrDOM_n    ", &
       "q_refrDOM_p    ", &
       "q_POM_n        ", &
       "q_POM_p        ", &
       "r_nitrf        ", &
       "remin_prf_n    ", &
       "remin_prf_p    ", &
       "wnsvo          ", &
       "remin          ",&
       "R_std          ",&
       "epsilon_anh4  ",&
       "epsilon_ano3  ",&
       "epsilon_zexrt ",&
       "epsilon_nitrf  ",&
       "epsilon_denit ",&
       "epsilon_remin ",&
       "epsilon_nf   ",&
       "epsilon_g   ",&
       "epsilon_exnh4 ",&
       "epsilon_BAdon ",&
       "delta_no3    ",&
       "delta_nh4   ",&
       "R_inno3    ",&
       "R_innh4    "/)

!-------------------------------------------------------------------------
! ***Required*** nparams_opt - number of optimizable parameters.
!-------------------------------------------------------------------------
! full set
! integer, parameter :: nparams_opt=nparams_bio
! limited set
 integer, parameter :: nparams_opt=16
!-------------------------------------------------------------------------
! ***Required*** bio_opt_map - set of biological parameters 
! which are subject to optimization.  Must be subset of full set 
! listed above.
!-------------------------------------------------------------------------
 integer, dimension(nparams_opt), parameter :: bio_opt_map=(/ &
!       iae, & 
        imu_SP, & 
        ialpha_SP, & 
!       ia_SP, &
!       iv_SPn, & 
!       ik_nh4SP, &
!       ik_no3SP, & 
        iv_SPp, &
!       ik_po4SP, &
!       izeta, &
        itheta, &
!       ir_excrSP_1, &
!       ir_excrSP_2, &
!       ir_pomSP, &
!       imu_TR, &
!       ialpha_TR, &
!       ia_TR, &
!       iv_TRn, &
        ik_nh4TR, &
        ik_no3TR, &
!       iv_TRp, &
!       ik_po4TR, &
!       imu_pickTRpo4, &
!       izeta_nf, &
!       ir_excrTR_1, &
!       ir_excrTR_n, &
!       ir_excrTR_2, &
!       ir_pomTR, &
!       imu_DDA, &
!       ialpha_DDA, &
!       ia_DDA, &
!       iv_DDAn, &
        ik_nh4DDA, &
        ik_no3DDA, &
!       iv_DDAp, &
!       ik_po4DDA, &
!       imu_pickDDApo4, &
!       izeta_nf_DDA, &
!       ir_excrDDA_1, &
!       ir_excrDDA_n, &
!       ir_excrDDA_2, &
!       ir_pomDDA, &
        imu_UN, &
!       ialpha_UN, &
!       ik_DOM, &
!       ib_SDONlabi, &
!       ib_SDOPlabi, & 
        ir_SDOM, &
!       imu_BA, &
!       ib_BAresp, &
!       ir_BAadju, &
!       ir_BAremi, &
!       ir_BArefr, &
!       if_BAslct, &
!       ir_BAresp_1, &
!       ir_BAresp_min, &
        ir_BAresp_max, &
!       ir_BAmort, &
        imu_PRT, &
!       ig_sp, & 
!       ig_ba, &
!       ir_PRTex, &
!       if_exPRTldom, &
!       ir_PRTresp_1, &
        ir_PRTresp_2, &
!       ir_PRTadju, &
!       ir_PRTremi, &
!       ir_pomPRT, &
!       imu_MZ, &
!       ig_prt, &
        ig_tr, &
        ig_dda, &
!       ir_MZex, &
!       if_exMZldom, &
!       ir_MZresp_1, &
!       ir_MZresp_2, &
!       ir_MZadju, &
!       ir_MZremi, &
!       ir_MZpom, &
!       ir_MZrefr, &
!       ir_MZremv, &
!       if_HZsdom, &
!       if_HZpom, &
!       ir_SDOMrefr, &
!       iq_refrDOM_n, &
!       iq_refrDOM_p, &
!       iq_POM_n, &
!       iq_POM_p, &
!       ir_nitrf, &
!       iremin_prf_n, &
!       iremin_prf_p, &
        iwnsvo /) 
!       iremin /)




! mwC              molecular weight of Carbon (g/mol)
! mwN              molecular weight of Nitrogen (g/mol)
! mwN              molecular weight of Phosphorus (g/mol)
  double precision, parameter :: mwC=12.0,mwN=14.0,mwP=31


! filenames for initialization files.  Order must match first NumStateVar
! indices above
  character(len=*),dimension(NumStateVar), parameter :: &
       fname_bio_suffix=(/ &
       'SPc  ', &
       'SPn  ', &
       'SPp  ', &
       'TRc  ', &
       'TRn  ', &
       'TRp  ', &
       'DDAc ', &
       'DDAn ', &
       'DDAp ', &
       'UNc  ', &
       'UNn  ', &
       'UNp  ', &   
       'BAc  ', &
       'BAn  ', &
       'BAp  ', &
       'PRTc ', &
       'PRTn ', &
       'PRTp ', &
       'MZc  ', &
       'MZn  ', &
       'MZp  ', &
       'LDOMc', &
       'LDOMn', &
       'LDOMp', &
       'SDOMc', &
       'SDOMn', &
       'SDOMp', &
       'DETc ', &
       'DETn ', &
       'DETp ', &
       'NH4  ', &
       'NO3  ', &
       'PO4  ', &
       'SPchl', &
       'TRchl', &
       'DDAchl', &
       'UNchl', &
       'SP15n ', &
       'TR15n ', &
       'DDA15n', &
       'UN15n ', &
       'BA15n ', &
       'PRT15n', &
       'MZ15n ', & 
       'LDOM15n', &
       'SDOM15n', &
       'DET15n', &
       '15NH4 ', &
       '15NO3 '/) 
       
  contains

  subroutine get_opt_param_names(opt_param_names)
    implicit none

    character(len=20), dimension(nparams_opt), intent(out) :: opt_param_names
    integer :: i	

    do i=1,nparams_opt
       opt_param_names(i) = param_names(bio_opt_map(i))
    end do
    
  end subroutine get_opt_param_names
 
end module eco_params
