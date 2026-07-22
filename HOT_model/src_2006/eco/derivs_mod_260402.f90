!----------------------------------------------------------------------------
! CVS: $Id: derivs_mod.F90, added DDA+15N $
!     Yangchun Xu 2026/03/19
! CVS: $Name: $
!----------------------------------------------------------------------------

module derivs_mod

contains

  subroutine derivs(y,dydtt,dydtt_diag,istep,iz,bioparams)

    use const,  only: c0,  c1,  SecPerDay
    use eco_common, only: rlt, wnsvflag
    use eco_params, only: iae, imu_SP, ialpha_SP, ia_SP, &
        iv_SPn, ik_nh4SP, ik_no3SP,iv_SPp,ik_po4SP,izeta, & 
        itheta, ir_excrSP_1,ir_excrSP_2,ir_pomSP,imu_TR, &
        ialpha_TR,ia_TR,iv_TRn,ik_nh4TR,ik_no3TR,iv_TRp, &
        ik_po4TR,imu_pickTRpo4, izeta_nf, &
        ir_excrTR_1,ir_excrTR_n,ir_excrTR_2,ir_pomTR, &
        imu_DDA,ialpha_DDA,ia_DDA, &
        iv_DDAn,ik_nh4DDA,ik_no3DDA, &
        iv_DDAp,ik_po4DDA,imu_pickDDApo4, &
        izeta_nf_DDA,ir_excrDDA_1,ir_excrDDA_n, &
        ir_excrDDA_2,ir_pomDDA,imu_UN, &
        ialpha_UN,ir_SDOM,ik_DOM,imu_BA, &
        ib_BAresp,ir_BAadju,ir_BAremi,ir_BArefr, &
        if_BAslct,ir_BAresp_1, ir_BAresp_min, &
        ir_BAresp_max,ir_BAmort, imu_PRT,ig_sp,ig_ba,&
        ir_PRTex,if_exPRTldom,ir_PRTresp_1, &
        ir_PRTresp_2,ir_PRTadju,ir_PRTremi, &
        ir_pomPRT,imu_MZ,ig_prt,ig_tr,ig_dda, &
        ir_MZex,if_exMZldom,ir_MZresp_1,ir_MZresp_2, & 
        ir_MZadju,ir_MZremi,ir_MZpom,ir_MZremv, if_HZsdom,if_HZpom, &
        iq_refrDOM_n, iq_refrDOM_p,iq_POM_n,iq_POM_p, &
        ir_nitrf,iremin_prf_n,iremin_prf_p,iwnsvo,iremin, &
        iR_std, iepsilon_anh4, iepsilon_ano3, iepsilon_zexrt, &
        iepsilon_nitrf, iepsilon_denit, iepsilon_remin, &
        iepsilon_nf, iepsilon_g, iepsilon_exnh4, &
        iepsilon_BAdon, idelta_no3, idelta_nh4, &
        iR_inno3, iR_innh4     

use eco_params, only: NumStateVar, NumDiagVar
use eco_params, only: iSPc,iSPn, iSPp, iTRc, iTRn, iTRp, &
                iDDAc, iDDAn, iDDAp,iUNc,iUNn,iUNp,iBAc,&
                iBAn,iBAp,iPRTc,iPRTn,iPRTp,iMZc,iMZn,iMZp,iLDOMc,&
                iLDOMn,iLDOMp,iSDOMc,iSDOMn,iSDOMp,iDETc,iDETn,&
                iDETp,iNH4,iNO3,iPO4,iSPchl,iTRchl,iDDAchl,iUNchl

use eco_params, only: iPP, iprBAc, &
                   igrowSPc, igrowSPnh4, igrowSPno3, &
                   igrowSPn, igrowSPp, &
                   iexcrSP_1c, iexcrSP_1n, iexcrSP_1p, &
                   iexcrSP_2c, iexcrSP_2n, iexcrSP_2p, &
                   ipomSPc, ipomSPn, ipomSPp, &
                   igrazSPc, igrazSPn, igrazSPp, & 
                   igrowTRc, igrowTRnh4, igrowTRno3, igrowTRnf, &
                   igrowTRn, igrowTRpo4, ipickTRpo4, igrowTRp, &
                   iexcrTR_1c, iexcrTR_1n, iexcrTR_1p, iexcrTR_nh4, &
                   iexcrTR_2c, iexcrTR_2n, iexcrTR_2p, &
                   ipomTRc, ipomTRn, ipomTRp, &
                   igrazTRc, igrazTRn, igrazTRp, &
                   igrowDDAc, igrowDDAnh4, igrowDDAno3, igrowDDAnf, &
                   igrowDDAn, igrowDDApo4, ipickDDApo4, igrowDDAp, &
                   iexcrDDA_1c, iexcrDDA_1n, iexcrDDA_1p,  &
                   iexcrDDA_nh4,iexcrDDA_2c, iexcrDDA_2n,  &
                   iexcrDDA_2p,ipomDDAc, ipomDDAn, ipomDDAp, &
                   igrazDDAc, igrazDDAn, igrazDDAp, &
                   igrowUNc, igrowUNnh4, igrowUNno3, &
                   igrowUNnf, igrowUNn, igrowUNp, &
                   iexcrUN_1c, iexcrUN_1n, iexcrUN_1p,  &
                   iexcrUN_nh4,iexcrUN_2c, iexcrUN_2n,  &
                   iexcrUN_2p,ipomUNc, ipomUNn, ipomUNp, &
                   igrazUNc, igrazUNn, igrazUNp,igrowBAldoc, &
                   igrowBAldon, igrowBAldop,igrowBAsdoc,  &
                   igrowBAsdon, igrowBAsdop,igrowBAnh4, &
                   igrowBAno3, igrowBApo4,igrowBAc, &
                   igrowBAn, igrowBAp, irespBA, &
                   irefrBAc, irefrBAn, irefrBAp, &
                   iexcrBAc, iexcrBAn, iexcrBAp, &
                   iremiBAn, iremiBAp, igrazBAc,&
                   igrazBAn, igrazBAp, imortBAc,&
                   imortBAn, imortBAp,ifluxBAnh4, ifluxBApo4,  &
                   igrowPRTc, igrowPRTn, igrowPRTp, irespPRT, &
                   iexcrPRTldomc, iexcrPRTldomn, iexcrPRTldomp, &
                   iexcrPRTsdomc, iexcrPRTsdomn, iexcrPRTsdomp, &
                   iexcrPRTsdom2c, iexcrPRTsdom2n,  &
                   iexcrPRTsdom2p,iremiPRTn, iremiPRTp, &
                   ipomPRTc, ipomPRTn, ipomPRTp, &
                   igrazPRTc, igrazPRTn, igrazPRTp, &
                   igrowMZc, igrowMZn, igrowMZp,irespMZ, &
                   iexcrMZldomc, iexcrMZldomn, iexcrMZldomp, &
                   iexcrMZsdomc, iexcrMZsdomn, iexcrMZsdomp, &
                   iexcrMZsdom2c, iexcrMZsdom2n, iexcrMZsdom2p, &
                   iremiMZn, iremiMZp,ipomMZc, ipomMZn,  &
                   !irefrMZc, irefrMZn, irefrMZp, &
                   ipomMZp, iremvMZc, iremvMZn, iremvMZp, &
                   ipomHZc, ipomHZn, ipomHZp,iexcrHZsdomc,  &
                   iexcrHZsdomn, iexcrHZsdomp, iremiHZn, &
                   iremiHZp,idisDETc, idisDETn, idisDETp, &
                   initrf, iexportc, iexportn, iexportp, &
                   irespSP, irespTR, irespDDA, irespUN 
! 15N 状态变量索引 - 映射由外部参数文件定义 
use eco_params, only: iSP15n, iTR15n, iDDA15n, iUN15n, iBA15n, &
        iPRT15n, iMZ15n, iLDOM15n, iSDOM15n, iDET15n, i15NH4, i15NO3
! 15N 诊断变量索引
use eco_params, only: igrowSP15nh4, igrowSP15no3, igrowSP15n, &
        iexcrSP_1_15n, iexcrSP_2_15n, ipomSP15n, igrazSP15n, &
        igrowTR15nh4, igrowTR15no3, igrowTR15nf, &
        igrowTR15n, iexcrTR_1_15n, iexcrTR_15nh4, &
        iexcrTR_2_15n, ipomTR15n, igrazTR15n, &
        igrowDDA15nh4, igrowDDA15no3, igrowDDA15nf, &
        igrowDDA15n, iexcrDDA_1_15n, iexcrDDA_15nh4, &
        iexcrDDA_2_15n, ipomDDA15n, igrazDDA15n, &
        igrowUN15nh4, igrowUN15no3, igrowUN15nf, &
        igrowUN15n, iexcrUN_1_15n, iexcrUN_15nh4, &
        iexcrUN_2_15n, ipomUN15n, igrazUN15n, &
        igrowBA15ldon, igrowBA15sdon, igrowBA15nh4, &
        igrowBA15no3, igrowBA15n, irefrBA15n, &
iexcrBA15n, iremiBA15n, igrazBA15n, imortBA15n, &
        ifluxBA15nh4, igrowPRT15n, iexcrPRTldom15n, &
        iexcrPRTsdom15n, iexcrPRTsdom2_15n, iremiPRT15n, &
        ipomPRT15n, igrazPRT15n, igrowMZ15n, iexcrMZldom15n, &
        iexcrMZsdom15n, iexcrMZsdom2_15n, iremiMZ15n, ipomMZ15n, &
        iremvMZ15n, ipomHZ15n, iexcrHZsdom15n, &
        iremiHZ15n, idisDET15n, initrf15, iexport15n

use forcing, only: Tdat
use grid, only: nz, dzt

    implicit none

!-----------------------------------------------------------------------
!  Yawei Luo's microbial-loop and N2-fixation model
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!     Arguments
!-----------------------------------------------------------------------

    double precision, dimension(:) :: y, dydtt,dydtt_diag
    integer :: istep,iz
    double precision, dimension(:) :: bioparams

!-----------------------------------------------------------------------
!     Local variables
!-----------------------------------------------------------------------
! ecosystem model parameters
    double precision :: temp,temp1
    double precision :: ae,mu_SP,alpha_SP,a_SP,v_SPn,k_nh4SP,k_no3SP,&
      v_SPp,k_po4SP, zeta, theta,r_excrSP_1,r_excrSP_2,r_pomSP,&
      mu_TR,alpha_TR,a_TR,k_nh4TR,v_TRn,k_no3TR,&
      v_TRp, k_po4TR,mu_pickTRpo4, zeta_nf, &
      r_excrTR_1,r_excrTR_n,r_excrTR_2,r_pomTR,&
      mu_DDA,alpha_DDA,a_DDA,k_nh4DDA,v_DDAn,k_no3DDA,&
      v_DDAp, k_po4DDA,mu_pickDDApo4, zeta_nf_DDA, &
      r_excrDDA_1,r_excrDDA_n,r_excrDDA_2,r_pomDDA,&
      mu_UN,alpha_UN,r_SDOM,k_DOM,mu_BA,& 
      b_BAresp,r_BAadju,r_BAremi,r_BArefr, &
      f_BAslct,r_BAresp_1, r_BAresp_min,r_BAresp_max,&
      r_BAmort, mu_PRT,g_sp,g_ba,r_PRTex,f_exPRTldom,&
      r_PRTresp_1,r_PRTresp_2,r_PRTadju,r_PRTremi,&
      r_pomPRT,mu_MZ,g_prt,g_tr, g_dda,r_MZex,&
      f_exMZldom,r_MZresp_1,r_MZresp_2,r_MZadju,r_MZremi,&
      r_MZpom,r_MZremv,f_HZsdom,f_HZpom,q_refrDOM_n,q_refrDOM_p,&
      !r_MZrefr,r_SDOMrefr,
      q_POM_n,q_POM_p,r_nitrf,remin_prf_n,remin_prf_p,wnsvo,remin,&
      R_std, epsilon_anh4, epsilon_ano3, epsilon_zexrt,&
      epsilon_nitrf, epsilon_denit, epsilon_remin,&
      epsilon_nf, epsilon_g, epsilon_exnh4, &
      epsilon_BAdon, delta_no3, delta_nh4,&
      R_inno3, R_innh4

     
     ! local fixed parameters
      double precision :: q_SP_min_n, q_SP_min_p, q_SP_max_n, &
      q_SP_max_p, q_SP_rdf_n, q_SP_rdf_p,v_UNn,v_UNp, &
      q_UN_min0_n,q_UN_min0_p, q_UN_max0_n,k_nh4UN, &
      k_no3UN, k_po4UN, r_excrUN_1,r_excrUN_n,r_excrUN_2, &
      r_pomUN, q_BA_n,q_BA_p,g_un,q_PRT_n,q_PRT_p, &
      q_MZ_n,q_MZ_p,Tref,a_UN

   ! local variables for temperature effect function
    double precision :: Tfunc
    ! local variables in the SP module
    double precision :: Nfunc_sp_n, Nfunc_sp_p, Pc_SPmax, growSPnh4, growSPno3
    double precision :: V_SPmax_n, V_SPmax_p, respSP, &
            growSPchl, grazSPchl, pomSPchl
    double precision :: growSPc,growSPn,growSPp,excrSP_1c,excrSP_1n,excrSP_1p
    double precision :: excrSP_2c,excrSP_2n,excrSP_2p,pomSPc,pomSPn,pomSPp
    double precision :: grazSPc,grazSPn,grazSPp
    ! local variables in the TR module
    double precision :: q_TR_min_n,q_TR_min_p, &
                        q_TR_max_n,q_TR_max_p, &
                        q_TR_rdf_n,q_TR_rdf_p, &
                        growTRnh4,growTRno3,growTRnf,maxTRnf, &
                        excrTR_nh4,growTRpo4,pickTRpo4
double precision :: growTRc,growTRn,growTRp, &  
                excrTR_1c,excrTR_1n,excrTR_1p,&
                excrTR_2c,excrTR_2n,excrTR_2p, &
                pomTRc,pomTRn,pomTRp,grazTRc,&
                grazTRn,grazTRp
double precision ::  Nfunc_tr_n, Nfunc_tr_p, Pc_TRmax,&
                    V_TRmax_n, V_TRmax_p, &
                    respTR, growTRchl, grazTRchl, pomTRchl

! local variables in the DDA module
    double precision :: q_DDA_min_n,q_DDA_min_p, &
                        q_DDA_max_n,q_DDA_max_p, &
                        q_DDA_rdf_n,q_DDA_rdf_p,growDDAnh4, &
                        growDDAno3,growDDAnf,maxDDAnf, &
                        excrDDA_nh4,growDDApo4,pickDDApo4
    double precision :: growDDAc,growDDAn,growDDAp, &
                    excrDDA_1c,excrDDA_1n,excrDDA_1p,excrDDA_2c, &
                    excrDDA_2n,excrDDA_2p,pomDDAc,pomDDAn, &
                    pomDDAp,grazDDAc,grazDDAn,grazDDAp
    double precision ::  Nfunc_dda_n, Nfunc_dda_p, Pc_DDAmax, &
                    V_DDAmax_n, V_DDAmax_p, &
                    respDDA, growDDAchl, grazDDAchl, pomDDAchl

    ! local variables in the UN module
    double precision :: q_UN_min_n,q_UN_min_p, &
                        q_UN_max_n,q_UN_max_p, &
                        q_UN_rdf_n,q_UN_rdf_p, &
                        growUNnh4,growUNno3,growUNnf,&
                        maxUNnf, excrUN_nh4
    double precision :: growUNc,growUNn,growUNp, &
                excrUN_1c,excrUN_1n,excrUN_1p,excrUN_2c, &
                excrUN_2n,excrUN_2p, pomUNc,pomUNn,pomUNp, &
                grazUNc,grazUNn,grazUNp
    double precision ::  Nfunc_un_n, Nfunc_un_p, Pc_UNmax, &
                V_UNmax_n, V_UNmax_p, respUN, &
                growUNchl, grazUNchl, pomUNchl
    ! local variables in the BA module
    double precision :: ALC,ASC,Nfunc_ba_n,Nfunc_ba_p,&
                growBAldoc,growBAldon,growBAldop,&
                growBAsdoc,growBAsdon,growBAsdop,&
                growBAnh4,growBAno3,growBApo4,respBA,&
                fluxBAnh4,fluxBAno3,fluxBApo4, &
                growBAc,growBAn,growBAp,refrBAc,refrBAn,refrBAp,&
                excrBAc,excrBAn,excrBAp,remiBAn,remiBAp,&
                grazBAc,grazBAn,grazBAp,mortBAc,mortBAn,mortBAp
    ! local variables in the PRT module
    double precision :: growPRTc,growPRTn,growPRTp,&
                        excrPRTldomc,excrPRTldomn,excrPRTldomp, &
                        excrPRTsdomc,excrPRTsdomn,excrPRTsdomp, &
                        excrPRTsdom2c,excrPRTsdom2n,excrPRTsdom2p, &
                        remiPRTn,remiPRTp,pomPRTc,pomPRTn,pomPRTp, &
                        grazPRTc,grazPRTn,grazPRTp,respPRT
    ! local variables in the MZ module
    double precision :: growMZc,growMZn,growMZp,&
                        excrMZldomc,excrMZldomn,excrMZldomp, &
                        excrMZsdomc,excrMZsdomn,excrMZsdomp, &
                        excrMZsdom2c,excrMZsdom2n,excrMZsdom2p, &
                        remiMZn,remiMZp,pomMZc,pomMZn,pomMZp, &
                        remvMZc,remvMZn,remvMZp, pomHZc,pomHZn,&
                        !refrMZc,refrMZn,refrMZp,
                        pomHZp,excrHZsdomc, excrHZsdomn,&
                        excrHZsdomp, remiHZn,remiHZp,respMZ

 ! local variables in the DOM module
    !  double precision :: refrSDOMc,refrSDOMn,refrSDOMp
    ! local variables in the DET module
double precision :: disDETc,disDETn,disDETp, &
       exportc, exportn, export15n, exportp
    ! local variables in the DIN module
    double precision :: nitrf
   
! 局部变量-15N通量 (追踪N通量)
double precision :: dt
double precision :: growSP15nh4, growSP15no3, growSP15n,&
      excrSP_1_15n, excrSP_2_15n, pomSP15n, grazSP15n, &
      total_nh4_util, u_nh4, beta_nh4, factor_nh4, &
      total_no3_util, u_no3, beta_no3, factor_no3, &
      u_sp_graz, beta_sp_graz, factor_sp_graz, &
      u_un_graz, beta_un_graz, factor_un_graz, &
      u_tr_graz, beta_tr_graz, factor_tr_graz, &
      u_dda_graz, beta_dda_graz, factor_dda_graz, &
      u_ba_graz, beta_ba_graz, factor_ba_graz, &
      u_prt_graz, beta_prt_graz, factor_prt_graz, &
      beta_nf, beta_nitrf, &
      u_tr_excr, beta_tr_excr,factor_tr_excr,&
      u_un_excr, beta_un_excr,factor_un_excr,&
      u_dda_excr, beta_dda_excr,factor_dda_excr

double precision :: growTR15nh4, growTR15no3, &
        growTR15nf, growTR15n, excrTR_1_15n, excrTR_15nh4, &
        excrTR_2_15n, pomTR15n, grazTR15n
double precision :: growDDA15nh4, growDDA15no3, growDDA15nf,&
        growDDA15n, excrDDA_1_15n, excrDDA_15nh4, &
        excrDDA_2_15n, pomDDA15n, grazDDA15n
double precision :: growUN15nh4, growUN15no3, growUN15nf, &
        growUN15n, excrUN_1_15n, excrUN_15nh4, excrUN_2_15n, &
        pomUN15n, grazUN15n
double precision :: growBA15ldon, growBA15sdon, growBA15nh4, &
        growBA15no3, growBA15n, refrBA15n, fluxBA15no3, &
excrBA15n, remiBA15n
        !excrBA15n_loc, remiBA15n_loc
double precision :: grazBA15n, mortBA15n, fluxBA15nh4, growPRT15n,&
        excrPRTldom15n, excrPRTsdom15n, excrPRTsdom2_15n
double precision :: remiPRT15n, pomPRT15n, grazPRT15n, &
        growMZ15n, excrMZldom15n, excrMZsdom15n, excrMZsdom2_15n
double precision :: remiMZ15n, pomMZ15n, remvMZ15n, pomHZ15n,&
        excrHZsdom15n, remiHZ15n, disDET15n, nitrf15
 
! 用于计算源池 15N/N 比例的局部变量
double precision :: rSP, rTR, rDDA, rUN, rBA, rPRT, rMZ, &
        rDET, rLDOM, rSDOM, rNH4, rNO3
double precision, parameter :: f15n_atm = 0.003663d0
double precision, parameter :: eps = 1.0d-16
double precision, parameter :: u_limit = 0.9999d0

! map bioparams to local copies
  ae            = bioparams(iae         )
  mu_SP         = bioparams(imu_SP          )
  alpha_SP      = bioparams(ialpha_SP       )
  a_SP          = bioparams(ia_SP       )
  v_SPn         = bioparams(iv_SPn        )
  k_nh4SP       = bioparams(ik_nh4SP        )
  k_no3SP       = bioparams(ik_no3SP        )
  v_SPp         = bioparams(iv_SPp       )
  k_po4SP       = bioparams(ik_po4SP        )
  zeta          = bioparams(izeta       )
  theta         = bioparams(itheta       )
  r_excrSP_1    = bioparams(ir_excrSP_1     )
  r_excrSP_2    = bioparams(ir_excrSP_2     )
  r_pomSP       = bioparams(ir_pomSP        )
  mu_TR         = bioparams(imu_TR          )
  alpha_TR      = bioparams(ialpha_TR       )
  a_TR          = bioparams(ia_TR       )
  v_TRn         = bioparams(iv_TRn        )
  k_nh4TR       = bioparams(ik_nh4TR        )
  k_no3TR       = bioparams(ik_no3TR        )
  v_TRp         = bioparams(iv_TRp        )
  k_po4TR       = bioparams(ik_po4TR        )
  mu_pickTRpo4  = bioparams(imu_pickTRpo4)
  zeta_nf       = bioparams(izeta_nf        )
  r_excrTR_1    = bioparams(ir_excrTR_1     )
  r_excrTR_n    = bioparams(ir_excrTR_n   )
  r_excrTR_2    = bioparams(ir_excrTR_2     )
  r_pomTR       = bioparams(ir_pomTR        )
  mu_DDA        = bioparams(imu_DDA      )
  alpha_DDA     = bioparams(ialpha_DDA       )
  a_DDA         = bioparams(ia_DDA       )
  v_DDAn        = bioparams(iv_DDAn        )
  k_nh4DDA      = bioparams(ik_nh4DDA        )
  k_no3DDA      = bioparams(ik_no3DDA        )
  v_DDAp        = bioparams(iv_DDAp        )
  k_po4DDA      = bioparams(ik_po4DDA        )
  mu_pickDDApo4 = bioparams(imu_pickDDApo4)
  zeta_nf_DDA   = bioparams(izeta_nf_DDA    )
  r_excrDDA_1   = bioparams(ir_excrDDA_1     )
  r_excrDDA_n   = bioparams(ir_excrDDA_n   )
  r_excrDDA_2   = bioparams(ir_excrDDA_2     )
  r_pomDDA      = bioparams(ir_pomDDA        )
  mu_UN         = bioparams(imu_UN          )
  alpha_UN      = bioparams(ialpha_UN       )
  k_DOM         = bioparams(ik_DOM        )
  !b_SDONlabi   = bioparams(ib_SDONlabi     )
  !b_SDOPlabi   = bioparams(ib_SDOPlabi     )
  r_SDOM        = bioparams(ir_SDOM    )
  mu_BA         = bioparams(imu_BA          )
  b_BAresp      = bioparams(ib_BAresp       )
  r_BAadju      = bioparams(ir_BAadju       )
  r_BAremi      = bioparams(ir_BAremi       )
  r_BArefr      = bioparams(ir_BArefr       )
  f_BAslct      = bioparams(if_BAslct)
  r_BAresp_1    = bioparams(ir_BAresp_1)
  r_BAresp_min  = bioparams(ir_BAresp_min)
  r_BAresp_max  = bioparams(ir_BAresp_max)
  r_BAmort      = bioparams(ir_BAmort)
  mu_PRT        = bioparams(imu_PRT       )
  g_sp          = bioparams(ig_sp        )
  g_ba          = bioparams(ig_ba        )
  r_PRTex       = bioparams(ir_PRTex      )
  f_exPRTldom   = bioparams(if_exPRTldom    )
  r_PRTresp_1   = bioparams(ir_PRTresp_1    )
  r_PRTresp_2   = bioparams(ir_PRTresp_2    )
  r_PRTadju     = bioparams(ir_PRTadju      )
  r_PRTremi     = bioparams(ir_PRTremi      )
  r_pomPRT      = bioparams(ir_pomPRT       )
  mu_MZ         = bioparams(imu_MZ       )
  g_prt         = bioparams(ig_prt        )
  g_tr          = bioparams(ig_tr         )
  g_dda         = bioparams(ig_dda         )
  r_MZex        = bioparams(ir_MZex         )
  f_exMZldom    = bioparams(if_exMZldom     )
  r_MZresp_1    = bioparams(ir_MZresp_1     )
  r_MZresp_2    = bioparams(ir_MZresp_2     )
  r_MZadju      = bioparams(ir_MZadju       )
  r_MZremi      = bioparams(ir_MZremi       )
  r_MZpom       = bioparams(ir_MZpom        )
! r_MZrefr      = bioparams(ir_MZrefr       )
  r_MZremv      = bioparams(ir_MZremv       )
  f_HZsdom      = bioparams(if_HZsdom       )
  f_HZpom       = bioparams(if_HZpom        )
! r_SDOMrefr    = bioparams(ir_SDOMrefr     )
  q_refrDOM_n   = bioparams(iq_refrDOM_n)
  q_refrDOM_p   = bioparams(iq_refrDOM_p)
  q_POM_n       = bioparams(iq_POM_n        )
  q_POM_p       = bioparams(iq_POM_p        )
  r_nitrf       = bioparams(ir_nitrf        )
  remin_prf_n   = bioparams(iremin_prf_n    )
  remin_prf_p   = bioparams(iremin_prf_p    )
  wnsvo         = bioparams(iwnsvo          )
  remin         = bioparams(iremin          )
  
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



    !-----------------------------------------------------------------------
    !      Calculate terms used in the ecosystem model.
    !-----------------------------------------------------------------------
    !-----------------------------------------------------------------------
    !      Initialization
    !-----------------------------------------------------------------------
    !Fixed Parameters
    ! minimum [N,P]: C for SP
    q_SP_min_n = 0.034d0
    q_SP_min_p = 0.00188d0
    ! maximum [N,P]: C for SP
    q_SP_max_n = 0.17d0
    q_SP_max_p = 0.0169d0
    ! Redfield ratios
    q_SP_rdf_n  = 0.15d0
    q_SP_rdf_p  = 0.0094d0
    q_TR_min_n  = 0.12d0
    q_TR_min_p  = 0.001d0
    q_TR_max_n  = 0.20d0
    q_TR_max_p  = 0.0060d0
    q_TR_rdf_n  = 0.16d0
    q_TR_rdf_p  = 0.0035d0
    q_DDA_min_n = 0.12d0
    q_DDA_min_p = 0.001d0
    q_DDA_max_n = 0.20d0
    q_DDA_max_p = 0.0060d0
    q_DDA_rdf_n = 0.16d0
    q_DDA_rdf_p = 0.0035d0
    q_UN_min_n  = 0.12d0
    q_UN_min_p  = q_TR_min_p
    q_UN_max_n  = 0.20d0
    q_UN_max_p  = q_TR_max_p
    q_UN_rdf_n  = 0.16d0
    q_UN_rdf_p  = q_TR_rdf_p
    v_UNn       = v_SPn
    k_nh4UN     = k_nh4SP
    k_no3UN     = k_no3SP
    v_UNp       = v_SPp
    k_po4UN     = k_po4SP
    r_excrUN_1  = r_excrSP_1
    r_excrUN_n  = r_excrTR_n
    r_excrUN_2  = r_excrSP_2
    r_pomUN     = r_pomSP
    ! Optimal baterial [C, N, P]: C ratio
    q_BA_n = 0.18d0
    q_BA_p = 0.02d0
    g_un   = g_sp
    ! Optimal protozoa [C, N, P]: C ratio
    q_PRT_n = 0.2d0
    q_PRT_p = 0.022d0
    ! Optimal metazoa [C, N, P]: C ratio
    q_MZ_n = 0.2d0
    q_MZ_p = 0.008d0
    a_UN   = a_SP
    
    Tref = 25.d0 ! Reference Temperature for Function of Temperatue Effects
    ! Temperature Effects
    Tfunc       = exp(-ae*( 1/(Tdat(iz,istep)+273.15) - 1/(Tref+273.15) ) )
    mu_SP       = mu_SP * Tfunc
    v_SPn       = v_SPn * Tfunc
    v_SPp       = v_SPp * Tfunc
    mu_TR       = mu_TR * Tfunc
    v_TRn       = v_TRn * Tfunc
    v_TRp       = v_TRp * Tfunc
    mu_DDA      = mu_DDA * Tfunc
    v_DDAn      = v_DDAn * Tfunc
    v_DDAp      = v_DDAp * Tfunc
    mu_UN       = mu_UN * Tfunc
    v_UNn       = v_UNn * Tfunc
    v_UNp       = v_UNp * Tfunc
    mu_BA       = mu_BA * Tfunc
    mu_PRT      = mu_PRT * Tfunc
    r_BAresp_1  = r_BAresp_1 * Tfunc
    r_PRTresp_1 = r_PRTresp_1 * Tfunc
    mu_MZ       = mu_MZ * Tfunc
    r_MZresp_1  = r_MZresp_1 * Tfunc

    ! 计算各源池当前 15N/N 的比例
    rSP   = y(iSP15n) / (y(iSPn) + eps)
    rTR   = y(iTR15n) / (y(iTRn) + eps)
    rDDA  = y(iDDA15n) / (y(iDDAn) + eps)
    rUN   = y(iUN15n) / (y(iUNn) + eps)
    rBA   = y(iBA15n) / (y(iBAn) + eps)
    rPRT  = y(iPRT15n) / (y(iPRTn) + eps)
    rMZ   = y(iMZ15n) / (y(iMZn) + eps)
    rDET  = y(iDET15n) / (y(iDETn) + eps)
    rLDOM = y(iLDOM15n) / (y(iLDOMn) + eps)
    rSDOM = y(iSDOM15n) / (y(iSDOMn) + eps)
    rNH4  = y(i15NH4) / (y(iNH4) + eps)
    rNO3  = y(i15NO3) / (y(iNO3) + eps)
    !-----------------------------------------------------------------------
    !      Microphytoplankton Processes
    !-----------------------------------------------------------------------
    ! Nutrient quota Limitation
    Nfunc_sp_n = (y(iSPn)/y(iSPc) - q_SP_min_n) / &
                    (q_SP_rdf_n - q_SP_min_n) 
    Nfunc_sp_p = (y(iSPp)/y(iSPc) - q_SP_min_p) / &
                    (q_SP_rdf_p - q_SP_min_p) 
    temp     = min(Nfunc_sp_n, Nfunc_sp_p)
    temp     = min(temp, c1)
    temp     = max(temp, c0)
    Pc_SPmax = mu_SP * temp
    ! Light Limitation
    IF ((Pc_SPmax.gt.c0).and.(y(iSPc).gt.c0)) THEN
               growSPc = y(iSPc) * Pc_SPmax * (c1 - exp(-alpha_SP & 
                *y(iSPchl)/y(iSPc)*rlt/Pc_SPmax)) * exp(-a_SP*rlt)
    ELSE
        growSPc = c0
    END IF  

    ! Nutrient uptake
    V_SPmax_n = max(c0, (q_SP_max_n - y(iSPn)/y(iSPc) ) / (q_SP_max_n - q_SP_rdf_n))
    V_SPmax_n = min(c1, V_SPmax_n)
    growSPnh4 = y(iSPc) * v_SPn * V_SPmax_n * &
                   y(iNH4) / (y(iNH4) + k_nh4SP + y(iNO3) * k_nh4SP/k_no3SP)
    growSPno3 = y(iSPc) * v_SPn * V_SPmax_n * &
                   y(iNO3) / (y(iNO3) + k_no3SP + y(iNH4) * k_no3SP/k_nh4SP) 
    growSPn = growSPnh4 + growSPno3
   
! 添加15N同位素分馏效应
 dt = 1/24
!--------------------------------------------------------------------
! SP NH4 assimilation
!--------------------------------------------------------------------
total_nh4_util = growSPnh4 + growTRnh4 + growDDAnh4 + growUNnh4 + & 
growBAnh4 + nitrf
u_nh4 = (total_nh4_util* dt) / (y(iNH4)+eps) 
u_nh4 = max(0.0d0, min(0.999999d0, u_nh4)) ! 数值约束在 [0, 1]

if (u_nh4 < 1.0d-8) then 
factor_nh4 = 1.0d0 
else 
factor_nh4= -((1.0d0 - u_nh4) / u_nh4* log(1.0d0 - u_nh4))
end if

! 计算 SP 同化NH4的特定 beta 值 
beta_nh4 = rNH4 * (1.0d0 - (epsilon_anh4 / 1000.0d0) * factor_nh4) 
! 最终 SP 的 15N 吸收方程
growSP15nh4 = growSPnh4 * (beta_nh4 / (1.0d0 + beta_nh4))

!--------------------------------------------------------------------
! SP NO3 assimilation
!--------------------------------------------------------------------
total_no3_util = growSPno3 + growTRno3 + growDDAno3 + growUNno3 + & 
growBAno3
u_no3 = (total_no3_util* dt) / y(iNO3) 
u_no3 = max(0.0d0, min (0.999999d0, u_no3)) ! 数值约束在 [0, 1]

if (u_no3 < 1.0d-8) then 
factor_no3 = 1.0d0 
else 
factor_no3= -((1.0d0 - u_no3) / u_no3* log(1.0d0 - u_no3))
end if

!计算 SP 同化NO3的特定 beta 值 
beta_no3 = rNO3 * (1.0d0 - (epsilon_ano3 / 1000.0d0) * factor_no3) 
! 最终 SP 的 15N 吸收方程
growSP15no3 = growSPno3 * (beta_no3 / (1.0d0 + beta_no3))

! 浮游植物同化DIN的总15N
 growSP15n   = growSP15nh4 + growSP15no3

    V_SPmax_p = max(c0, (q_SP_max_p - y(iSPp)/y(iSPc) ) / (q_SP_max_p - q_SP_rdf_p))
    V_SPmax_p = min(c1, V_SPmax_p)
    growSPp   = y(iSPc) * v_SPp * V_SPmax_p *y(iPO4) /( y(iPO4) + k_po4SP)
     
    respSP = growSPno3 * zeta
    
    ! Chlorophyll
    IF (rlt .gt. c0) THEN
        growSPchl = theta * growSPn * &
                   growSPc / (alpha_SP*y(iSPchl)*rlt*exp(-a_SP*rlt)) 
    ELSE
        growSPchl = c0
    END IF  
    
    ! SP excretion (could be also considered as mortality)
    excrSP_1c    = r_excrSP_1 * y(iSPc) + r_excrSP_2 * growSPc * 0.75 ! Passive
    excrSP_1n    = r_excrSP_1 * y(iSPn)
    excrSP_1_15n = excrSP_1n * rSP ! 15N 排泄 1
    excrSP_1p    = r_excrSP_1 * y(iSPp)

    temp      = MAX(c1-y(iSPn)/y(iSPc)/q_SP_rdf_n, c1-y(iSPp)/y(iSPc)/q_SP_rdf_p)
    excrSP_2c = 0.5 * y(iSPc) * MAX(temp,c0)

    IF (excrSP_2c > c0) THEN 
        temp      = MAX(c0, c1 - y(iSPp)/y(iSPn) / (q_SP_rdf_p/q_SP_rdf_n))
        temp1     = MAX(c0, c1 - y(iSPn)/y(iSPp) / (q_SP_rdf_n/q_SP_rdf_p))
        excrSP_2n = 0.5 * MIN(0.25d0 * y(iSPn) * temp, excrSP_2c * q_SP_rdf_n)
        excrSP_2p = 0.5 * MIN(0.25d0 * y(iSPp) * temp1, excrSP_2c * q_SP_rdf_p)
    ELSE
        excrSP_2n = c0
        excrSP_2p = c0
    END IF
    excrSP_2c    = excrSP_2c + r_excrSP_2 * growSPc * 0.25
    excrSP_2_15n = excrSP_2n * rSP ! 15N 排泄 2

    ! Aggregation of SP
        pomSPc   = r_pomSP * y(iSPc)*y(iSPc)
        pomSPn   = pomSPc * y(iSPn) / y(iSPc)
        pomSP15n = pomSPn * rSP       ! 15N 碎屑产生
        pomSPp   = pomSPc * y(iSPp) / y(iSPc)
        pomSPchl = pomSPn * y(iSPchl) / y(iSPn)
    ! SP grazed
    grazSPc = mu_PRT * y(iPRTc) * y(iSPc) * y(iSPc) &
           / (y(iSPc) * y(iSPc) + g_sp * g_sp + &
           y(iUNc)*y(iUNc)/g_un/g_un*g_sp*g_sp + &
           y(iBAc)*y(iBAc)/g_ba/g_ba*g_sp*g_sp)
        grazSPn   = grazSPc * y(iSPn) /y(iSPc)
    
     !--------------------------------------------------------------------
     ! SP be grazed 
     !--------------------------------------------------------------------
     u_sp_graz= (grazSPn* dt) / y(iSPn)
     u_sp_graz = min(1.0d0, max(0.0d0, u_sp_graz)) ! 数值约束在 [0, 1]
     factor_sp_graz= -((1.0d0 - u_sp_graz) / u_sp_graz* log(1.0d0 - u_sp_graz))
     !  计算SP被捕食的特定 beta 值 
     beta_sp_graz = rSP * (1.0d0 - (epsilon_g / 1000.0d0) * factor_sp_graz) 
     ! SP被捕食15N吸收方程
      grazSP15n = grazSPn * (beta_sp_graz / (1.0d0 + beta_sp_graz))

        grazSPp   = grazSPc * y(iSPp) /y(iSPc)
        grazSPchl = grazSPn * y(iSPchl) / y(iSPn)
       ! SP derivs
        dydtt(iSPc)   = (growSPc - excrSP_1c - excrSP_2c - grazSPc - pomSPc & 
                      - respSP)/SecPerDay
        dydtt(iSPn)   = (growSPn - excrSP_1n - excrSP_2n - grazSPn - pomSPn)/ SecPerDay
        dydtt(iSP15n) = (growSP15n - excrSP_1_15n - excrSP_2_15n - grazSP15n &
                        - pomSP15n)/SecPerDay
        dydtt(iSPp)   = (growSPp - excrSP_1p - excrSP_2p - grazSPp - pomSPp)/ SecPerDay
        dydtt(iSPchl) = (growSPchl - grazSPchl - pomSPchl)/ SecPerDay
    
    !-----------------------------------------------------------------------
    !      Trichodesmium Processes
    !-----------------------------------------------------------------------
    ! Primary Production
    ! Nutrient quota Limitation
        Nfunc_tr_n = (y(iTRn)/y(iTRc) - q_TR_min_n) / &
                      (q_TR_rdf_n - q_TR_min_n) 
        Nfunc_tr_p = (y(iTRp)/y(iTRc) - q_TR_min_p) / &
                      (q_TR_rdf_p - q_TR_min_p) 
    temp     = min(Nfunc_tr_n, Nfunc_tr_p)
    temp     = min(temp, c1)
    temp     = max(temp, c0)
    Pc_TRmax = mu_TR * temp
     ! Light Limitation
    IF ((Pc_TRmax.gt.c0).and.(y(iTRc).gt.c0)) THEN 
     growTRc = y(iTRc) * Pc_TRmax * (c1 - exp(-&
            alpha_TR*y(iTRchl)/y(iTRc)*rlt/Pc_TRmax))* exp(-a_TR*rlt)
    ELSE
        growTRc = c0
    END IF  
    ! Nutrient uptake
    V_TRmax_n = max(c0, (q_TR_max_n - y(iTRn)/y(iTRc) ) &
            / (q_TR_max_n - q_TR_rdf_n))
    V_TRmax_n = min(c1, V_TRmax_n)
    growTRnh4 = y(iTRc) * v_TRn * V_TRmax_n *y(iNH4) / (y(iNH4) + &
                 k_nh4TR + y(iNO3) * k_nh4TR/k_no3TR)
    growTRno3 = y(iTRc) * v_TRn * V_TRmax_n * y(iNO3) / (y(iNO3) &
                + k_no3TR + y(iNH4) * k_no3TR/k_nh4TR) 
    IF (Tdat(iz,istep) < 20.0) THEN
        maxTRnf = c0
    ELSE 
        maxTRnf = MAX(c0, &
                 ((y(iTRc) + growTRc - growTRno3 * zeta) * q_TR_max_n &
                   - y(iTRn) - growTRno3 - growTRnh4) &
                / (c1 + zeta_nf * q_TR_max_n) * V_TRmax_n * Tfunc)
    END IF
    growTRn = MIN( y(iTRc) * v_TRn * V_TRmax_n, &
            growTRnh4 + growTRno3 + maxTRnf )
    growTRnf = growTRn - growTRnh4 - growTRno3
    
   ! 15N 吸收与固氮 
   !--------------------------------------------------------------------
     ! TR N2 fixation 
   ! --------------------------------------------------------------------

   beta_nf = f15n_atm * (1.0d0 - epsilon_nf / 1000.0d0) 
   growTR15nf = growTRnf * (beta_nf / (1.0d0 + beta_nf))

    !--------------------------------------------------------------------
     ! TR NH4 Assimilation 
    !--------------------------------------------------------------------
    growTR15nh4 = growTRnh4 * (beta_nh4 / (1.0d0 + beta_nh4))

    !--------------------------------------------------------------------
     ! TR NO3 Assimilation 
    !--------------------------------------------------------------------
    growTR15no3 = growTRno3 * (beta_no3 / (1.0d0 + beta_no3)) 

    growTR15n = growTR15nh4 + growTR15no3 + growTR15nf

    V_TRmax_p = max(c0, (q_TR_max_p - y(iTRp)/y(iTRc) )&
                / (q_TR_max_p -  q_TR_rdf_p))
    V_TRmax_p = min(c1, V_TRmax_p)
    growTRpo4 = y(iTRc) * v_TRp * V_TRmax_p * &
                y(iPO4) /( y(iPO4) + k_po4TR)
    pickTRpo4 = mu_pickTRpo4 * MAX( y(iTRc)*  & 
                (q_TR_max_p+q_TR_rdf_P)/2 - y(iTRp), c0 )
    growTRp = growTRpo4 + pickTRpo4
    ! Respiration
    respTR = growTRno3 * zeta + growTRnf * zeta_nf
    ! Chlorophyll
    IF (rlt .gt. c0) THEN
        growTRchl = theta * growTRn * &
              growTRc / (alpha_TR*y(iTRchl)*rlt*exp(-a_TR*rlt)) 
    ELSE
        growTRchl = c0
END IF  

    ! TR excretion (mortality)
    excrTR_1c = r_excrTR_1 * y(iTRc) + r_excrTR_2 * growTRc * 0.75 ! Passive
    excrTR_1n = r_excrTR_1 * y(iTRn) + 0.5* r_excrTR_n &
                * growTRnf * min(c1, Nfunc_tr_n)
    excrTR_1_15n = excrTR_1n * rTR
    excrTR_nh4 = 0.5* r_excrTR_n * growTRnf * min(c1, Nfunc_tr_n)
    !--------------------------------------------------------------------
     ! TR NH4 excretion
    !--------------------------------------------------------------------
    u_tr_excr = (excrTR_nh4 * dt) /y(iTRn)
    u_tr_excr = min(1.0d0, max(0.0d0, u_tr_excr))
    factor_tr_excr= -((1.0d0 - u_tr_excr) / u_tr_excr* log(1.0d0 - u_tr_excr))
    beta_tr_excr = rTR * (1.0d0 - (epsilon_exnh4 / 1000.0d0) * factor_tr_excr) 
    excrTR_15nh4 = excrTR_nh4 * (beta_tr_excr / (1.0d0 + beta_tr_excr)) 

    excrTR_1p    = r_excrTR_1 * y(iTRp)

    temp      = MAX(c1-y(iTRn)/y(iTRc)/q_TR_rdf_n, c1-y(iTRp)/y(iTRc)/q_TR_rdf_p)
    excrTR_2c = r_excrTR_2 * y(iTRc) * MAX(temp, c0)
    IF (excrTR_2c > c0) THEN 
        temp      = MAX(c0, c1 - y(iTRp)/y(iTRn) / (q_TR_rdf_p/q_TR_rdf_n))
        temp1     = MAX(c0, c1 - y(iTRn)/y(iTRp) / (q_TR_rdf_n/q_TR_rdf_p))
        excrTR_2n = r_excrTR_2 * MIN(0.25d0 * y(iTRn) * &
                temp, excrTR_2c * q_TR_rdf_n) 
        excrTR_2p = r_excrTR_2 * MIN(0.25d0 * y(iTRp) * &
                temp1, excrTR_2c * q_TR_rdf_p) 
    ELSE
        excrTR_2n = c0
        excrTR_2p = c0
    END IF
    excrTR_2c    = excrTR_2c + r_excrTR_2 * growTRc * 0.25
    excrTR_2_15n = excrTR_2n * rTR

    ! Aggregation of TR
    pomTRc   = r_pomTR * y(iTRc)*y(iTRc)
    pomTRn   = pomTRc * y(iTRn) / y(iTRc)
    pomTR15n = pomTRn * rTR
    pomTRp   = pomTRc * y(iTRp) / y(iTRc)
    pomTRchl = pomTRc * y(iTRchl) / y(iTRc)
    
    ! TR grazed
    grazTRc = mu_MZ * y(iMZc) * y(iTRc) * y(iTRc) &
        / (y(iTRc) * y(iTRc) + g_tr * g_tr + &
           y(iPRTc)*y(iPRTc)/g_prt/g_prt*g_tr*g_tr) 
        grazTRn   = grazTRc * y(iTRn)/y(iTRc)

    !--------------------------------------------------------------------
     ! TR be grazed 
    !--------------------------------------------------------------------
     u_tr_graz= (grazTRn* dt) / y(iTRn) 
     u_tr_graz = min(1.0d0, max(0.0d0, u_tr_graz)) ! 数值约束在 [0, 1]
     factor_tr_graz= -((1.0d0 - u_tr_graz) / u_tr_graz * log(1.0d0 - u_tr_graz))
     !  计算TR被捕食的特定 beta 值 
      beta_tr_graz = rTR * (1.0d0 - (epsilon_g / 1000.0d0) * factor_tr_graz)
     !  TR被捕食15N吸收方程
      grazTR15n = grazTRn * (beta_tr_graz / (1.0d0 + beta_tr_graz))

        grazTRp   = grazTRc * y(iTRp)/y(iTRc)
        grazTRchl = grazTRc * y(iTRchl)/y(iTRc)
    ! New TR
       dydtt(iTRc) = (growTRc - excrTR_1c - excrTR_2c - grazTRc &
                   - pomTRc - respTR)/ SecPerDay
       dydtt(iTRn) = (growTRn - excrTR_1n - excrTR_2n - grazTRn - pomTRn &
                      - excrTR_nh4)/ SecPerDay
       dydtt(iTR15n) = (growTR15n - excrTR_1_15n - excrTR_2_15n - &
                       grazTR15n - pomTR15n - excrTR_15nh4)/ SecPerDay
       dydtt(iTRp)   = (growTRp - excrTR_1p - excrTR_2p - grazTRp - pomTRp)/ SecPerDay
       dydtt(iTRchl) = (growTRchl - grazTRchl - pomTRchl)/ SecPerDay
   
    !-----------------------------------------------------------------------
    !      Diatom-Associations Processes
    !-----------------------------------------------------------------------
    ! Primary Production
    ! Nutrient quota Limitation
    Nfunc_dda_n = (y(iDDAn)/y(iDDAc) - q_DDA_min_n) / &
                    (q_DDA_rdf_n - q_DDA_min_n) 
    Nfunc_dda_p = (y(iDDAp)/y(iDDAc) - q_DDA_min_p) / &
                    (q_DDA_rdf_p - q_DDA_min_p) 
    temp      = min(Nfunc_dda_n, Nfunc_dda_p)
    temp      = min(temp, c1)
    temp      = max(temp, c0)
    Pc_DDAmax = mu_DDA * temp
     ! Light Limitation
    IF ((Pc_DDAmax.gt.c0).and.(y(iDDAc).gt.c0)) THEN 
            growDDAc = y(iDDAc) * Pc_DDAmax * (c1 &
            -exp(-alpha_DDA*y(iDDAchl)/y(iDDAc)*rlt/Pc_DDAmax)) &
                * exp(-a_DDA*rlt)
    ELSE
        growDDAc = c0
    END IF  
    ! Nutrient uptake
            V_DDAmax_n = max(c0, (q_DDA_max_n - y(iDDAn)/y(iDDAc) ) &
                    / (q_DDA_max_n - q_DDA_rdf_n))
    V_DDAmax_n = min(c1, V_DDAmax_n)
    growDDAnh4 = y(iDDAc) * v_DDAn * V_DDAmax_n * &
                y(iNH4) / (y(iNH4) + k_nh4DDA + y(iNO3) * k_nh4DDA/k_no3DDA)
    growDDAno3 = y(iDDAc) * v_DDAn * V_DDAmax_n * &
                y(iNO3) / (y(iNO3) + k_no3DDA + y(iNH4) * k_no3DDA/k_nh4DDA) 
    IF (Tdat(iz,istep) < 20.0) THEN
        maxDDAnf = c0
    ELSE 
        maxDDAnf = MAX(c0,((y(iDDAc) + growDDAc - &
                  growDDAno3 * zeta) * q_DDA_max_n &
                 - y(iDDAn) - growDDAno3 - growDDAnh4) &
              / (c1 + zeta_nf_DDA * q_DDA_max_n) * V_DDAmax_n * Tfunc)
    END IF
    growDDAn = MIN( y(iDDAc) * v_DDAn * V_DDAmax_n, &
            growDDAnh4 + growDDAno3 + maxDDAnf )
    growDDAnf = growDDAn - growDDAnh4 - growDDAno3

    ! 15N 吸收与固氮
    !--------------------------------------------------------------------
     ! DDA N2 fixation 
    !--------------------------------------------------------------------
    beta_nf = f15n_atm * (1.0d0 - epsilon_nf / 1000.0d0) 
    growDDA15nf = growDDAnf * (beta_nf / (1.0d0 + beta_nf))

    !--------------------------------------------------------------------
     ! DDA NH4 Assimilation 
    !--------------------------------------------------------------------
    growDDA15nh4 = growDDAnh4 * (beta_nh4 / (1.0d0 + beta_nh4))

    !--------------------------------------------------------------------
     ! DDA NO3 Assimilation 
    !--------------------------------------------------------------------
    growDDA15no3 = growDDAno3 * (beta_no3 / (1.0d0 + beta_no3)) 
    growDDA15n = growDDA15nh4 + growDDA15no3 + growDDA15nf

    V_DDAmax_p = max(c0, (q_DDA_max_p - y(iDDAp)/y(iDDAc) ) &
                    / (q_DDA_max_p - q_DDA_rdf_p))
    V_DDAmax_p = min(c1, V_DDAmax_p)
    growDDApo4 = y(iDDAc) * v_DDAp * V_DDAmax_p * &
                   y(iPO4) /( y(iPO4) + k_po4DDA)
    pickDDApo4 = mu_pickDDApo4 * MAX( y(iDDAc)* & 
                (q_DDA_max_p+q_DDA_rdf_P)/2 - y(iDDAp), c0 )
    growDDAp = growDDApo4 + pickDDApo4
    ! Respiration
    respDDA = growDDAno3 * zeta + growDDAnf * zeta_nf_DDA
    ! Chlorophyll
    IF (rlt .gt. c0) THEN
        growDDAchl = theta * growDDAn * growDDAc / (alpha_DDA* &
                   y(iDDAchl)*rlt*exp(-a_DDA*rlt)) 
    ELSE
        growDDAchl = c0
    END IF  
    ! DDA excretion (mortality)
    excrDDA_1c = r_excrDDA_1 * y(iDDAc) + r_excrDDA_2 * growDDAc * 0.75 ! Passive
    excrDDA_1n = r_excrDDA_1 * y(iDDAn) + 0.5* r_excrDDA_n &
               * growDDAnf * min(c1, Nfunc_DDA_n)
    excrDDA_1p  = r_excrDDA_1 * y(iDDAp)
    excrDDA_nh4 = 0.5* r_excrDDA_n * growDDAnf * min(c1, Nfunc_dda_n)
    excrDDA_1_15n = excrDDA_1n * rDDA


    !-------------------------------------------------------------------
     ! DDA NH4 excretion
    !--------------------------------------------------------------------
    u_dda_excr = (excrDDA_nh4 * dt) /y(iDDAn)
    u_dda_excr = min(1.0d0, max(0.0d0, u_dda_excr))
    factor_dda_excr= -((1.0d0 - u_dda_excr) / u_dda_excr* log(1.0d0 - u_dda_excr))
    beta_dda_excr = rDDA* (1.0d0 - (epsilon_exnh4 / 1000.0d0) * factor_dda_excr) 
    excrDDA_15nh4 = excrDDA_nh4 * (beta_dda_excr / (1.0d0 + beta_dda_excr)) 

    temp = MAX(c1-y(iDDAn)/y(iDDAc)/q_DDA_rdf_n, c1-&
            y(iDDAp)/y(iDDAc)/q_DDA_rdf_p)
    excrDDA_2c = r_excrDDA_2 * y(iDDAc) * MAX(temp, c0)
    IF (excrDDA_2c > c0) THEN 
        temp       = MAX(c0, c1 - y(iDDAp)/y(iDDAn) / (q_DDA_rdf_p/q_DDA_rdf_n))
        temp1      = MAX(c0, c1 - y(iDDAn)/y(iDDAp) / (q_DDA_rdf_n/q_DDA_rdf_p))
        excrDDA_2n = r_excrDDA_2 * MIN(0.25d0 * y(iDDAn) * temp, & 
                excrDDA_2c * q_DDA_rdf_n) 
        excrDDA_2p = r_excrDDA_2 * MIN(0.25d0 * y(iDDAp) * temp1, &
                excrDDA_2c *q_DDA_rdf_p) 
    ELSE
        excrDDA_2n = c0
        excrDDA_2p = c0
END IF
    excrDDA_2c    = excrDDA_2c + r_excrDDA_2 * growDDAc * 0.25
    excrDDA_2_15n = excrDDA_2n * rDDA

    ! Aggregation of DDA
    pomDDAc   = r_pomDDA * y(iDDAc)*y(iDDAc)
    pomDDAn   = pomDDAc * y(iDDAn) / y(iDDAc)
    pomDDA15n = pomDDAn * rDDA
    pomDDAp   = pomDDAc * y(iDDAp) / y(iDDAc)
    pomDDAchl = pomDDAc * y(iDDAchl) / y(iDDAc)
    
   ! DDA grazed
    grazDDAc = mu_MZ * y(iMZc) * y(iDDAc) * y(iDDAc) &
        / (y(iDDAc) * y(iDDAc) + g_dda * g_dda &
          + y(iPRTc)*y(iPRTc)/g_prt/g_prt*g_dda*g_dda) 
    grazDDAn   = grazDDAc * y(iDDAn)/y(iDDAc)
  
    !--------------------------------------------------------------------
     ! DDA be grazed 
    !--------------------------------------------------------------------
    u_dda_graz= (grazDDAn* dt) / (y(iDDAn))
    u_dda_graz = min(1.0d0, max(0.0d0, u_dda_graz)) ! 数值约束在 [0, 1]
    factor_dda_graz= -((1.0d0 - u_dda_graz) / u_dda_graz) * log(1.0d0 - u_dda_graz)
    ! 计算DDA被捕食的特定 beta 值 
    beta_dda_graz = rDDA * (1.0d0 - (epsilon_g / 1000.0d0) * factor_dda_graz)
    ! DDA被捕食15N吸收方程
    grazDDA15n = grazDDAn * (beta_dda_graz / (1.0d0 + beta_dda_graz))

    grazDDAp   = grazDDAc * y(iDDAp)/y(iDDAc)
    grazDDAchl = grazDDAc * y(iDDAchl)/y(iDDAc)
    ! New DDA
    dydtt(iDDAc) = (growDDAc - excrDDA_1c - excrDDA_2c - grazDDAc &
             - pomDDAc - respDDA)/ SecPerDay
    dydtt(iDDAn) = (growDDAn - excrDDA_1n - excrDDA_2n - grazDDAn &
            - pomDDAn - excrDDA_nh4)/ SecPerDay
    dydtt(iDDA15n) = (growDDA15n - excrDDA_1_15n - excrDDA_2_15n&
            - grazDDA15n - pomDDA15n - excrDDA_15nh4)/ SecPerDay
    dydtt(iDDAp) = (growDDAp - excrDDA_1p - excrDDA_2p - grazDDAp & 
            - pomDDAp)/ SecPerDay
    dydtt(iDDAchl) = (growDDAchl - grazDDAchl - pomDDAchl)/ SecPerDay


    !-----------------------------------------------------------------------
    !      Unicellular N2-fixers Processes
    !-----------------------------------------------------------------------
    ! Primary Production
    ! Nutrient quota Limitation
    Nfunc_un_n = (y(iUNn)/y(iUNc) - q_UN_min_n) / &
                  (q_UN_rdf_n - q_UN_min_n) 
    Nfunc_un_p = (y(iUNp)/y(iUNc) - q_UN_min_p) / &
                  (q_UN_rdf_p - q_UN_min_p) 
    temp     = min(Nfunc_un_n, Nfunc_un_p)
    temp     = min(temp, c1)
    temp     = max(temp, c0)
    Pc_UNmax = mu_UN * temp
    ! Light Limitation
     IF ((Pc_UNmax.gt.c0).and.(y(iUNc).gt.c0)) THEN 
        growUNc = y(iUNc) * Pc_UNmax * (c1 - exp(- &
        alpha_UN*y(iUNchl)/y(iUNc)*rlt/Pc_UNmax)) &
                * exp(-a_UN*rlt)
     ELSE
        growUNc = c0
     END IF   
     ! Nutrient uptake
    V_UNmax_n = max(c0, (q_UN_max_n - y(iUNn)/y(iUNc) ) &
                / (q_UN_max_n - q_UN_rdf_n))
    V_UNmax_n = min(c1, V_UNmax_n)
    growUNnh4 = y(iUNc) * v_UNn * V_UNmax_n * y(iNH4) / (y(iNH4) &
                   + k_nh4UN + y(iNO3) * k_nh4UN/k_no3UN)
    growUNno3 = y(iUNc) * v_UNn * V_UNmax_n * y(iNO3) / (y(iNO3) + &
                 k_no3UN + y(iNH4) * k_no3UN/k_nh4UN) 
   maxUNnf = MAX(c0, ((y(iUNc) - growUNc - growUNno3 * zeta)&
                 * q_UN_max_n- y(iUNn) - growUNno3 - growUNnh4) & 
                / (c1 + zeta_nf * q_UN_max_n) * V_UNmax_n * Tfunc)            
    growUNn = MIN( y(iUNc) * v_UNn * V_UNmax_n, &
            growUNnh4 + growUNno3 + maxUNnf )
    growUNnf = growUNn - growUNnh4 - growUNno3
    
   ! 15N 吸收与固氮
    !--------------------------------------------------------------------
     ! UN N2 fixation 
    !--------------------------------------------------------------------
    beta_nf = f15n_atm * (1.0d0 - epsilon_nf / 1000.0d0) 
    growUN15nf = growUNnf * (beta_nf / (1.0d0 + beta_nf))

    !--------------------------------------------------------------------
    ! UN NH4 Assimilation 
    !--------------------------------------------------------------------
    growUN15nh4 = growUNnh4 * (beta_nh4 / (1.0d0 + beta_nh4))

    !--------------------------------------------------------------------
     ! UN NO3 Assimilation 
    !--------------------------------------------------------------------
    growUN15no3 = growUNno3 * (beta_no3 / (1.0d0 + beta_no3)) 
    growUN15n = growUN15nh4 + growUN15no3 + growUN15nf

    V_UNmax_p = max(c0, (q_UN_max_p - y(iUNp)/y(iUNc) )&
        / (q_UN_max_p - q_UN_rdf_p))
    V_UNmax_p = min(c1, V_UNmax_p)
    growUNp   = y(iUNc) * v_UNp * V_UNmax_p * &
                y(iPO4) /( y(iPO4) + k_po4UN)
    respUN = growUNno3 * zeta + growUNnf * zeta_nf
     ! Chlorophyll
    IF (rlt .gt. c0) THEN
        growUNchl = theta * growUNn * growUNc /&
            (alpha_UN*y(iUNchl)*rlt*exp(-a_UN*rlt)) 
    ELSE
        growUNchl = c0
    END IF  
    ! UN excretion
    excrUN_1c = r_excrUN_1 * y(iUNc) + r_excrUN_2 * growUNc * 0.75 ! Passive
    excrUN_1n = r_excrUN_1 * y(iUNn) + 0.5 * r_excrUN_n *&
                growUNnf * min(c1, Nfunc_un_n)
    excrUN_nh4   = 0.5 * r_excrUN_n * growUNnf * min(c1, Nfunc_un_n)

    excrUN_1_15n = excrUN_1n * rUN
    !-------------------------------------------------------------------
     ! UN NH4 excretion
    !--------------------------------------------------------------------
    u_un_excr = (excrUN_nh4 * dt) /y(iUNn)
    u_un_excr = min(1.0d0, max(0.0d0, u_un_excr))
    factor_un_excr= -((1.0d0 - u_un_excr) / u_un_excr* log(1.0d0 - u_un_excr))
    beta_un_excr = rUN*(1.0d0 - (epsilon_exnh4 / 1000.0d0) * factor_un_excr) 
    excrUN_15nh4 = excrUN_nh4 * (beta_un_excr / (1.0d0 + beta_un_excr)) 
    
    excrUN_1p = r_excrUN_1 * y(iUNp)
    temp      = MAX(c1-y(iUNn)/y(iUNc)/q_UN_rdf_n, c1-y(iUNp)/y(iUNc)/q_UN_rdf_p)
    excrUN_2c = r_excrUN_2 * y(iUNc) * MAX(temp,c0)
    IF (excrUN_2c > c0) THEN 
        temp      = MAX(c0, c1 - y(iUNp)/y(iUNn) / (q_UN_rdf_p/q_UN_rdf_n))
        temp1     = MAX(c0, c1 - y(iUNn)/y(iUNp) / (q_UN_rdf_n/q_UN_rdf_p))
        excrUN_2n = r_excrUN_2 * MIN(0.25d0 * y(iUNn) *&
                     temp, excrUN_2c * q_UN_rdf_n) 
        excrUN_2p = r_excrUN_2 * MIN(0.25d0 * y(iUNp) *&
                     temp1, excrUN_2c * q_UN_rdf_p) 
    ELSE
        excrUN_2n = c0
        excrUN_2p = c0
    END IF
    excrUN_2c    = excrUN_2c + r_excrUN_2 * growUNc * 0.25
    excrUN_2_15n = excrUN_2n * rUN

    ! Aggregation of UN
    pomUNc   = r_pomUN * y(iUNc)*y(iUNc)
    pomUNn   = pomUNc * y(iUNn) / y(iUNc)
    pomUN15n = pomUNn * rUN
    pomUNp   = pomUNc * y(iUNp) / y(iUNc)
    pomUNchl = pomUNc * y(iUNchl) / y(iUNc)
    
! UN grazed
    grazUNc = mu_PRT * y(iPRTc) * y(iUNc) * y(iUNc) &
           / (y(iUNc) * y(iUNc) + g_un * g_un + &
           y(iSPc)*y(iSPc)/g_sp/g_sp*g_un*g_un + &
           y(iBAc)*y(iBAc)/g_ba/g_ba*g_un*g_un)
    grazUNn   = grazUNc * y(iUNn)/y(iUNc)

    !--------------------------------------------------------------------
     ! UN be grazed 
    !--------------------------------------------------------------------
    u_un_graz= (grazUNn* dt) / (y(iUNn) )
    u_un_graz = min(1.0d0, max(0.0d0, u_un_graz)) ! 数值约束在 [0, 1]
    factor_un_graz= -((1.0d0 - u_un_graz) / u_un_graz) * log(1.0d0 - u_un_graz)
    !计算UN被捕食的特定 beta 值 
    beta_un_graz = rUN * (1.0d0 - (epsilon_g/1000.0d0) * factor_un_graz)
    ! UN被捕食15N吸收方程
    grazUN15n = grazUNn * (beta_un_graz / (1.0d0 + beta_un_graz))

    grazUNp   = grazUNc * y(iUNp)/y(iUNc)
    grazUNchl = grazUNc * y(iUNchl)/y(iUNc)
    ! New UN
    dydtt(iUNc) = (growUNc - excrUN_1c - excrUN_2c - grazUNc - pomUNc&
 - respUN)/ SecPerDay
    dydtt(iUNn) = (growUNn - excrUN_1n - excrUN_2n - grazUNn - pomUNn &
                   - excrUN_nh4)/ SecPerDay
    dydtt(iUN15n) = (growUN15n - excrUN_1_15n - excrUN_2_15n - &
                grazUN15n - pomUN15n - excrUN_15nh4)/ SecPerDay
    dydtt(iUNp)   = (growUNp - excrUN_1p - excrUN_2p - grazUNp - pomUNp)/ SecPerDay
    dydtt(iUNchl) = (growUNchl - grazUNchl - pomUNchl)/ SecPerDay
       
    !-----------------------------------------------------------------------
    !      Bacterial Processes
    !-----------------------------------------------------------------------
    ! 1. Gross Grow
    ! Maximum possible C amount for bacterial use
        ALC   = y(iLDOMc)
    !temp = MIN(c1, exp(b_SDONlabi * (y(iSDOMn)/y(iSDOMc)/q_BA_n - c1)) )
    !temp = MIN(temp, exp(b_SDOPlabi * (y(iSDOMp)/y(iSDOMc)/q_BA_p - c1)) )
        ASC   = y(iSDOMc) * r_SDOM
    ! Carbon Usage
    Nfunc_ba_n = y(iBAn)/y(iBAc)/q_BA_n
    Nfunc_ba_p = y(iBAp)/y(iBAc)/q_BA_p
    temp       = min(Nfunc_ba_n, Nfunc_ba_p)
    temp       = min(temp, c1)
    growBAldoc = mu_BA * y(iBAc) * temp * ALC &
                          / (ALC+ k_DOM + ASC) 
    growBAsdoc = mu_BA * y(iBAc) * temp * ASC &
                          / (ASC+ k_DOM + ALC)
    ! DON and DOP usage
    growBAldon = growBAldoc/y(iLDOMc)*y(iLDOMn) ! available labile N
    growBAldop = growBAldoc/y(iLDOMc)*y(iLDOMp) ! available labile P
    growBAsdon = growBAsdoc * min(q_BA_n, (y(iSDOMn)/y(iSDOMc)&
               + f_BAslct/Nfunc_ba_n*(q_BA_n-y(iSDOMn)/y(iSDOMc))))
    growBAsdop = growBAsdoc * min(q_BA_p, (y(iSDOMp)/&
             y(iSDOMc) + f_BAslct/Nfunc_ba_p*(q_BA_p-y(iSDOMp)/y(iSDOMc))))

    ! 15N DON 摄取
    growBA15ldon = growBAldon * rLDOM
    growBA15sdon = growBAsdon * rSDOM

    ! inorganic nutrients uptake
    growBAnh4 = growBAldon / y(iLDOMn) * y(iNH4) * min(c1, 1/Nfunc_ba_n)
    if (Nfunc_ba_n.lt.c1) then
        growBAno3 = min(0.1 * (growBAldon + growBAsdon) / (y(iLDOMn)&
            +y(iSDOMn)) * y(iNO3) * min(c1, 1/Nfunc_ba_n), &
                (growBAldon + growBAsdon) / (y(iLDOMn) + y(iSDOMn)) &
                 * (y(iNO3) + y(iNH4)) - growBAnh4)
        growBAno3 = max(c0, growBAno3)
    else
        growBAno3 = c0
    end if
    growBApo4 = growBAldop / y(iLDOMp) * y(iPO4) * min(c1, 1/Nfunc_ba_p)

! 15N 无机摄取
    !--------------------------------------------------------------------
    ! BA NH4 Assimilation 
    !--------------------------------------------------------------------
    growBA15nh4 = growBAnh4 * (beta_nh4 / (1.0d0 + beta_nh4))

    !--------------------------------------------------------------------
     ! BA NO3 Assimilation 
    !--------------------------------------------------------------------
    growBA15no3 = growBAno3 * (beta_no3 / (1.0d0 + beta_no3)) 

    ! Bacteria gross growth
    growBAc   = growBAldoc + growBAsdoc
    growBAn   = growBAldon + growBAsdon + growBAnh4 + growBAno3
    growBA15n = growBA15ldon + growBA15sdon + growBA15nh4 + growBA15no3
    growBAp   = growBAldop + growBAsdop + growBApo4
    ! 2. respiration
    respBA = r_BAresp_1 * y(iBAc) + zeta * growBAno3 + &
        (r_BAresp_min + (r_BAresp_max - r_BAresp_min) &
            *EXP(-b_BAresp*growBAc) )* growBAc

    ! 3. excreting refractory DOM
    refrBAc   = r_BArefr * y(iBAc)
    refrBAn   = q_refrDOM_n * refrBAc
    refrBA15n = refrBAn * rBA
    refrBAp   = q_refrDOM_p * refrBAc
    ! 4. excreting semi-labile DOM and regenerating DIN
    IF ( (y(iBAc) < y(iBAn)/q_BA_n) .AND. &
         (y(iBAc) < y(iBAp)/q_BA_p) ) THEN  !Cabon in short
         excrBAc = c0
         excrBAn = c0
         excrBAp = c0
         remiBAn = r_BAremi * (y(iBAn) - y(iBAc) * q_BA_n)
         remiBAp = r_BAremi * (y(iBAp) - y(iBAc) * q_BA_p)
    ELSE IF ( (y(iBAc) > y(iBAn)/q_BA_n) .AND. &
         (y(iBAp)/q_BA_p > y(iBAn)/q_BA_n) ) THEN !Nitrogen in short
         excrBAc = r_BAadju * (y(iBAc) - y(iBAn)/q_BA_n)
         excrBAn = c0
         excrBAp = r_BAadju * (y(iBAp) - y(iBAn)/q_BA_n * q_BA_p)
         remiBAn = c0
         remiBAp = c0
    ELSE !Phosphorus in short
         excrBAc = r_BAadju * (y(iBAc) - y(iBAp)/q_BA_p)
         excrBAn = r_BAadju * (y(iBAn) - y(iBAp)/q_BA_p * q_BA_n)
         excrBAp = c0
         remiBAn = c0
         remiBAp = c0
END IF
excrBA15n= excrBAn* rBA
remiBA15n = remiBAn * rBA

    !6. removal by grazing
    grazBAc = mu_PRT * y(iPRTc) * y(iBAc) * y(iBAc) &
        / (y(iBAc) * y(iBAc) + g_ba * g_ba + &
           y(iSPc)*y(iSPc)/g_sp/g_sp*g_ba*g_ba + &
           y(iUNc)*y(iUNc)/g_un/g_un*g_ba*g_ba)
    grazBAn   = grazBAc / y(iBAc) * y(iBAn)

    !--------------------------------------------------------------------
     ! BA be grazed 
    !--------------------------------------------------------------------
    u_ba_graz= (grazBAn* dt) / (y(iBAn) )
    u_ba_graz = min(1.0d0, max(0.0d0, u_ba_graz)) ! 数值约束在 [0, 1]
    factor_ba_graz= -((1.0d0 - u_ba_graz) / u_ba_graz) * log(1.0d0 - u_ba_graz)
    !  计算BA被捕食的特定 beta 值 
    beta_ba_graz = rBA * (1.0d0 - (epsilon_g/1000.0d0) * factor_ba_graz)
    ! BA被捕食15N吸收方程
    grazBA15n = grazBAn * (beta_ba_graz / (1.0d0 + beta_ba_graz))

    grazBAp   = grazBAc / y(iBAc) * y(iBAp)
    !6b. Mortality due to viruses
    mortBAc   = r_BAmort * y(iBAc)
    mortBAn   = r_BAmort * y(iBAn)
    mortBA15n = mortBAn * rBA
    mortBAp   = r_BAmort * y(iBAp)
     
    !7. BA Derivs
    dydtt(iBAc) = (growBAc  - excrBAc - grazBAc &
                   - respBA - mortBAc)/ SecPerDay
    dydtt(iBAn) = (growBAn  - excrBAn - remiBAn - grazBAn &
                   - mortBAn) / SecPerDay
    dydtt(iBA15n) = (growBA15n - excrBA15n - remiBA15n &
                    -grazBA15n - mortBA15n)/ SecPerDay
    dydtt(iBAp) = (growBAp - excrBAp - remiBAp - grazBAp &
                   - mortBAp) / SecPerDay
    !8. Flux of inorganic nutrients through bacteria
    fluxBAnh4   = growBAnh4 - remiBAn
    fluxBAno3   = growBAno3
    fluxBA15nh4 = growBA15nh4 - remiBA15n
    fluxBA15no3 = growBA15no3
    fluxBApo4   = growBApo4 - remiBAp

    !-----------------------------------------------------------------------
    !      Protozoan Processes
    !-----------------------------------------------------------------------
    ! 1. gross growth
    growPRTc   = grazSPc + grazBAc + grazUNc
    growPRTn   = grazSPn + grazBAn + grazUNn
    growPRT15n = grazSP15n + grazBA15n + grazUN15n
    growPRTp   = grazSPp + grazBAp + grazUNp
    ! 2. DOM excretion
    excrPRTldomc   = f_exPRTldom * r_PRTex * growPRTc
    excrPRTldomn   = f_exPRTldom * r_PRTex * growPRTn
    excrPRTldom15n = f_exPRTldom * r_PRTex * growPRT15n
    excrPRTldomp   = f_exPRTldom * r_PRTex * growPRTp
    excrPRTsdomc   = (c1 - f_exPRTldom) * r_PRTex * growPRTc
    excrPRTsdomn   = (c1 - f_exPRTldom) * r_PRTex * growPRTn *&
                      y(iPRTn)/y(iPRTc)/q_PRT_n
    excrPRTsdom15n = excrPRTsdomn * rPRT
    excrPRTsdomp   = (c1 - f_exPRTldom) * r_PRTex * growPRTp *&
                   y(iPRTp)/y(iPRTc)/q_PRT_p
    ! * EXP(3*(y(iPRTp)/y(iPRTc)/q_PRT_p-c1))
    ! 3. respiration
    respPRT = r_PRTresp_1 * y(iPRTc) + r_PRTresp_2 * growPRTc
    ! 4. adjust body stoichiometry by excreting semi-labile DOM
    temp = MAX(c1 - y(iPRTn)/y(iPRTc)/q_PRT_n, c1 - y(iPRTp)/y(iPRTc)/q_PRT_p)
    temp = MAX(c0, temp)
    excrPRTsdom2c    = r_PRTadju * y(iPRTc) * temp
    excrPRTsdom2n    = 0.5d0*excrPRTsdom2c * y(iPRTn)/y(iPRTc)
    excrPRTsdom2_15n = excrPRTsdom2n * rPRT
    excrPRTsdom2p    = 0.5d0*excrPRTsdom2c * y(iPRTp)/y(iPRTc)
    ! 5. adjust body stoichiometry by remineralizing inorganic nutrients
    remiPRTn = MAX(r_PRTremi*(y(iPRTn)-q_PRT_n * y(iPRTc)), &
             r_PRTremi*(y(iPRTn)-q_PRT_n/q_PRT_p*y(iPRTp)) )
    remiPRTn   = MAX(c0, remiPRTn)
    remiPRT15n = remiPRTn * rPRT
    remiPRTp   = MAX(r_PRTremi*(y(iPRTp)-q_PRT_p * y(iPRTc)), &
                r_PRTremi*(y(iPRTp)-q_PRT_p/q_PRT_n*y(iPRTn)) )
    remiPRTp = MAX(c0, remiPRTp)
    ! POM production
    pomPRTc   = r_pomPRT * growPRTc
    pomPRTn   = pomPRTc * q_POM_n
    pomPRT15n = pomPRTn * rPRT
    pomPRTp   = pomPRTc * q_POM_p
    ! 6. removal by microzooplankton
    grazPRTc = mu_MZ * y(iMZc) * y(iPRTc) * y(iPRTc) &
        / (y(iPRTc) * y(iPRTc) + g_prt * g_prt + &
           y(iTRc)*y(iTRc)/g_tr/g_tr*g_prt*g_prt + &
           y(iDDAc)*y(iDDAc)/g_dda/g_dda*g_prt*g_prt)  
        grazPRTn   = grazPRTc * y(iPRTn) / y(iPRTc)
     !--------------------------------------------------------------------
     ! PRT be grazed 
     !--------------------------------------------------------------------
     u_prt_graz= (grazPRTn* dt)/y(iPRTn) 
     u_prt_graz = min(1.0d0, max(0.0d0, u_prt_graz)) ! 数值约束在 [0, 1]
     factor_prt_graz= -((1.0d0 - u_tr_graz) /u_prt_graz * log(1.0d0 - u_prt_graz))
     !  计算PRT被捕食的特定 beta 值 
      beta_prt_graz = rPRT * (1.0d0 - (epsilon_g / 1000.0d0) * factor_prt_graz)
     ! PRT被捕食15N吸收方程
      grazPRT15n = grazPRTn * (beta_prt_graz / (1.0d0 + beta_prt_graz))

     grazPRTp   = grazPRTc * y(iPRTp) / y(iPRTc)
    ! 7. new PRT
    dydtt(iPRTc) = (growPRTc - excrPRTldomc - excrPRTsdomc -&
            excrPRTsdom2c - grazPRTc - pomPRTc- respPRT)/ SecPerDay
    dydtt(iPRTn) = (growPRTn - excrPRTldomn - excrPRTsdomn - &
            excrPRTsdom2n - remiPRTn - grazPRTn - pomPRTn)/ SecPerDay
    dydtt(iPRT15n) = (growPRT15n - excrPRTldom15n - &
            excrPRTsdom15n - excrPRTsdom2_15n - remiPRT15n&
            - grazPRT15n - pomPRT15n)/ SecPerDay
    dydtt(iPRTp) = (growPRTp - excrPRTldomp - excrPRTsdomp - &
            excrPRTsdom2p - remiPRTp - grazPRTp- pomPRTp)/ SecPerDay

    !-----------------------------------------------------------------------
    !      Metozoan Processes
    !-----------------------------------------------------------------------
    ! 1. gross growth
    growMZc   = grazPRTc + grazTRc+grazDDAc
    growMZn   = grazPRTn + grazTRn+grazDDAn
    growMZ15n = grazPRT15n + grazTR15n + grazDDA15n
    growMZp   = grazPRTp + grazTRp+grazDDAp
    ! 2. DOM excretion
    excrMZldomc   = f_exMZldom * r_MZex * growMZc
    excrMZldomn   = f_exMZldom * r_MZex * growMZn
    excrMZldom15n = f_exMZldom * r_MZex * growMZ15n
    excrMZldomp   = f_exMZldom * r_MZex * growMZp
    excrMZsdomc   = (c1 - f_exMZldom) * r_MZex * growMZc
    excrMZsdomn   = (c1 - f_exMZldom) * r_MZex * growMZn * &
                y(iMZn)/y(iMZc)/q_MZ_n
    excrMZsdom15n = excrMZsdomn * rMZ
    excrMZsdomp   = (c1 - f_exMZldom) * r_MZex * growMZp *&
                y(iMZp)/y(iMZc)/q_MZ_p
    
    ! 3. respiration
    respMZ = r_MZresp_1 * y(iMZc) + r_MZresp_2 * growMZc
    ! 4. adjust body stoichiometry by excreting semi-labile DOM
        temp = MAX(c1 - y(iMZn)/y(iMZc)/q_MZ_n, c1 - y(iMZp)/y(iMZc)/q_MZ_p)
        temp  = MAx(temp, c0)
        excrMZsdom2c   = r_MZadju * y(iMZc) * temp
        excrMZsdom2n   = excrMZsdom2c * y(iMZn)/y(iMZc)*0.5d0
        excrMZsdom2_15n = excrMZsdom2n * rMZ
        excrMZsdom2p   = excrMZsdom2c * y(iMZp)/y(iMZc)*0.5d0
    ! 5. adjust body stoichiometry by remineralizing inorganic nutrients
    remiMZn = MAX(r_MZremi*(y(iMZn)-q_MZ_n * y(iMZc)), &
                r_MZremi*(y(iMZn)-q_MZ_n/q_MZ_p*y(iMZp)))
    remiMZn   = MAX(remiMZn, c0)
    remiMZ15n = remiMZn * rMZ
    remiMZp   = MAX(r_MZremi*(y(iMZp)-q_MZ_p * y(iMZc)), &
                  r_MZremi*(y(iMZp)-q_MZ_p/q_MZ_n*y(iMZn)))
    remiMZp = MAX(remiMZp, c0)
    ! 6. indissolved POM and refractory DOM production
        pomMZc   = r_MZpom * growMZc
        pomMZn   = q_POM_n * pomMZc
        pomMZ15n = pomMZn * rMZ
        pomMZp   = q_POM_p * pomMZc
        ! refrMZc  = r_MZrefr * growMZc
        ! refrMZn  = q_refrDOM_n * refrMZc
        ! refrMZp  = q_refrDOM_p * refrMZc
    ! 7. removal by higher-level zooplankton
        remvMZc       = r_MZremv * y(iMZc) * y(iMZc)
        remvMZn       = remvMZc / y(iMZc) * y(iMZn)
        remvMZ15n     = remvMZn * rMZ
        remvMZp       = remvMZc / y(iMZc) * y(iMZp)
        pomHZc        = f_HZpom * remvMZc
        pomHZn        = f_HZpom * remvMZn
        pomHZ15n      = f_HZpom * remvMZ15n
        pomHZp        = f_HZpom * remvMZp
        excrHZsdomc   = remvMZc * f_HZsdom
        excrHZsdomn   = remvMZn * f_HZsdom
        excrHZsdom15n = remvMZ15n * f_HZsdom
        excrHZsdomp   = remvMZp * f_HZsdom
        remiHZn       = remvMZn - excrHZsdomn - pomHZn
        remiHZ15n     = remvMZ15n - excrHZsdom15n - pomHZ15n
        remiHZp       = remvMZp - excrHZsdomp - pomHZp
    ! 8. new MZ
        dydtt(iMZc) = (growMZc - excrMZldomc -excrMZsdomc - excrMZsdom2c &
                    - pomMZc - remvMZc - respMZ) / SecPerDay
        dydtt(iMZn) = (growMZn - excrMZldomn -excrMZsdomn - remiMZn - &
                excrMZsdom2n - pomMZn - remvMZn)/ SecPerDay
        dydtt(iMZ15n) = (growMZ15n - excrMZldom15n -excrMZsdom15n -&
            remiMZ15n - excrMZsdom2_15n - pomMZ15n - remvMZ15n)/ SecPerDay
        dydtt(iMZp) = (growMZp - excrMZldomp -excrMZsdomp - remiMZp - &
                excrMZsdom2p - pomMZp - remvMZp)/ SecPerDay

    !-----------------------------------------------------------------------
    !      Detritus Processes
    !-----------------------------------------------------------------------
        disDETc      = remin * y(iDETc)
        disDETn      = remin * y(iDETn) * remin_prf_n
        disDET15n    = disDETn * rDET
        disDETp      = remin * y(iDETp) * remin_prf_p
       
        dydtt(iDETc) = (pomSPc + pomTRc + pomDDAc + pomUNc + pomPRTc &
                    + pomMZc + pomHZc - disDETc)/ SecPerDay
        dydtt(iDETn) = (pomSPn + pomTRn + pomDDAn + pomUNn + pomPRTn &
                    + pomMZn + pomHZn - disDETn)/ SecPerDay
        dydtt(iDET15n) = (pomSP15n + pomTR15n + pomDDA15n + pomUN15n &
          + pomPRT15n + pomMZ15n + pomHZ15n - disDET15n)/ SecPerDay
        dydtt(iDETp) = (pomSPp + pomTRp + pomDDAp + pomUNp + pomPRTp &
                    + pomMZp + pomHZp - disDETp)/ SecPerDay

    !-----------------------------------------------------------------------
    !   Inorganic Nutrients Processes
    !-----------------------------------------------------------------------
        nitrf = r_nitrf * y(iNH4)
     !--------------------------------------------------------------------
     ! nitrification
     !--------------------------------------------------------------------
       nitrf15= nitrf * rNH4 * (1.0d0 - epsilon_nitrf / 1000.0d0)
       
        dydtt(iNH4) = (remiPRTn + remiMZn + remiHZn + excrTR_nh4 + &
            excrDDA_nh4+excrUN_nh4 - growSPnh4 - growTRnh4 &
            - growDDAnh4- growUNnh4 - fluxBAnh4 - nitrf) / SecPerDay
        dydtt(iNO3) = (-growSPno3 - growTRno3 - growDDAno3 - growUNno3 &
                    - fluxBAno3 + nitrf)/ SecPerDay
        dydtt(i15NH4) = (remiPRT15n + remiMZ15n + remiHZ15n + excrTR_15nh4 + &
                    excrDDA_15nh4 + excrUN_15nh4 - growSP15nh4 - &
                    growTR15nh4 - growDDA15nh4 - growUN15nh4 - &
                    fluxBA15nh4 - nitrf15) / SecPerDay
        dydtt(i15NO3) = (nitrf15-growSP15no3 - growTR15no3 - growDDA15no3 - &
                    growUN15no3 - fluxBA15no3)/ SecPerDay
        dydtt(iPO4) = (remiPRTp + remiMZp + remiHZp - growSPp - growTRpo4 &
                    - growDDApo4 - growUNp - fluxBApo4)/ SecPerDay

    !-----------------------------------------------------------------------
    !      Dissolved Organic Matter (DOM) Processes
    !-----------------------------------------------------------------------
        dydtt(iLDOMc) = (excrSP_1c + excrTR_1c + excrDDA_1c + &
                    excrUN_1c + excrPRTldomc+ excrMZldomc - &
                    growBAldoc + mortBAc)/ SecPerDay
        dydtt(iLDOMn) = (excrSP_1n + excrTR_1n + excrDDA_1n + &
                    excrUN_1n + excrPRTldomn + excrMZldomn - &
                    growBAldon + mortBAn)/ SecPerDay
        dydtt(iLDOM15n) = (excrSP_1_15n + excrTR_1_15n + excrDDA_1_15n + &
                    excrUN_1_15n + excrPRTldom15n + excrMZldom15n - &
                    growBA15ldon + mortBA15n)/ SecPerDay
        dydtt(iLDOMp) = (excrSP_1p + excrTR_1p + excrDDA_1p &
            + excrUN_1p + excrPRTldomp + excrMZldomp&
                 - growBAldop + mortBAp)/ SecPerDay
        dydtt(iSDOMc) = (excrSP_2c + excrTR_2c+ excrDDA_2c + excrUN_2c &
                    + excrBAc + excrPRTsdomc + excrPRTsdom2c &
                    + excrMZsdomc + excrMZsdom2c + excrHZsdomc & 
                    + disDETc - growBAsdoc)/ SecPerDay
        dydtt(iSDOMn) = (excrSP_2n + excrTR_2n + excrDDA_2n + excrUN_2n & 
                    + excrBAn + excrPRTsdomn + excrPRTsdom2n &
                    + excrMZsdomn + excrMZsdom2n + excrHZsdomn &
                    + disDETn - growBAsdon)/ SecPerDay
        dydtt(iSDOM15n) = (excrSP_2_15n + excrTR_2_15n + excrDDA_2_15n + &
                    excrUN_2_15n + excrBA15n + excrPRTsdom15n &
                    + excrPRTsdom2_15n + excrMZsdom15n + &
                    excrMZsdom2_15n + excrHZsdom15n + disDET15n &
                    - growBA15sdon)/ SecPerDay
        dydtt(iSDOMp) = (excrSP_2p + excrTR_2p + excrDDA_2p + excrUN_2p &
                    + excrBAp + excrPRTsdomp + excrPRTsdom2p &
                    + excrMZsdomp + excrMZsdom2p + excrHZsdomp + disDETp &
                    - growBAsdop)/ SecPerDay   
      
        dydtt_diag(iPP)=(growSPc+growTRc+growDDAc +growUNc &
                    - respSP - respTR - respDDA - respUN &
                    - excrSP_1c - excrTR_1c - excrDDA_1c &
                    - excrUN_1c - excrSP_2c - excrTR_2c - &
                    excrDDA_2c - excrUN_2c)/SecPerDay*12.d0

    dydtt_diag(iprBAc)           = (growBAc-respBA-mortBAc)/SecPerDay
    dydtt_diag(igrowSPc)         = growSPc/SecPerDay
    dydtt_diag(igrowSPnh4)       = growSPnh4/SecPerDay
    dydtt_diag(igrowSPno3)       = growSPno3/SecPerDay
    dydtt_diag(igrowSPn)         = growSPn/SecPerDay
    dydtt_diag(igrowSP15nh4)     = growSP15nh4/SecPerDay
    dydtt_diag(igrowSP15no3)     = growSP15no3/SecPerDay
    dydtt_diag(igrowSP15n)       = growSP15n/SecPerDay
    dydtt_diag(igrowSPp)         = growSPp/SecPerDay
    dydtt_diag(iexcrSP_1c)       = excrSP_1c/SecPerDay
    dydtt_diag(iexcrSP_1n)       = excrSP_1n/SecPerDay
    dydtt_diag(iexcrSP_1_15n)    = excrSP_1_15n/SecPerDay
    dydtt_diag(iexcrSP_1p)       = excrSP_1p/SecPerDay
    dydtt_diag(iexcrSP_2c)       = excrSP_2c/SecPerDay
    dydtt_diag(iexcrSP_2n)       = excrSP_2n/SecPerDay
    dydtt_diag(iexcrSP_2_15n)    = excrSP_2_15n/SecPerDay
    dydtt_diag(iexcrSP_2p)       = excrSP_2p/SecPerDay
    dydtt_diag(ipomSPc)          = pomSPc/SecPerDay
    dydtt_diag(ipomSPn)          = pomSPn/SecPerDay
    dydtt_diag(ipomSP15n)        = pomSP15n/SecPerDay
    dydtt_diag(ipomSPp)          = pomSPp/SecPerDay
    dydtt_diag(igrazSPc)         = grazSPc/SecPerDay
    dydtt_diag(igrazSPn)         = grazSPn/SecPerDay
    dydtt_diag(igrazSP15n)       = grazSP15n/SecPerDay
    dydtt_diag(igrazSPp)         = grazSPp/SecPerDay
    dydtt_diag(igrowTRc)         = growTRc/SecPerDay
    dydtt_diag(igrowTRnh4)       = growTRnh4/SecPerDay
    dydtt_diag(igrowTRno3)       = growTRno3/SecPerDay
    dydtt_diag(igrowTRnf)        = growTRnf/SecPerDay
    dydtt_diag(igrowTRn)         = growTRn/SecPerDay
    dydtt_diag(igrowTR15nh4)     = growTR15nh4/SecPerDay
    dydtt_diag(igrowTR15no3)     = growTR15no3/SecPerDay
    dydtt_diag(igrowTR15nf)      = growTR15nf/SecPerDay
    dydtt_diag(igrowTR15n)       = growTR15n/SecPerDay
    dydtt_diag(igrowTRpo4)       = growTRpo4/SecPerDay
    dydtt_diag(ipickTRpo4)       = pickTRpo4/SecPerDay
    dydtt_diag(igrowTRp)         = growTRp/SecPerDay
    dydtt_diag(iexcrTR_1c)       = excrTR_1c/SecPerDay
    dydtt_diag(iexcrTR_1n)       = excrTR_1n/SecPerDay
    dydtt_diag(iexcrTR_1_15n)    = excrTR_1_15n/SecPerDay
    dydtt_diag(iexcrTR_1p)       = excrTR_1p/SecPerDay
    dydtt_diag(iexcrTR_nh4)      = excrTR_nh4/SecPerDay
    dydtt_diag(iexcrTR_15nh4)    = excrTR_15nh4/SecPerDay
    dydtt_diag(iexcrTR_2c)       = excrTR_2c/SecPerDay
    dydtt_diag(iexcrTR_2n)       = excrTR_2n/SecPerDay
    dydtt_diag(iexcrTR_2_15n)    = excrTR_2_15n/SecPerDay
    dydtt_diag(iexcrTR_2p)       = excrTR_2p/SecPerDay
    dydtt_diag(ipomTRc)          = pomTRc/SecPerDay
    dydtt_diag(ipomTRn)          = pomTRn/SecPerDay
    dydtt_diag(ipomTR15n)        = pomTR15n/SecPerDay
    dydtt_diag(ipomTRp)          = pomTRp/SecPerDay
    dydtt_diag(igrazTRc)         = grazTRc/SecPerDay
    dydtt_diag(igrazTRn)         = grazTRn/SecPerDay
    dydtt_diag(igrazTR15n)       = grazTR15n/SecPerDay
    dydtt_diag(igrazTRp)         = grazTRp/SecPerDay
    dydtt_diag(igrowDDAc)        = growDDAc/SecPerDay
    dydtt_diag(igrowDDAnh4)      = growDDAnh4/SecPerDay
    dydtt_diag(igrowDDA15nh4)    = growDDA15nh4/SecPerDay
    dydtt_diag(igrowDDAno3)      = growDDAno3/SecPerDay
    dydtt_diag(igrowDDAnf)       = growDDAnf/SecPerDay
    dydtt_diag(igrowDDAn)        = growDDAn/SecPerDay
    dydtt_diag(igrowDDA15no3)    = growDDA15no3/SecPerDay
    dydtt_diag(igrowDDA15nf)     = growDDA15nf/SecPerDay
    dydtt_diag(igrowDDApo4)      = growDDApo4/SecPerDay
    dydtt_diag(ipickDDApo4)      = pickDDApo4/SecPerDay
    dydtt_diag(igrowDDAp)        = growDDAp/SecPerDay
    dydtt_diag(iexcrDDA_1c)      = excrDDA_1c/SecPerDay
    dydtt_diag(iexcrDDA_1n)      = excrDDA_1n/SecPerDay
    dydtt_diag(iexcrDDA_1_15n)   = excrDDA_1_15n/SecPerDay
    dydtt_diag(iexcrDDA_1p)      = excrDDA_1p/SecPerDay
    dydtt_diag(iexcrDDA_nh4)     = excrDDA_nh4/SecPerDay
    dydtt_diag(iexcrDDA_15nh4)   = excrDDA_15nh4/SecPerDay
    dydtt_diag(iexcrDDA_2c)      = excrDDA_2c/SecPerDay
    dydtt_diag(iexcrDDA_2n)      = excrDDA_2n/SecPerDay
    dydtt_diag(iexcrDDA_2_15n)   = excrDDA_2_15n/SecPerDay
    dydtt_diag(iexcrDDA_2p)      = excrDDA_2p/SecPerDay
    dydtt_diag(ipomDDAc)         = pomDDAc/SecPerDay
    dydtt_diag(ipomDDAn)         = pomDDAn/SecPerDay
    dydtt_diag(ipomDDA15n)       = pomDDA15n/SecPerDay
    dydtt_diag(ipomDDAp)         = pomDDAp/SecPerDay
    dydtt_diag(igrazDDAc)        = grazDDAc/SecPerDay
    dydtt_diag(igrazDDAn)        = grazDDAn/SecPerDay
    dydtt_diag(igrazDDA15n)      = grazDDA15n/SecPerDay
    dydtt_diag(igrazDDAp)        = grazDDAp/SecPerDay
    dydtt_diag(igrowUNc)         = growUNc/SecPerDay
    dydtt_diag(igrowUNnh4)       = growUNnh4/SecPerDay
    dydtt_diag(igrowUN15nh4)     = growUN15nh4/SecPerDay
    dydtt_diag(igrowUNno3)       = growUNno3/SecPerDay
    dydtt_diag(igrowUNnf)        = growUNnf/SecPerDay
    dydtt_diag(igrowUNn)         = growUNn/SecPerDay
    dydtt_diag(igrowUN15no3)     = growUN15no3/SecPerDay
    dydtt_diag(igrowUN15nf)      = growUN15nf/SecPerDay
    dydtt_diag(igrowUN15n)       = growUN15n/SecPerDay
    dydtt_diag(igrowUNp)         = growUNp/SecPerDay
    dydtt_diag(iexcrUN_1c)       = excrUN_1c/SecPerDay
    dydtt_diag(iexcrUN_1n)       = excrUN_1n/SecPerDay
    dydtt_diag(iexcrUN_1_15n)    = excrUN_1_15n/SecPerDay
    dydtt_diag(iexcrUN_1p)       = excrUN_1p/SecPerDay
    dydtt_diag(iexcrUN_nh4)      = excrUN_nh4/SecPerDay
    dydtt_diag(iexcrUN_15nh4)    = excrUN_15nh4/SecPerDay
    dydtt_diag(iexcrUN_2c)       = excrUN_2c/SecPerDay
    dydtt_diag(iexcrUN_2n)       = excrUN_2n/SecPerDay
    dydtt_diag(iexcrUN_2_15n)    = excrUN_2_15n/SecPerDay
    dydtt_diag(iexcrUN_2p)       = excrUN_2p/SecPerDay
    dydtt_diag(ipomUNc)          = pomUNc/SecPerDay
    dydtt_diag(ipomUNn)          = pomUNn/SecPerDay
    dydtt_diag(ipomUN15n)        = pomUN15n/SecPerDay
    dydtt_diag(ipomUNp)          = pomUNp/SecPerDay
    dydtt_diag(igrazUNc)         = grazUNc/SecPerDay
    dydtt_diag(igrazUNn)         = grazUNn/SecPerDay
    dydtt_diag(igrazUN15n)       = grazUN15n/SecPerDay
    dydtt_diag(igrazUNp)         = grazUNp/SecPerDay
    dydtt_diag(igrowBAldoc)      = growBAldoc/SecPerDay
    dydtt_diag(igrowBAldon)      = growBAldon/SecPerDay
    dydtt_diag(igrowBA15ldon)    = growBA15ldon/SecPerDay
    dydtt_diag(igrowBAldop)      = growBAldop/SecPerDay
    dydtt_diag(igrowBAsdoc)      = growBAsdoc/SecPerDay
    dydtt_diag(igrowBAsdon)      = growBAsdon/SecPerDay
    dydtt_diag(igrowBA15sdon)    = growBA15sdon/SecPerDay
    dydtt_diag(igrowBAsdop)      = growBAsdop/SecPerDay
    dydtt_diag(igrowBAnh4)       = growBAnh4/SecPerDay
    dydtt_diag(igrowBAno3)       = growBAno3/SecPerDay
    dydtt_diag(igrowBA15nh4)     = growBA15nh4/SecPerDay
    dydtt_diag(igrowBA15no3)     = growBA15no3/SecPerDay
    dydtt_diag(igrowBApo4)       = growBApo4/SecPerDay
    dydtt_diag(igrowBAc)         = growBAc/SecPerDay
    dydtt_diag(igrowBAn)         = growBAn/SecPerDay
    dydtt_diag(igrowBA15n)       = growBA15n/SecPerDay
    dydtt_diag(igrowBAp)         = growBAp/SecPerDay
    dydtt_diag(irespBA)          = respBA/SecPerDay
    dydtt_diag(irefrBAc)         = refrBAc/SecPerDay
    dydtt_diag(irefrBAn)         = refrBAn/SecPerDay
    dydtt_diag(irefrBA15n)       = refrBA15n/SecPerDay
    dydtt_diag(irefrBAp)         = refrBAp/SecPerDay
    dydtt_diag(iexcrBAc)         = excrBAc/SecPerDay
    dydtt_diag(iexcrBAn)         = excrBAn/SecPerDay
    dydtt_diag(iexcrBA15n)       = excrBA15n/SecPerDay
    dydtt_diag(iexcrBAp)         = excrBAp/SecPerDay
    dydtt_diag(iremiBAn)         = remiBAn/SecPerDay
    dydtt_diag(iremiBA15n)       = remiBA15n/SecPerDay
    dydtt_diag(iremiBAp)         = remiBAp/SecPerDay
    dydtt_diag(igrazBAc)         = grazBAc/SecPerDay
    dydtt_diag(igrazBAn)         = grazBAn/SecPerDay
    dydtt_diag(igrazBA15n)       = grazBA15n/SecPerDay
    dydtt_diag(igrazBAp)         = grazBAp/SecPerDay
    dydtt_diag(imortBAc)         = mortBAc/SecPerDay
    dydtt_diag(imortBAn)         = mortBAn/SecPerDay
    dydtt_diag(imortBA15n)       = mortBA15n/SecPerDay
    dydtt_diag(imortBAp)         = mortBAp/SecPerDay
    dydtt_diag(ifluxBAnh4)       = fluxBAnh4/SecPerDay
    dydtt_diag(ifluxBA15nh4)     = fluxBA15nh4/SecPerDay
    dydtt_diag(ifluxBApo4)       = fluxBApo4/SecPerDay
    dydtt_diag(igrowPRTc)        = growPRTc/SecPerDay
    dydtt_diag(igrowPRTn)        = growPRTn/SecPerDay
    dydtt_diag(igrowPRT15n)      = growPRT15n/SecPerDay
    dydtt_diag(igrowPRTp)        = growPRTp/SecPerDay
    dydtt_diag(irespPRT)         = respPRT/SecPerDay
    dydtt_diag(iexcrPRTldomc)    = excrPRTldomc/SecPerDay
    dydtt_diag(iexcrPRTldomn)    = excrPRTldomn/SecPerDay
    dydtt_diag(iexcrPRTldom15n)  = excrPRTldom15n/SecPerDay
    dydtt_diag(iexcrPRTldomp)    = excrPRTldomp/SecPerDay
    dydtt_diag(iexcrPRTsdomc)    = excrPRTsdomc/SecPerDay
    dydtt_diag(iexcrPRTsdomn)    = excrPRTsdomn/SecPerDay
    dydtt_diag(iexcrPRTsdom15n)  = excrPRTsdom15n/SecPerDay
    dydtt_diag(iexcrPRTsdomp)    = excrPRTsdomp/SecPerDay
    dydtt_diag(iexcrPRTsdom2c)   = excrPRTsdom2c/SecPerDay
    dydtt_diag(iexcrPRTsdom2n)   = excrPRTsdom2n/SecPerDay
    dydtt_diag(iexcrPRTsdom2_15n) = excrPRTsdom2_15n/SecPerDay
    dydtt_diag(iexcrPRTsdom2p)   = excrPRTsdom2p/SecPerDay
    dydtt_diag(iremiPRTn)        = remiPRTn/SecPerDay
    dydtt_diag(iremiPRT15n)      = remiPRT15n/SecPerDay
    dydtt_diag(iremiPRTp)        = remiPRTp/SecPerDay
    dydtt_diag(ipomPRTc)         = pomPRTc/SecPerDay
    dydtt_diag(ipomPRTn)         = pomPRTn/SecPerDay
    dydtt_diag(ipomPRT15n)       = pomPRT15n/SecPerDay
    dydtt_diag(ipomPRTp)         = pomPRTp/SecPerDay
    dydtt_diag(igrazPRTc)        = grazPRTc/SecPerDay
    dydtt_diag(igrazPRTn)        = grazPRTn/SecPerDay
    dydtt_diag(igrazPRT15n)      = grazPRT15n/SecPerDay
    dydtt_diag(igrazPRTp)        = grazPRTp/SecPerDay
    dydtt_diag(igrowMZc)         = growMZc/SecPerDay
    dydtt_diag(igrowMZn)         = growMZn/SecPerDay
    dydtt_diag(igrowMZ15n)       = growMZ15n/SecPerDay
    dydtt_diag(igrowMZp)         = growMZp/SecPerDay
    dydtt_diag(irespMZ)          = respMZ/SecPerDay
    dydtt_diag(iexcrMZldomc)     = excrMZldomc/SecPerDay
    dydtt_diag(iexcrMZldomn)     = excrMZldomn/SecPerDay
    dydtt_diag(iexcrMZldom15n)   = excrMZldom15n/SecPerDay
    dydtt_diag(iexcrMZldomp)     = excrMZldomp/SecPerDay
    dydtt_diag(iexcrMZsdomc)     = excrMZsdomc/SecPerDay
    dydtt_diag(iexcrMZsdomn)     = excrMZsdomn/SecPerDay
    dydtt_diag(iexcrMZsdom15n)   = excrMZsdom15n/SecPerDay
    dydtt_diag(iexcrMZsdomp)     = excrMZsdomp/SecPerDay
    dydtt_diag(iexcrMZsdom2c)    = excrMZsdom2c/SecPerDay
    dydtt_diag(iexcrMZsdom2n)    = excrMZsdom2n/SecPerDay
    dydtt_diag(iexcrMZsdom2_15n)  = excrMZsdom2_15n/SecPerDay
    dydtt_diag(iexcrMZsdom2p)    = excrMZsdom2p/SecPerDay
    dydtt_diag(iremiMZn)         = remiMZn/SecPerDay
    dydtt_diag(iremiMZ15n)       = remiMZ15n/SecPerDay
    dydtt_diag(iremiMZp)         = remiMZp/SecPerDay
    dydtt_diag(ipomMZc)          = pomMZc/SecPerDay
    dydtt_diag(ipomMZn)          = pomMZn/SecPerDay
    dydtt_diag(ipomMZ15n)        = pomMZ15n/SecPerDay
    dydtt_diag(ipomMZp)          = pomMZp/SecPerDay
    dydtt_diag(iremvMZc)         = remvMZc/SecPerDay
    dydtt_diag(iremvMZn)         = remvMZn/SecPerDay
    dydtt_diag(iremvMZ15n)        = remvMZ15n/SecPerDay
    dydtt_diag(iremvMZp)         = remvMZp/SecPerDay
    dydtt_diag(ipomHZc)          = pomHZc/SecPerDay
    dydtt_diag(ipomHZn)          = pomHZn/SecPerDay
    dydtt_diag(ipomHZ15n)        = pomHZ15n/SecPerDay
    dydtt_diag(ipomHZp)          = pomHZp/SecPerDay
    dydtt_diag(iexcrHZsdomc)     = excrHZsdomc/SecPerDay
    dydtt_diag(iexcrHZsdomn)     = excrHZsdomn/SecPerDay
    dydtt_diag(iexcrHZsdom15n)   = excrHZsdom15n/SecPerDay
    dydtt_diag(iexcrHZsdomp)     = excrHZsdomp/SecPerDay
    dydtt_diag(iremiHZn)         = remiHZn/SecPerDay
    dydtt_diag(iremiHZ15n)       = remiHZ15n/SecPerDay
    dydtt_diag(iremiHZp)         = remiHZp/SecPerDay
    dydtt_diag(idisDETc)         = disDETc/SecPerDay
    dydtt_diag(idisDETn)         = disDETn/SecPerDay
    dydtt_diag(idisDET15n)       = disDET15n/SecPerDay
    dydtt_diag(idisDETp)         = disDETp/SecPerDay
    dydtt_diag(initrf)           = nitrf/SecPerDay
    dydtt_diag(initrf15)         = nitrf15/SecPerDay
    dydtt_diag(iexportc)         = wnsvo*wnsvflag(iDETc) * y(iDETc) /SecPerDay
    dydtt_diag(iexportn)         = wnsvo*wnsvflag(iDETn) * y(iDETn) /SecPerDay
    dydtt_diag(iexport15n)       = wnsvo*wnsvflag(iDET15n) * y(iDET15n) /SecPerDay
    dydtt_diag(iexportp)         = wnsvo*wnsvflag(iDETp) * y(iDETp) /SecPerDay
    dydtt_diag(irespSP)          = respSP/SecPerDay
    dydtt_diag(irespTR)          = respTR/SecPerDay
    dydtt_diag(irespDDA)         = respDDA/SecPerDay
    dydtt_diag(irespUN)          = respUN/SecPerDay

  end subroutine derivs


end module derivs_mod

