program summa_parameter_editor

		use strings
        real:: valuex


        integer:: summa_parameter_ident,eof, nargs2
		
		integer, parameter :: linemax=100 ! max on a line (maximum number of landuse or soil class
		real :: val(linemax)
	
	
		integer:: mm


		real,dimension(:),allocatable :: upperBoundHead,lowerBoundHead,upperBoundTheta,lowerBoundTheta
		real,dimension(:),allocatable :: upperBoundTemp,lowerBoundTemp
		
        real,dimension(:),allocatable :: tempCritRain,tempRangeTimestep,frozenPrecipMultip
		real,dimension(:),allocatable :: snowfrz_scale,fixedThermalCond_snow,albedoMax
        real,dimension(:),allocatable :: albedoMinWinter,albedoMinSpring,albedoMaxVisible
		real,dimension(:),allocatable :: albedoMinVisible,albedoMaxNearIR,albedoMinNearIR
		real,dimension(:),allocatable :: albedoDecayRate
		real,dimension(:),allocatable :: albedoSootLoad,albedoRefresh

        real,dimension(:),allocatable :: radExt_snow,directScale,Frad_direct
		real,dimension(:),allocatable :: Frad_vis,newSnowDenMin,newSnowDenMult
		real,dimension(:),allocatable :: newSnowDenScal

		real,dimension(:),allocatable :: constSnowDen,newSnowDenAdd,newSnowDenMultTemp
		real,dimension(:),allocatable :: newSnowDenMultWind,newSnowDenMultAnd,newSnowDenBase
        real,dimension(:),allocatable :: densScalGrowth,tempScalGrowth
		real,dimension(:),allocatable :: grainGrowthRate,densScalOvrbdn
		real,dimension(:),allocatable :: tempScalOvrbdn
		real,dimension(:),allocatable :: baseViscosity
		real,dimension(:),allocatable :: Fcapil,k_snow
		real,dimension(:),allocatable :: mw_exp
		real,dimension(:),allocatable :: z0Snow,z0Soil
		real,dimension(:),allocatable :: z0Canopy
		real,dimension(:),allocatable :: zpdFraction,critRichNumber,Louis79_bparam
		real,dimension(:),allocatable :: Louis79_cStar,Mahrt87_eScale,leafExchangeCoeff
		real,dimension(:),allocatable :: windReductionParam,Kc25,Ko25
		real,dimension(:),allocatable :: Kc_qFac

		real,dimension(:),allocatable :: Ko_qFac, kc_Ha, ko_Ha
		real,dimension(:),allocatable :: vcmax25_canopyTop,vcmax_qFac,vcmax_Ha
		real,dimension(:),allocatable :: vcmax_Hd,vcmax_Sv,vcmax_Kn
		real,dimension(:),allocatable :: jmax25_scale,jmax_Ha,jmax_Hd
		real,dimension(:),allocatable :: jmax_Sv,fractionJ
		real,dimension(:),allocatable :: quantamYield


		real,dimension(:),allocatable :: vpScaleFactor,cond2photo_slope,minStomatalConductance
		real,dimension(:),allocatable :: winterSAI,summerLAI
		real,dimension(:),allocatable :: rootScaleFactor1
		real,dimension(:),allocatable :: rootScaleFactor2,rootingDepth
		real,dimension(:),allocatable :: rootDistExp
		
		
		real,dimension(:),allocatable :: plantWiltPsi,soilStressParam
		real,dimension(:),allocatable :: critSoilWilting
		real,dimension(:),allocatable :: critSoilTranspire
		real,dimension(:),allocatable :: critAquiferTranspire
		real,dimension(:),allocatable :: minStomatalResistance, leafDimension
		real,dimension(:),allocatable :: heightCanopyTop,heightCanopyBottom
		
		
		!------------------------------------------------------------------
		real,dimension(:),allocatable :: upperBoundHead_sampled,lowerBoundHead_sampled
		real,dimension(:),allocatable :: upperBoundTheta_sampled,lowerBoundTheta_sampled
		real,dimension(:),allocatable :: albedoSootLoad_sampled,albedoRefresh_sampled
		real,dimension(:),allocatable :: upperBoundTemp_sampled,lowerBoundTemp_sampled
        real,dimension(:),allocatable :: tempCritRain_sampled,tempRangeTimestep_sampled
		real,dimension(:),allocatable :: frozenPrecipMultip_sampled
		real,dimension(:),allocatable :: snowfrz_scale_sampled,fixedThermalCond_snow_sampled
		real,dimension(:),allocatable :: albedoMax_sampled
        real,dimension(:),allocatable :: albedoMinWinter_sampled,albedoMinSpring_sampled
		real,dimension(:),allocatable :: albedoMaxVisible_sampled
		real,dimension(:),allocatable :: albedoMinVisible_sampled,albedoMaxNearIR_sampled
		real,dimension(:),allocatable :: albedoMinNearIR_sampled
		real,dimension(:),allocatable :: albedoDecayRate_sampled

        real,dimension(:),allocatable :: radExt_snow_sampled
		real,dimension(:),allocatable :: directScale_sampled,Frad_direct_sampled
		real,dimension(:),allocatable :: Frad_vis_sampled,newSnowDenMin_sampled
		real,dimension(:),allocatable :: newSnowDenMult_sampled
		real,dimension(:),allocatable :: newSnowDenScal_sampled

		real,dimension(:),allocatable :: constSnowDen_sampled,newSnowDenAdd_sampled
		real,dimension(:),allocatable :: newSnowDenMultTemp_sampled
		real,dimension(:),allocatable :: newSnowDenMultWind_sampled,newSnowDenMultAnd_sampled
		real,dimension(:),allocatable :: newSnowDenBase_sampled
        real,dimension(:),allocatable :: densScalGrowth_sampled
		real,dimension(:),allocatable :: tempScalGrowth_sampled
		real,dimension(:),allocatable :: grainGrowthRate_sampled
		real,dimension(:),allocatable :: densScalOvrbdn_sampled
		real,dimension(:),allocatable :: tempScalOvrbdn_sampled
		real,dimension(:),allocatable :: baseViscosity_sampled
		real,dimension(:),allocatable :: Fcapil_sampled
		real,dimension(:),allocatable :: k_snow_sampled
		real,dimension(:),allocatable :: mw_exp_sampled
		real,dimension(:),allocatable :: z0Snow_sampled
		real,dimension(:),allocatable :: z0Soil_sampled
		real,dimension(:),allocatable :: z0Canopy_sampled
		real,dimension(:),allocatable :: zpdFraction_sampled,critRichNumber_sampled
		real,dimension(:),allocatable :: Louis79_bparam_sampled
		real,dimension(:),allocatable :: Louis79_cStar_sampled,Mahrt87_eScale_sampled
		real,dimension(:),allocatable :: leafExchangeCoeff_sampled
		real,dimension(:),allocatable :: windReductionParam_sampled,Kc25_sampled
		real,dimension(:),allocatable :: Ko25_sampled
		real,dimension(:),allocatable :: Kc_qFac_sampled

		real,dimension(:),allocatable :: Ko_qFac_sampled, kc_Ha_sampled
		real,dimension(:),allocatable :: ko_Ha_sampled
		real,dimension(:),allocatable :: vcmax25_canopyTop_sampled,vcmax_qFac_sampled
		real,dimension(:),allocatable :: vcmax_Ha_sampled
		real,dimension(:),allocatable :: vcmax_Hd_sampled,vcmax_Sv_sampled
		real,dimension(:),allocatable :: vcmax_Kn_sampled
		real,dimension(:),allocatable :: jmax25_scale_sampled,jmax_Ha_sampled
		real,dimension(:),allocatable :: jmax_Hd_sampled
		real,dimension(:),allocatable :: jmax_Sv_sampled
		real,dimension(:),allocatable :: fractionJ_sampled
		real,dimension(:),allocatable :: quantamYield_sampled


		real,dimension(:),allocatable :: vpScaleFactor_sampled,cond2photo_slope_sampled
		real,dimension(:),allocatable :: minStomatalConductance_sampled
		real,dimension(:),allocatable :: winterSAI_sampled
		real,dimension(:),allocatable :: summerLAI_sampled
		real,dimension(:),allocatable :: rootScaleFactor1_sampled
		real,dimension(:),allocatable :: rootScaleFactor2_sampled
		real,dimension(:),allocatable :: rootingDepth_sampled
		real,dimension(:),allocatable :: rootDistExp_sampled
		
		
		real,dimension(:),allocatable :: plantWiltPsi_sampled
		real,dimension(:),allocatable :: soilStressParam_sampled
		real,dimension(:),allocatable :: critSoilWilting_sampled
		real,dimension(:),allocatable :: critSoilTranspire_sampled
		real,dimension(:),allocatable :: critAquiferTranspire_sampled
		
		real,dimension(:),allocatable :: minStomatalResistance_sampled, leafDimension_sampled
		real,dimension(:),allocatable :: heightCanopyTop_sampled
		real,dimension(:),allocatable :: heightCanopyBottom_sampled
		
		
		!New pars added for Lake Winnipeg
		real,dimension(:),allocatable :: specificHeatVeg
		real,dimension(:),allocatable :: maxMassVegetation
		real,dimension(:),allocatable :: throughfallScaleSnow
		real,dimension(:),allocatable :: throughfallScaleRain
		real,dimension(:),allocatable :: refInterceptCapSnow
		real,dimension(:),allocatable :: refInterceptCapRain
		real,dimension(:),allocatable :: snowUnloadingCoeff
		real,dimension(:),allocatable :: canopyDrainageCoeff
		real,dimension(:),allocatable :: ratioDrip2Unloading
		real,dimension(:),allocatable :: canopyWettingFactor
		real,dimension(:),allocatable :: canopyWettingExp
		real,dimension(:),allocatable :: soil_dens_intr
		real,dimension(:),allocatable :: thCond_soil
		real,dimension(:),allocatable :: frac_sand
		real,dimension(:),allocatable :: frac_silt
		real,dimension(:),allocatable :: frac_clay
		real,dimension(:),allocatable :: fieldCapacity
		real,dimension(:),allocatable :: wettingFrontSuction
		real,dimension(:),allocatable :: theta_mp
		real,dimension(:),allocatable :: theta_sat
		real,dimension(:),allocatable :: theta_res
		real,dimension(:),allocatable :: vGn_alpha
		real,dimension(:),allocatable :: vGn_n
		real,dimension(:),allocatable :: mpExp
		real,dimension(:),allocatable :: k_soil
		real,dimension(:),allocatable :: k_macropore
		real,dimension(:),allocatable :: kAnisotropic
		real,dimension(:),allocatable :: zScale_TOPMODEL
		real,dimension(:),allocatable :: compactedDepth
		real,dimension(:),allocatable :: aquiferBaseflowRate
		real,dimension(:),allocatable :: aquiferScaleFactor
		real,dimension(:),allocatable :: aquiferBaseflowExp
		real,dimension(:),allocatable :: qSurfScale
		real,dimension(:),allocatable :: specificYield
		real,dimension(:),allocatable :: specificStorage
		real,dimension(:),allocatable :: f_impede
		real,dimension(:),allocatable :: soilIceScale
		real,dimension(:),allocatable :: soilIceCV
		real,dimension(:),allocatable :: minwind
		real,dimension(:),allocatable :: minstep
		real,dimension(:),allocatable :: maxstep
		real,dimension(:),allocatable :: wimplicit
		real,dimension(:),allocatable :: maxiter
		real,dimension(:),allocatable :: relConvTol_liquid
		real,dimension(:),allocatable :: absConvTol_liquid
		real,dimension(:),allocatable :: relConvTol_matric
		real,dimension(:),allocatable :: absConvTol_matric
		real,dimension(:),allocatable :: relConvTol_energy
		real,dimension(:),allocatable :: absConvTol_energy
		real,dimension(:),allocatable :: relConvTol_aquifr
		real,dimension(:),allocatable :: absConvTol_aquifr
		real,dimension(:),allocatable :: zmin
		real,dimension(:),allocatable :: zmax
		real,dimension(:),allocatable :: zminLayer1
		real,dimension(:),allocatable :: zminLayer2
		real,dimension(:),allocatable :: zminLayer3
		real,dimension(:),allocatable :: zminLayer4
		real,dimension(:),allocatable :: zminLayer5
		real,dimension(:),allocatable :: zmaxLayer1_lower
		real,dimension(:),allocatable :: zmaxLayer2_lower
		real,dimension(:),allocatable :: zmaxLayer3_lower
		real,dimension(:),allocatable :: zmaxLayer4_lower
		real,dimension(:),allocatable :: zmaxLayer1_upper
		real,dimension(:),allocatable :: zmaxLayer2_upper
		real,dimension(:),allocatable :: zmaxLayer3_upper
		real,dimension(:),allocatable :: zmaxLayer4_upper
		real,dimension(:),allocatable :: minTempUnloading
		real,dimension(:),allocatable :: minWindUnloading
		real,dimension(:),allocatable :: rateTempUnloading
		real,dimension(:),allocatable :: rateWindUnloading
		
		real,dimension(:),allocatable :: relTolTempCas
		real,dimension(:),allocatable :: absTolTempCas
		real,dimension(:),allocatable :: relTolTempVeg
		real,dimension(:),allocatable :: absTolTempVeg
		real,dimension(:),allocatable :: relTolWatVeg
		real,dimension(:),allocatable :: absTolWatVeg
		real,dimension(:),allocatable :: relTolTempSoilSnow
		real,dimension(:),allocatable :: absTolTempSoilSnow
		real,dimension(:),allocatable :: relTolWatSnow
		real,dimension(:),allocatable :: absTolWatSnow
		real,dimension(:),allocatable :: relTolMatric
		real,dimension(:),allocatable :: absTolMatric
		real,dimension(:),allocatable :: relTolAquifr
		real,dimension(:),allocatable :: absTolAquifr
		
		real,dimension(:),allocatable :: ep_corr
		real,dimension(:),allocatable :: lowerBoundHead_corr
		real,dimension(:),allocatable :: surfm_corr
		real,dimension(:),allocatable :: deprl_corr
		!real,dimension(:),allocatable :: albedoMinWinter
		!real,dimension(:),allocatable :: newSnowDenMult
		real,dimension(:),allocatable :: upper2deep
		real,dimension(:),allocatable :: tcfriver
		real,dimension(:),allocatable :: scfriver
		real,dimension(:),allocatable :: ccfriver
		real,dimension(:),allocatable :: lcfriver
		real,dimension(:),allocatable :: tcflake
		real,dimension(:),allocatable :: scflake
		real,dimension(:),allocatable :: ccflake
		real,dimension(:),allocatable :: lcflake
		real,dimension(:),allocatable :: stbcorr1
		real,dimension(:),allocatable :: stbcorr2
		real,dimension(:),allocatable :: licewme
		real,dimension(:),allocatable :: licetf
		real,dimension(:),allocatable :: licesndens
		real,dimension(:),allocatable :: licekika
		real,dimension(:),allocatable :: licekexp
		real,dimension(:),allocatable :: licetmelt
		real,dimension(:),allocatable :: licewcorr
		real,dimension(:),allocatable :: ricewme
		real,dimension(:),allocatable :: ricetf
		real,dimension(:),allocatable :: ricesndens
		real,dimension(:),allocatable :: ricekika
		real,dimension(:),allocatable :: ricekexp
		real,dimension(:),allocatable :: ricetmelt
		
		
		!Basin parameters
		
		real,dimension(:),allocatable :: basin__aquiferHydCond
		real,dimension(:),allocatable :: routingGammaShape
		real,dimension(:),allocatable :: routingGammaScale
		real,dimension(:),allocatable :: basin__aquiferScaleFactor
		real,dimension(:),allocatable :: basin__aquiferBaseflowExp
		
		
		
		real,dimension(:),allocatable :: specificHeatVeg_sampled
		real,dimension(:),allocatable :: maxMassVegetation_sampled		
		real,dimension(:),allocatable :: throughfallScaleSnow_sampled		
		real,dimension(:),allocatable :: throughfallScaleRain_sampled		
		real,dimension(:),allocatable :: refInterceptCapSnow_sampled		
		real,dimension(:),allocatable :: refInterceptCapRain_sampled		
		real,dimension(:),allocatable :: snowUnloadingCoeff_sampled		
		real,dimension(:),allocatable :: canopyDrainageCoeff_sampled
        real,dimension(:),allocatable :: ratioDrip2Unloading_sampled
        real,dimension(:),allocatable :: canopyWettingFactor_sampled
        real,dimension(:),allocatable :: canopyWettingExp_sampled		
		real,dimension(:),allocatable :: soil_dens_intr_sampled		
		real,dimension(:),allocatable :: thCond_soil_sampled		
		real,dimension(:),allocatable :: frac_sand_sampled		
		real,dimension(:),allocatable :: frac_silt_sampled		
		real,dimension(:),allocatable :: frac_clay_sampled		
		real,dimension(:),allocatable :: fieldCapacity_sampled		
		real,dimension(:),allocatable :: wettingFrontSuction_sampled		
		real,dimension(:),allocatable :: theta_mp_sampled		
		real,dimension(:),allocatable :: theta_sat_sampled		
		real,dimension(:),allocatable :: theta_res_sampled		
		real,dimension(:),allocatable :: vGn_alpha_sampled		
		real,dimension(:),allocatable :: vGn_n_sampled		
		real,dimension(:),allocatable :: mpExp_sampled		
		real,dimension(:),allocatable :: k_soil_sampled		
		real,dimension(:),allocatable :: k_macropore_sampled		
		real,dimension(:),allocatable :: kAnisotropic_sampled		
		real,dimension(:),allocatable :: zScale_TOPMODEL_sampled		
		real,dimension(:),allocatable :: compactedDepth_sampled		
		real,dimension(:),allocatable :: aquiferBaseflowRate_sampled		
		real,dimension(:),allocatable :: aquiferScaleFactor_sampled		
		real,dimension(:),allocatable :: aquiferBaseflowExp_sampled		
		real,dimension(:),allocatable :: qSurfScale_sampled		
		real,dimension(:),allocatable :: specificYield_sampled		
		real,dimension(:),allocatable :: specificStorage_sampled		
		real,dimension(:),allocatable :: f_impede_sampled		
		real,dimension(:),allocatable :: soilIceScale_sampled		
		real,dimension(:),allocatable :: soilIceCV_sampled		
		real,dimension(:),allocatable :: minwind_sampled		
		real,dimension(:),allocatable :: minstep_sampled		
		real,dimension(:),allocatable :: maxstep_sampled		
		real,dimension(:),allocatable :: wimplicit_sampled		
		real,dimension(:),allocatable :: maxiter_sampled		
		real,dimension(:),allocatable :: relConvTol_liquid_sampled		
		real,dimension(:),allocatable :: absConvTol_liquid_sampled		
		real,dimension(:),allocatable :: relConvTol_matric_sampled		
		real,dimension(:),allocatable :: absConvTol_matric_sampled		
		real,dimension(:),allocatable :: relConvTol_energy_sampled		
		real,dimension(:),allocatable :: absConvTol_energy_sampled		
		real,dimension(:),allocatable :: relConvTol_aquifr_sampled		
		real,dimension(:),allocatable :: absConvTol_aquifr_sampled		
		real,dimension(:),allocatable :: zmin_sampled		
		real,dimension(:),allocatable :: zmax_sampled		
		real,dimension(:),allocatable :: zminLayer1_sampled		
		real,dimension(:),allocatable :: zminLayer2_sampled		
		real,dimension(:),allocatable :: zminLayer3_sampled		
		real,dimension(:),allocatable :: zminLayer4_sampled		
		real,dimension(:),allocatable :: zminLayer5_sampled		
		real,dimension(:),allocatable :: zmaxLayer1_lower_sampled		
		real,dimension(:),allocatable :: zmaxLayer2_lower_sampled		
		real,dimension(:),allocatable :: zmaxLayer3_lower_sampled		
		real,dimension(:),allocatable :: zmaxLayer4_lower_sampled		
		real,dimension(:),allocatable :: zmaxLayer1_upper_sampled		
		real,dimension(:),allocatable :: zmaxLayer2_upper_sampled		
		real,dimension(:),allocatable :: zmaxLayer3_upper_sampled		
		real,dimension(:),allocatable :: zmaxLayer4_upper_sampled		
		real,dimension(:),allocatable :: minTempUnloading_sampled		
		real,dimension(:),allocatable :: minWindUnloading_sampled		
		real,dimension(:),allocatable :: rateTempUnloading_sampled		
		real,dimension(:),allocatable :: rateWindUnloading_sampled

		real,dimension(:),allocatable :: relTolTempCas_sampled
		real,dimension(:),allocatable :: absTolTempCas_sampled
		real,dimension(:),allocatable :: relTolTempVeg_sampled
		real,dimension(:),allocatable :: absTolTempVeg_sampled
		real,dimension(:),allocatable :: relTolWatVeg_sampled
		real,dimension(:),allocatable :: absTolWatVeg_sampled
		real,dimension(:),allocatable :: relTolTempSoilSnow_sampled
		real,dimension(:),allocatable :: absTolTempSoilSnow_sampled
		real,dimension(:),allocatable :: relTolWatSnow_sampled
		real,dimension(:),allocatable :: absTolWatSnow_sampled
		real,dimension(:),allocatable :: relTolMatric_sampled
		real,dimension(:),allocatable :: absTolMatric_sampled
		real,dimension(:),allocatable :: relTolAquifr_sampled
		real,dimension(:),allocatable :: absTolAquifr_sampled
		
		real,dimension(:),allocatable :: ep_corr_sampled		
		real,dimension(:),allocatable :: lowerBoundHead_corr_sampled		
		real,dimension(:),allocatable :: surfm_corr_sampled		
		real,dimension(:),allocatable :: deprl_corr_sampled		
		!real,dimension(:),allocatable :: albedoMinWinter_sampled		
		!real,dimension(:),allocatable :: newSnowDenMult_sampled		
		real,dimension(:),allocatable :: upper2deep_sampled		
		real,dimension(:),allocatable :: tcfriver_sampled		
		real,dimension(:),allocatable :: scfriver_sampled		
		real,dimension(:),allocatable :: ccfriver_sampled		
		real,dimension(:),allocatable :: lcfriver_sampled		
		real,dimension(:),allocatable :: tcflake_sampled		
		real,dimension(:),allocatable :: scflake_sampled		
		real,dimension(:),allocatable :: ccflake_sampled		
		real,dimension(:),allocatable :: lcflake_sampled		
		real,dimension(:),allocatable :: stbcorr1_sampled		
		real,dimension(:),allocatable :: stbcorr2_sampled		
		real,dimension(:),allocatable :: licewme_sampled		
		real,dimension(:),allocatable :: licetf_sampled		
		real,dimension(:),allocatable :: licesndens_sampled		
		real,dimension(:),allocatable :: licekika_sampled		
		real,dimension(:),allocatable :: licekexp_sampled		
		real,dimension(:),allocatable :: licetmelt_sampled		
		real,dimension(:),allocatable :: licewcorr_sampled		
		real,dimension(:),allocatable :: ricewme_sampled		
		real,dimension(:),allocatable :: ricetf_sampled		
		real,dimension(:),allocatable :: ricesndens_sampled		
		real,dimension(:),allocatable :: ricekika_sampled		
		real,dimension(:),allocatable :: ricekexp_sampled		
		real,dimension(:),allocatable :: ricetmelt_sampled
		
		
		!Basin parameters resampled
		
		real,dimension(:),allocatable :: basin__aquiferHydCond_sampled
		real,dimension(:),allocatable :: routingGammaShape_sampled
		real,dimension(:),allocatable :: routingGammaScale_sampled
		
		real,dimension(:),allocatable :: basin__aquiferScaleFactor_sampled
		real,dimension(:),allocatable :: basin__aquiferBaseflowExp_sampled
		
		
		!------------------------------------------------------------------
		


        real,dimension(:),allocatable :: parameters,parname_modelin
		real,dimension(:,:),allocatable :: multipliers

        character(len=30):: parnamex,parname,parnamexx,parname2

		character(len=30), allocatable :: parnamex_list(:)
		character(len=30), allocatable :: parname_list(:)
		character(len=30), allocatable :: par_values(:)
		character*4000:: line, line2 ! the line buffer
		
		character(len=1):: delims,xx,xx1,xx2,xx3,char1
		integer,parameter:: StrMax=30,Nmax=100
		character(len=StrMax), dimension(Nmax):: args
		



		
        integer :: pos,  mis_count, n, gg, countPar,i,k,j,No_pars
		integer :: counter2
        
		real:: number1, total_parameterts

        allocate(parameters(250))
        parameters(:)= 0.
		val= -99.
		delims = ' '
		
		allocate(character(30) :: parnamex_list(250))
		allocate(character(30) :: parname_list(250))
		!character*80:: parnamex_list(linemax)
		!character*80:: parname_list(linemax)

		parname_list(:) = " "
		parnamex_list(:) = " "

        open(100,file="model.in", status = "old")

		counter = 0
		eof=0
		No_pars = 0
		do
			read(100,*,iostat=eof) parnamex,valuex
			if (eof<0) exit
			counter=counter+1
			
			
			!if((parnamex.NE.'dummy').OR.(parnamex.ne.'heightCanopyBottom')) then
			if(parnamex.NE.'dummy') then
			

				
				parameters(summa_parameter_ident(trim(parnamex))) = valuex

		parnamex_list(summa_parameter_ident(trim(parnamex)))=trim(parnamex)
				parname_list(summa_parameter_ident(trim(parnamex))) = 		&
	 &			parnamex(1:(len(trim(parnamex))-1))


			end if


			
			
		end do
		
		No_pars = counter

		close(100)
		
		!! Count the total number of parameters in the default_par.txt
		
		eof=0
		total_parameterts = 0
		
		open(1000,file='localParamInfo_default_sundials.txt',status='old')
		open(2000,file='basinParamInfo_default.txt',status='old')
		!read (1000, *)
		
		
		do


			read (1000, '(A)', iostat = eof)line
			if (eof < 0) exit
			line = adjustl(line)
            read(line,'(a1)')char1
            if((char1.ne.'!').or.(char1.ne."'")) then
			  total_parameterts = total_parameterts + 1
			end if
		
		end do
		
		close(1000)
		close(2000)

        !!default parameters

		


		open(1000,file='localParamInfo_default_sundials.txt',status='old')
		open(2000,file='basinParamInfo_default.txt',status='old')
		!read (1000, *)
		
		 open(200,file="localParamInfo.txt",status="replace")

        write(200,'(a121)')"! ======================================================================================================================="
	 
	    write(200,'(a121)')"! ======================================================================================================================="

	 
	    write(200,'(a121)')"! ===== DEFINITION OF MODEL PARAMETERS =================================================================================="
	 
	    write(200,'(a121)')"! ======================================================================================================================="
	 
	 
	    write(200,'(a121)')"! ======================================================================================================================="
	 
	 
	    write(200,'(a110)')"! Note: lines starting with ! are treated as comment lines -- there is no limit on the number ofcomment lines."
	 
	    write(200,'(a1)')"!"
		write(200,'(a121)')"! ======================================================================================================================="
	 
	    write(200,'(a121)')"! DEFINE SITE MODEL PARAMETERS==========================================================================================="
	 
	    write(200,'(a38)')"! ------------------------------------"
		
		write(200,'(a76)')"! the format definition defines the format of the file, which can be changed"
		
		write(200,'(a101)')"! the delimiters |  must be present (format a2), as these are used to check the integrety of the file"
	 
	    write(200,'(a14)')"! columns are:"
		write(200,'(a19)')"! 1: parameter name"
		write(200,'(a28)')"! 2: default parameter value"
		write(200,'(a26)')"! 3: lower parameter limit"
		write(200,'(a26)')"! 4: upper parameter limit"
		
		write(200,'(a121)')"! ======================================================================================================================="
	 
	 
	    write(200,'(a1)')"!"
		write(200,'(a70)')"! ===================================================================="
	 
	    write(200,'(a49)')"! define format string for parameter descriptions"
	 
	    write(200,'(a70)')"! ===================================================================="
	 
	    write(200,'(a71)')"'(a25,1x,3(a1,1x,f12.4,1x))' ! format string (must be in single quotes)"
	 
	    write(200,'(a70)')"! ===================================================================="
	 
	    write(200,'(a21)')"! boundary conditions"
		
		write(200,'(a70)')"! ===================================================================="
		
		
		
		
		
		        open(300,file="basinParamInfo.txt",status="replace")

        write(300,'(a121)')"! ***********************************************************************************************************************"
	 
	    write(300,'(a121)')"! ***********************************************************************************************************************"

	 
	    write(300,'(a121)')"! ***** DEFINITION OF BASIN PARAMETERS **********************************************************************************"
	 
	    write(300,'(a121)')"! ***********************************************************************************************************************"
	 
	 
	    write(300,'(a121)')"! ***********************************************************************************************************************"
	 
	 
	    write(300,'(a110)')"! Note: lines starting with ! are treated as comment lines -- there is no limit on the number ofcomment lines."
	 
	    write(300,'(a1)')"!"
		write(300,'(a121)')"! ***********************************************************************************************************************"
	 
	    write(300,'(a121)')"! DEFINE BASIN MODEL PARAMETERS******************************************************************************************"
	 
	    write(300,'(a38)')"! ------------------------------------"
		
		write(300,'(a76)')"! the format definition defines the format of the file, which can be changed"
		
		write(300,'(a101)')"! the delimiters |  must be present (format a2), as these are used to check the integrety of the file"
	 
	    write(300,'(a14)')"! columns are:"
		write(300,'(a19)')"! 1: parameter name"
		write(300,'(a28)')"! 2: default parameter value"
		write(300,'(a26)')"! 3: lower parameter limit"
		write(300,'(a26)')"! 4: upper parameter limit"
		
		write(300,'(a121)')"! ***********************************************************************************************************************"
	 
	 
	    write(300,'(a1)')"!"
		write(300,'(a70)')"! ********************************************************************"
	 
	    write(300,'(a49)')"! define format string for parameter descriptions"
	 
	    write(300,'(a70)')"! ********************************************************************"
	 
	    write(300,'(a71)')"'(a25,1x,3(a1,1x,f12.4,1x))' ! format string (must be in single quotes)"
	 
	    write(300,'(a70)')"! ********************************************************************"
	 
	    write(300,'(a10)')"! baseflow"
		
		write(300,'(a70)')"! ********************************************************************"
		
		
		
		

		!i=0
		eof=0
		xx = '_'                                   !The trailing character put after the parameter name in "model.in" file.
		
		
		
		!Check if the parameter names in the model.in file are recognizable by the parameter editor
		
		open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if(parname_list(summa_parameter_ident(trim(parname2))) .eq. " ") then
			write(*,*)"Warning! parameter ", parname2, "is unrecognizable by the parameter editor"
			end if
			
		
			
			
		end do
		close(100)
		
		do


			read (1000, '(A)', iostat = eof) line
			!write(*,*)"printing the line chunk"
			!write(*,*)line
			if (eof < 0) exit
			
			line = adjustl(line)
            read(line,'(a1)')char1
			char1 = trim(char1)
			!write(*,*)"printing char1"
			!write(*,*)char1
			
            if((char1.ne.'!').or.(char1.ne."'")) then                              !skip the lines that do not begin with a parameter name (begin)
			  
			
			
			
			
			call parse(line,delims,args,nargs)
			
			parname = trim(args(1))
			!write(*,*)"printing parname"
			!write(*,*)parname
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			parnamexx = trim(parname) // trim(xx)
			!write(*,*)"printing parnamexx"
			!write(*,*)parnamexx	
				
			counter2 = 0
			
			
			if (parname.eq.'minStomatalResistance') then
				allocate(minStomatalResistance(nargs-3))
				read (line2, *, end = 999)xx1,                     &
	 &          minStomatalResistance(1),xx2,                      &
	 &          minStomatalResistance(2),xx3,                      &
	 &          minStomatalResistance(3)
				
				
				
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
		
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'minStomatalResistance_') then
				
				allocate(minStomatalResistance_sampled(nargs))
				read (line2, *, end = 999)minStomatalResistance_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",    &
	 &      minStomatalResistance_sampled(1),"|",                  &
	 &      minStomatalResistance_sampled(2),"|",                  &
	 &      minStomatalResistance_sampled(3)
			
				deallocate(minStomatalResistance_sampled)
				
					counter2 = counter2 + 1
			
			end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          minStomatalResistance(1),"|",                   &
	 &          minStomatalResistance(2),"|",                   &
	 &          minStomatalResistance(3)
			end if
			
			deallocate(minStomatalResistance)

			
			else if (parname.eq.'leafDimension') then
				allocate(leafDimension(nargs-3))
				read (line2, *, end = 999)xx1,                     &
	 &          leafDimension(1),xx2,                      &
	 &          leafDimension(2),xx3,                      &
	 &          leafDimension(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
		
		
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'leafDimension_') then
				
				allocate(leafDimension_sampled(nargs))
				read (line2, *, end = 999)leafDimension_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      leafDimension_sampled(1),"|",                               &
	 &      leafDimension_sampled(2),"|",                               &
	 &      leafDimension_sampled(3)
					counter2 = counter2 + 1
					
			deallocate(leafDimension_sampled)
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          leafDimension(1),"|",                                   &
	 &          leafDimension(2),"|",                                   &
	 &          leafDimension(3)
			end if
			
			deallocate(leafDimension)
			
			else if (parname.eq.'upperBoundHead') then
				allocate(upperBoundHead(nargs-3))
				read (line2, *, end = 999)xx1,upperBoundHead(1),        &
	 &          xx2, upperBoundHead(2),xx3,upperBoundHead(3)
			
			
			
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'upperBoundHead_') then
				
				allocate(upperBoundHead_sampled(nargs))
				read (line2, *, end = 999)upperBoundHead_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      upperBoundHead_sampled(1),"|",                              &
	 &      upperBoundHead_sampled(2),"|",                              &
	 &      upperBoundHead_sampled(3)
			
				deallocate(upperBoundHead_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          upperBoundHead(1),"|",                                  &
	 &          upperBoundHead(2),"|",                                  &
	 &          upperBoundHead(3)
			end if
			
			deallocate(upperBoundHead)

			else if (parname.eq.'lowerBoundHead') then
				allocate(lowerBoundHead(nargs-3))
				read (line2, *, end = 999)xx1,lowerBoundHead(1),        &
	 &          xx2, lowerBoundHead(2),xx3,lowerBoundHead(3)
			
			
			
			counter2 = 0
			
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			
			call parse(line2,delims,args,nargs)
			
					
			
			
			if (parname2.eq.'lowerBoundHead_') then
				
				allocate(lowerBoundHead_sampled(nargs))
				read (line2, *, end = 999)lowerBoundHead_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      lowerBoundHead_sampled(1),"|",                              &
	 &      lowerBoundHead_sampled(2),"|",                              &
	 &      lowerBoundHead_sampled(3)
			
				deallocate(lowerBoundHead_sampled)
			

					counter2 = counter2 + 1
					
					
		end if
			
			if(counter2 == 0) then
				
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          lowerBoundHead(1),"|",                                  &
	 &          lowerBoundHead(2),"|",                                  &
	 &          lowerBoundHead(3)
				
			end if
			
			deallocate(lowerBoundHead)

			else if (parname.eq.'upperBoundTheta') then
				allocate(upperBoundTheta(nargs-3))
				read (line2, *, end = 999)xx1,upperBoundTheta(1),       &
	 &          xx2, upperBoundTheta(2),xx3,upperBoundTheta(3)
			
			
			
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'upperBoundTheta_') then
				
				allocate(upperBoundTheta_sampled(nargs))
				read (line2, *, end = 999)upperBoundTheta_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      upperBoundTheta_sampled(1),"|",                             &
	 &      upperBoundTheta_sampled(2),"|",                             &
	 &      upperBoundTheta_sampled(3)
			
				deallocate(upperBoundTheta_sampled)
			

			
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          upperBoundTheta(1),"|",                                 &
	 &          upperBoundTheta(2),"|",                                 &
	 &          upperBoundTheta(3)
			end if
			
			deallocate(upperBoundTheta)
			
		else if (parname.eq.'albedoSootLoad') then
				allocate(albedoSootLoad(nargs-3))
				read (line2, *, end = 999)xx1,albedoSootLoad(1),        &
	 &          xx2, albedoSootLoad(2),xx3,albedoSootLoad(3)
			
			
			
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'albedoSootLoad_') then
				
				allocate(albedoSootLoad_sampled(nargs))
				read (line2, *, end = 999)albedoSootLoad_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      albedoSootLoad_sampled(1),"|",                              &
	 &      albedoSootLoad_sampled(2),"|",                              &
	 &      albedoSootLoad_sampled(3)
			
				deallocate(albedoSootLoad_sampled)
			

			
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          albedoSootLoad(1),"|",                                  &
	 &          albedoSootLoad(2),"|",                                  &
	 &          albedoSootLoad(3)
			end if
			
			deallocate(albedoSootLoad)
			
			
			else if (parname.eq.'albedoRefresh') then
				allocate(albedoRefresh(nargs-3))
				read (line2, *, end = 999)xx1,albedoRefresh(1),         &
	 &          xx2, albedoRefresh(2),xx3,albedoRefresh(3)
			
			
			
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'albedoRefresh_') then
				
				allocate(albedoRefresh_sampled(nargs))
				read (line2, *, end = 999)albedoRefresh_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      albedoRefresh_sampled(1),"|",                               &
	 &      albedoRefresh_sampled(2),"|",                               &
	 &      albedoRefresh_sampled(3)
			
				deallocate(albedoRefresh_sampled)
			

			
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          albedoRefresh(1),"|",                                   &
	 &          albedoRefresh(2),"|",                                   &
	 &          albedoRefresh(3)
			end if
			
			deallocate(albedoRefresh)

			else if (parname.eq.'lowerBoundTheta') then
				allocate(lowerBoundTheta(nargs-3))
				read (line2, *, end = 999)xx1,lowerBoundTheta(1),       &
	 &          xx2, lowerBoundTheta(2),xx3,lowerBoundTheta(3)
			
			counter2 = 0
			
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'lowerBoundTheta_') then
				
				allocate(lowerBoundTheta_sampled(nargs))
				read (line2, *, end = 999)lowerBoundTheta_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      lowerBoundTheta_sampled(1),"|",                             &
	 &      lowerBoundTheta_sampled(2),"|",                             &
	 &      lowerBoundTheta_sampled(3)
			
				deallocate(lowerBoundTheta_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          lowerBoundTheta(1),"|",                                 &
	 &          lowerBoundTheta(2),"|",                                 &
	 &          lowerBoundTheta(3)
			end if
			
			deallocate(lowerBoundTheta)

			else if (parname.eq.'upperBoundTemp') then
				allocate(upperBoundTemp(nargs-3))
				read (line2, *, end = 999)xx1,upperBoundTemp(1),        &
	 &          xx2, upperBoundTemp(2),xx3,upperBoundTemp(3)
			
	
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'upperBoundTemp_') then
				
				allocate(upperBoundTemp_sampled(nargs))
				read (line2, *, end = 999)upperBoundTemp_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      upperBoundTemp_sampled(1),"|",                              &
	 &      upperBoundTemp_sampled(2),"|",                              &
	 &      upperBoundTemp_sampled(3)
			
				deallocate(upperBoundTemp_sampled)
			
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          upperBoundTemp(1),"|",                                  &
	 &          upperBoundTemp(2),"|",                                  &
	 &          upperBoundTemp(3)
			end if
			
			deallocate(upperBoundTemp)

			else if (parname.eq.'lowerBoundTemp') then
				allocate(lowerBoundTemp(nargs-3))
				read (line2, *, end = 999)xx1,lowerBoundTemp(1),        &
	 &          xx2, lowerBoundTemp(2),xx3,lowerBoundTemp(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'lowerBoundTemp_') then
				
				allocate(lowerBoundTemp_sampled(nargs))
				read (line2, *, end = 999)lowerBoundTemp_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      lowerBoundTemp_sampled(1),"|",                              &
	 &      lowerBoundTemp_sampled(2),"|",                              &
	 &      lowerBoundTemp_sampled(3)
			
				deallocate(lowerBoundTemp_sampled)
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          lowerBoundTemp(1),"|",                                  &
	 &          lowerBoundTemp(2),"|",                                  &
	 &          lowerBoundTemp(3)
			end if
			
			deallocate(lowerBoundTemp)

			else if (parname.eq.'tempCritRain') then
				allocate(tempCritRain(nargs-3))
				read (line2, *, end = 999)xx1,tempCritRain(1),        &
	 &          xx2, tempCritRain(2),xx3,tempCritRain(3)
			
			counter2 = 0
			
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'tempCritRain_') then
				
				allocate(tempCritRain_sampled(nargs))
				read (line2, *, end = 999)tempCritRain_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      tempCritRain_sampled(1),"|",                                &
	 &      tempCritRain_sampled(2),"|",                                &
	 &      tempCritRain_sampled(3)
			
				deallocate(tempCritRain_sampled)
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          tempCritRain(1),"|",                                    &
	 &          tempCritRain(2),"|",                                    &
	 &          tempCritRain(3)
			end if
			
			deallocate(tempCritRain)

			else if (parname.eq.'tempRangeTimestep') then
				allocate(tempRangeTimestep(nargs-3))
				read (line2, *, end = 999)xx1,tempRangeTimestep(1),     &
	 &          xx2, tempRangeTimestep(2),xx3,tempRangeTimestep(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'tempRangeTimestep_') then
				
				allocate(tempRangeTimestep_sampled(nargs))
				read (line2, *, end = 999)tempRangeTimestep_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      tempRangeTimestep_sampled(1),"|",                           &
	 &      tempRangeTimestep_sampled(2),"|",                           &
	 &      tempRangeTimestep_sampled(3)
			
				deallocate(tempRangeTimestep_sampled)
			
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          tempRangeTimestep(1),"|",                               &
	 &          tempRangeTimestep(2),"|",                               &
	 &          tempRangeTimestep(3)
			end if
			
			deallocate(tempRangeTimestep)

			else if (parname.eq.'frozenPrecipMultip') then
				allocate(frozenPrecipMultip(nargs-3))
				read (line2, *, end = 999)xx1,frozenPrecipMultip(1),    &
	 &          xx2, frozenPrecipMultip(2),xx3,frozenPrecipMultip(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'frozenPrecipMultip_') then
				
				allocate(frozenPrecipMultip_sampled(nargs))
				read (line2, *, end = 999)frozenPrecipMultip_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      frozenPrecipMultip_sampled(1),"|",                          &
	 &      frozenPrecipMultip_sampled(2),"|",                          &
	 &      frozenPrecipMultip_sampled(3)
			
				deallocate(frozenPrecipMultip_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          frozenPrecipMultip(1),"|",                              &
	 &          frozenPrecipMultip(2),"|",                              &
	 &          frozenPrecipMultip(3)
			end if
			
			deallocate(frozenPrecipMultip)

			else if (parname.eq.'snowfrz_scale') then
				allocate(snowfrz_scale(nargs-3))
				read (line2, *, end = 999)xx1,snowfrz_scale(1),         &
	 &          xx2, snowfrz_scale(2),xx3,snowfrz_scale(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'snowfrz_scale_') then
				
				allocate(snowfrz_scale_sampled(nargs))
				read (line2, *, end = 999)snowfrz_scale_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",       &
	 &      snowfrz_scale_sampled(1),"|",                             &
	 &      snowfrz_scale_sampled(2),"|",                             &
	 &      snowfrz_scale_sampled(3)
			
				deallocate(snowfrz_scale_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          snowfrz_scale(1),"|",                                   &
	 &          snowfrz_scale(2),"|",                                   &
	 &          snowfrz_scale(3)
			end if
			
			deallocate(snowfrz_scale)

			else if (parname.eq.'fixedThermalCond_snow') then
				allocate(fixedThermalCond_snow(nargs-3))
				read (line2, *, end = 999)xx1,fixedThermalCond_snow(1),        &
	 &          xx2, fixedThermalCond_snow(2),xx3,fixedThermalCond_snow(3)
			

			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'fixedThermalCond_snow_') then
				
				allocate(fixedThermalCond_snow_sampled(nargs))
				read (line2, *, end = 999)fixedThermalCond_snow_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      fixedThermalCond_snow_sampled(1),"|",                       &
	 &      fixedThermalCond_snow_sampled(2),"|",                       &
	 &      fixedThermalCond_snow_sampled(3)
			
				deallocate(fixedThermalCond_snow_sampled)
				
				
					counter2 = counter2 + 1
		end if
	
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          fixedThermalCond_snow(1),"|",                           &
	 &          fixedThermalCond_snow(2),"|",                           &
	 &          fixedThermalCond_snow(3)
			end if
			
			deallocate(fixedThermalCond_snow)

			else if (parname.eq.'albedoMax') then
				allocate(albedoMax(nargs-3))
				read (line2, *, end = 999)xx1,albedoMax(1),             &
	 &          xx2, albedoMax(2),xx3,albedoMax(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'albedoMax_') then
			
				allocate(albedoMax_sampled(nargs))
				read (line2, *, end = 999)albedoMax_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      albedoMax_sampled(1),"|",                                   &
	 &      albedoMax_sampled(2),"|",                                   &
	 &      albedoMax_sampled(3)
			
				deallocate(albedoMax_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          albedoMax(1),"|",                                       &
	 &          albedoMax(2),"|",                                       &
	 &          albedoMax(3)
			end if
			
			deallocate(albedoMax)

			else if (parname.eq.'albedoMinWinter') then
				allocate(albedoMinWinter(nargs-3))
				read (line2, *, end = 999)xx1,albedoMinWinter(1),       &
	 &          xx2, albedoMinWinter(2),xx3,albedoMinWinter(3)
			
			counter2 = 0
			
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'albedoMinWinter_') then
				
				allocate(albedoMinWinter_sampled(nargs))
				read (line2, *, end = 999)albedoMinWinter_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      albedoMinWinter_sampled(1),"|",                             &
	 &      albedoMinWinter_sampled(2),"|",                             &
	 &      albedoMinWinter_sampled(3)
			
				deallocate(albedoMinWinter_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          albedoMinWinter(1),"|",                                 &
	 &          albedoMinWinter(2),"|",                                 &
	 &          albedoMinWinter(3)
			end if
			
			deallocate(albedoMinWinter)

			else if (parname.eq.'albedoMinSpring') then
				allocate(albedoMinSpring(nargs-3))
				read (line2, *, end = 999)xx1,albedoMinSpring(1),       &
	 &          xx2, albedoMinSpring(2),xx3,albedoMinSpring(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'albedoMinSpring_') then
				
				allocate(albedoMinSpring_sampled(nargs))
				read (line2, *, end = 999)albedoMinSpring_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      albedoMinSpring_sampled(1),"|",                             &
	 &      albedoMinSpring_sampled(2),"|",                             &
	 &      albedoMinSpring_sampled(3)
			
				deallocate(albedoMinSpring_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          albedoMinSpring(1),"|",                                 &
	 &          albedoMinSpring(2),"|",                                 &
	 &          albedoMinSpring(3)
			end if
			
			deallocate(albedoMinSpring)

			else if (parname.eq.'albedoMaxVisible') then
				allocate(albedoMaxVisible(nargs-3))
				read (line2, *, end = 999)xx1,albedoMaxVisible(1),      &
	 &          xx2, albedoMaxVisible(2),xx3,albedoMaxVisible(3)
			

			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'albedoMaxVisible_') then
				
				allocate(albedoMaxVisible_sampled(nargs))
				read (line2, *, end = 999)albedoMaxVisible_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      albedoMaxVisible_sampled(1),"|",                            &
	 &      albedoMaxVisible_sampled(2),"|",                            &
	 &      albedoMaxVisible_sampled(3)
			
				deallocate(albedoMaxVisible_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          albedoMaxVisible(1),"|",                                &
	 &          albedoMaxVisible(2),"|",                                &
	 &          albedoMaxVisible(3)
			end if
			
			deallocate(albedoMaxVisible)

			else if (parname.eq.'albedoMinVisible') then
				allocate(albedoMinVisible(nargs-3))
				read (line2, *, end = 999)xx1,albedoMinVisible(1),      &
	 &          xx2, albedoMinVisible(2),xx3,albedoMinVisible(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'albedoMinVisible_') then
				
				allocate(albedoMinVisible_sampled(nargs))
				read (line2, *, end = 999)albedoMinVisible_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      albedoMinVisible_sampled(1),"|",                            &
	 &      albedoMinVisible_sampled(2),"|",                            &
	 &      albedoMinVisible_sampled(3)
			
				deallocate(albedoMinVisible_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          albedoMinVisible(1),"|",                                &
	 &          albedoMinVisible(2),"|",                                &
	 &          albedoMinVisible(3)
			end if
			
			deallocate(albedoMinVisible)

			else if (parname.eq.'albedoMaxNearIR') then
				allocate(albedoMaxNearIR(nargs-3))
				read (line2, *, end = 999)xx1,albedoMaxNearIR(1),       &
	 &          xx2, albedoMaxNearIR(2),xx3,albedoMaxNearIR(3)
			

			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'albedoMaxNearIR_') then
				
				allocate(albedoMaxNearIR_sampled(nargs))
				read (line2, *, end = 999)albedoMaxNearIR_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      albedoMaxNearIR_sampled(1),"|",                             &
	 &      albedoMaxNearIR_sampled(2),"|",                             &
	 &      albedoMaxNearIR_sampled(3)
			
				deallocate(albedoMaxNearIR_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          albedoMaxNearIR(1),"|",                                 &
	 &          albedoMaxNearIR(2),"|",                                 &
	 &          albedoMaxNearIR(3)
			end if
			
			deallocate(albedoMaxNearIR)

			else if (parname.eq.'albedoMinNearIR') then
				allocate(albedoMinNearIR(nargs-3))
				read (line2, *, end = 999)xx1,albedoMinNearIR(1),       &
	 &          xx2, albedoMinNearIR(2),xx3,albedoMinNearIR(3)
			

			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'albedoMinNearIR_') then
				
				allocate(albedoMinNearIR_sampled(nargs))
				read (line2, *, end = 999)albedoMinNearIR_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      albedoMinNearIR_sampled(1),"|",                             &
	 &      albedoMinNearIR_sampled(2),"|",                             &
	 &      albedoMinNearIR_sampled(3)
			
				deallocate(albedoMinNearIR_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          albedoMinNearIR(1),"|",                                 &
	 &          albedoMinNearIR(2),"|",                                 &
	 &          albedoMinNearIR(3)
			end if
			
			deallocate(albedoMinNearIR)

			else if (parname.eq.'albedoDecayRate') then
				allocate(albedoDecayRate(nargs-3))
				read (line2, *, end = 999)xx1,albedoDecayRate(1),       &
	 &          xx2, albedoDecayRate(2),xx3,albedoDecayRate(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'albedoDecayRate_') then
				
				allocate(albedoDecayRate_sampled(nargs))
				read (line2, *, end = 999)albedoDecayRate_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",         &
	 &      albedoDecayRate_sampled(1),"|",                             &
	 &      albedoDecayRate_sampled(2),"|",                             &
	 &      albedoDecayRate_sampled(3)
			
				deallocate(albedoDecayRate_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
				write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",     &
	 &          albedoDecayRate(1),"|",                                 &
	 &          albedoDecayRate(2),"|",                                 &
	 &          albedoDecayRate(3)
			end if
			
			deallocate(albedoDecayRate)

			else if (parname.eq.'radExt_snow') then
				allocate(radExt_snow(nargs-3))
				read (line2, *, end = 999)xx1,radExt_snow(1),           &
	 &          xx2, radExt_snow(2),xx3,radExt_snow(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'radExt_snow_') then
				allocate(radExt_snow_sampled(nargs))
				read (line2, *, end = 999)radExt_snow_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      radExt_snow_sampled(1),"|",                                 &
	 &      radExt_snow_sampled(2),"|",                                 &
	 &      radExt_snow_sampled(3)
			
				deallocate(radExt_snow_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          radExt_snow(1),"|",                                     &
	 &          radExt_snow(2),"|",                                     &
	 &          radExt_snow(3)
			end if
			
			deallocate(radExt_snow)

			else if (parname.eq.'directScale') then
				allocate(directScale(nargs-3))
				read (line2, *, end = 999)xx1,directScale(1),           &
	 &          xx2, directScale(2),xx3,directScale(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'directScale_') then
				
				allocate(directScale_sampled(nargs))
				read (line2, *, end = 999)directScale_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      directScale_sampled(1),"|",                                 &
	 &      directScale_sampled(2),"|",                                 &
	 &      directScale_sampled(3)
			
				deallocate(directScale_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          directScale(1),"|",                                     &
	 &          directScale(2),"|",                                     &
	 &          directScale(3)
			end if
			
			deallocate(directScale)

			else if (parname.eq.'Frad_direct') then
				allocate(Frad_direct(nargs-3))
				read (line2, *, end = 999)xx1,Frad_direct(1),           &
	 &          xx2, Frad_direct(2),xx3,Frad_direct(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'Frad_direct_') then
				
				allocate(Frad_direct_sampled(nargs))
				read (line2, *, end = 999)Frad_direct_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      Frad_direct_sampled(1),"|",                                 &
	 &      Frad_direct_sampled(2),"|",                                 &
	 &      Frad_direct_sampled(3)
			
				deallocate(Frad_direct_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          Frad_direct(1),"|",                                     &
	 &          Frad_direct(2),"|",                                     &
	 &          Frad_direct(3)
			end if
			
			deallocate(Frad_direct)

			else if (parname.eq.'Frad_vis') then
				allocate(Frad_vis(nargs-3))
				read (line2, *, end = 999)xx1,Frad_vis(1),              &
	 &          xx2, Frad_vis(2),xx3,Frad_vis(3)
			
		
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'Frad_vis_') then
				
				allocate(Frad_vis_sampled(nargs))
				read (line2, *, end = 999)Frad_vis_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      Frad_vis_sampled(1),"|",                                    &
	 &      Frad_vis_sampled(2),"|",                                    &
	 &      Frad_vis_sampled(3)
			
				deallocate(Frad_vis_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          Frad_vis(1),"|",                                        &
	 &          Frad_vis(2),"|",                                        &
	 &          Frad_vis(3)
			end if
			
			deallocate(Frad_vis)

			else if (parname.eq.'newSnowDenMin') then
				allocate(newSnowDenMin(nargs-3))
				read (line2, *, end = 999)xx1,newSnowDenMin(1),         &
	 &          xx2, newSnowDenMin(2),xx3,newSnowDenMin(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'newSnowDenMin_') then
				
				allocate(newSnowDenMin_sampled(nargs))
				read (line2, *, end = 999)newSnowDenMin_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      newSnowDenMin_sampled(1),"|",                               &
	 &      newSnowDenMin_sampled(2),"|",                               &
	 &      newSnowDenMin_sampled(3)
			
				deallocate(newSnowDenMin_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          newSnowDenMin(1),"|",                                   &
	 &          newSnowDenMin(2),"|",                                   &
	 &          newSnowDenMin(3)
			end if
			
			deallocate(newSnowDenMin)

			else if (parname.eq.'newSnowDenMult') then
				allocate(newSnowDenMult(nargs-3))
				read (line2, *, end = 999)xx1,newSnowDenMult(1),        &
	 &          xx2, newSnowDenMult(2),xx3,newSnowDenMult(3)
			
		
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'newSnowDenMult_') then
				
				allocate(newSnowDenMult_sampled(nargs))
				read (line2, *, end = 999)newSnowDenMult_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      newSnowDenMult_sampled(1),"|",                              &
	 &      newSnowDenMult_sampled(2),"|",                              &
	 &      newSnowDenMult_sampled(3)
			
				deallocate(newSnowDenMult_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          newSnowDenMult(1),"|",                                  &
	 &          newSnowDenMult(2),"|",                                  &
	 &          newSnowDenMult(3)
			end if
			
			deallocate(newSnowDenMult)

			else if (parname.eq.'newSnowDenScal') then
				allocate(newSnowDenScal(nargs-3))
				read (line2, *, end = 999)xx1,newSnowDenScal(1),        &
	 &          xx2, newSnowDenScal(2),xx3,newSnowDenScal(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'newSnowDenScal_') then
				
				allocate(newSnowDenScal_sampled(nargs))
				read (line2, *, end = 999)newSnowDenScal_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      newSnowDenScal_sampled(1),"|",                              &
	 &      newSnowDenScal_sampled(2),"|",                              &
	 &      newSnowDenScal_sampled(3)
			
				deallocate(newSnowDenScal_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          newSnowDenScal(1),"|",                                  &
	 &          newSnowDenScal(2),"|",                                  &
	 &          newSnowDenScal(3)
			end if
			
			deallocate(newSnowDenScal)

			else if (parname.eq.'newSnowDenAdd') then
				allocate(newSnowDenAdd(nargs-3))
				read (line2, *, end = 999)xx1,newSnowDenAdd(1),        &
	 &          xx2, newSnowDenAdd(2),xx3,newSnowDenAdd(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'newSnowDenAdd_') then
				
				allocate(newSnowDenAdd_sampled(nargs))
				read (line2, *, end = 999)newSnowDenAdd_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      newSnowDenAdd_sampled(1),"|",                               &
	 &      newSnowDenAdd_sampled(2),"|",                               &
	 &      newSnowDenAdd_sampled(3)
			
				deallocate(newSnowDenAdd_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          newSnowDenAdd(1),"|",                                   &
	 &          newSnowDenAdd(2),"|",                                   &
	 &          newSnowDenAdd(3)
			end if
			
			deallocate(newSnowDenAdd)

			else if (parname.eq.'newSnowDenMultTemp') then
				allocate(newSnowDenMultTemp(nargs-3))
				read (line2, *, end = 999)xx1,newSnowDenMultTemp(1),    &
	 &          xx2, newSnowDenMultTemp(2),xx3,newSnowDenMultTemp(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'newSnowDenMultTemp_') then
				
				allocate(newSnowDenMultTemp_sampled(nargs))
				read (line2, *, end = 999)newSnowDenMultTemp_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      newSnowDenMultTemp_sampled(1),"|",                          &
	 &      newSnowDenMultTemp_sampled(2),"|",                          &
	 &      newSnowDenMultTemp_sampled(3)
			
				deallocate(newSnowDenMultTemp_sampled)
			
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          newSnowDenMultTemp(1),"|",                              &
	 &          newSnowDenMultTemp(2),"|",                              &
	 &          newSnowDenMultTemp(3)
			end if
			
			deallocate(newSnowDenMultTemp)

			else if (parname.eq.'constSnowDen') then
				allocate(constSnowDen(nargs-3))
				read (line2, *, end = 999)xx1,constSnowDen(1),          &
	 &          xx2, constSnowDen(2),xx3,constSnowDen(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'constSnowDen_') then
				
				allocate(constSnowDen_sampled(nargs))
				read (line2, *, end = 999)constSnowDen_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      constSnowDen_sampled(1),"|",                                &
	 &      constSnowDen_sampled(2),"|",                                &
	 &      constSnowDen_sampled(3)
			
				deallocate(constSnowDen_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          constSnowDen(1),"|",                                    &
	 &          constSnowDen(2),"|",                                    &
	 &          constSnowDen(3)
			end if
			
			deallocate(constSnowDen)

			else if (parname.eq.'newSnowDenMultWind') then
				allocate(newSnowDenMultWind(nargs-3))
				read (line2, *, end = 999)xx1,newSnowDenMultWind(1),    &
	 &          xx2, newSnowDenMultWind(2),xx3,newSnowDenMultWind(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'newSnowDenMultWind_') then
				
				allocate(newSnowDenMultWind_sampled(nargs))
				read (line2, *, end = 999)newSnowDenMultWind_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      newSnowDenMultWind_sampled(1),"|",                          &
	 &      newSnowDenMultWind_sampled(2),"|",                          &
	 &      newSnowDenMultWind_sampled(3)
			
				deallocate(newSnowDenMultWind_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          newSnowDenMultWind(1),"|",                              &
	 &          newSnowDenMultWind(2),"|",                              &
	 &          newSnowDenMultWind(3)
			end if
			
			deallocate(newSnowDenMultWind)

			else if (parname.eq.'newSnowDenMultAnd') then
				allocate(newSnowDenMultAnd(nargs-3))
				read (line2, *, end = 999)xx1,newSnowDenMultAnd(1),     &
	 &          xx2, newSnowDenMultAnd(2),xx3,newSnowDenMultAnd(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'newSnowDenMultAnd_') then
				
				allocate(newSnowDenMultAnd_sampled(nargs))
				read (line2, *, end = 999)newSnowDenMultAnd_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      newSnowDenMultAnd_sampled(1),"|",                           &
	 &      newSnowDenMultAnd_sampled(2),"|",                           &
	 &      newSnowDenMultAnd_sampled(3)
			
				deallocate(newSnowDenMultAnd_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          newSnowDenMultAnd(1),"|",                               &
	 &          newSnowDenMultAnd(2),"|",                               &
	 &          newSnowDenMultAnd(3)
			end if
			
			deallocate(newSnowDenMultAnd)

			else if (parname.eq.'newSnowDenBase') then
				allocate(newSnowDenBase(nargs-3))
				read (line2, *, end = 999)xx1,newSnowDenBase(1),        &
	 &          xx2, newSnowDenBase(2),xx3,newSnowDenBase(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'newSnowDenBase_') then
				
				allocate(newSnowDenBase_sampled(nargs))
				read (line2, *, end = 999)newSnowDenBase_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      newSnowDenBase_sampled(1),"|",                              &
	 &      newSnowDenBase_sampled(2),"|",                              &
	 &      newSnowDenBase_sampled(3)
			
				deallocate(newSnowDenBase_sampled)
				
				
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          newSnowDenBase(1),"|",                                  &
	 &          newSnowDenBase(2),"|",                                  &
	 &          newSnowDenBase(3)
			end if
			
			deallocate(newSnowDenBase)

			else if (parname.eq.'densScalGrowth') then
				allocate(densScalGrowth(nargs-3))
				read (line2, *, end = 999)xx1,densScalGrowth(1),        &
	 &          xx2, densScalGrowth(2),xx3,densScalGrowth(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'densScalGrowth_') then
				
				allocate(densScalGrowth_sampled(nargs))
				read (line2, *, end = 999)densScalGrowth_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      densScalGrowth_sampled(1),"|",                              &
	 &      densScalGrowth_sampled(2),"|",                              &
	 &      densScalGrowth_sampled(3)
			
				deallocate(densScalGrowth_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          densScalGrowth(1),"|",                                  &
	 &          densScalGrowth(2),"|",                                  &
	 &          densScalGrowth(3)
			end if
			
			deallocate(densScalGrowth)

			else if (parname.eq.'tempScalGrowth') then
				allocate(tempScalGrowth(nargs-3))
				read (line2, *, end = 999)xx1,tempScalGrowth(1),        &
	 &          xx2, tempScalGrowth(2),xx3,tempScalGrowth(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'tempScalGrowth_') then
				
				allocate(tempScalGrowth_sampled(nargs))
				read (line2, *, end = 999)tempScalGrowth_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      tempScalGrowth_sampled(1),"|",                              &
	 &      tempScalGrowth_sampled(2),"|",                              &
	 &      tempScalGrowth_sampled(3)
			
				deallocate(tempScalGrowth_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          tempScalGrowth(1),"|",                                  &
	 &          tempScalGrowth(2),"|",                                  &
	 &          tempScalGrowth(3)
			end if
			
			deallocate(tempScalGrowth)

			else if (parname.eq.'grainGrowthRate') then
				allocate(grainGrowthRate(nargs-3))
				read (line2, *, end = 999)xx1,grainGrowthRate(1),       &
	 &          xx2, grainGrowthRate(2),xx3,grainGrowthRate(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'grainGrowthRate_') then
				
				allocate(grainGrowthRate_sampled(nargs))
				read (line2, *, end = 999)grainGrowthRate_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",         &
	 &      grainGrowthRate_sampled(1),"|",                             &
	 &      grainGrowthRate_sampled(2),"|",                             &
	 &      grainGrowthRate_sampled(3)
			
				deallocate(grainGrowthRate_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
				write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",     &
	 &          grainGrowthRate(1),"|",                                 &
	 &          grainGrowthRate(2),"|",                                 &
	 &          grainGrowthRate(3)
			end if
			
			deallocate(grainGrowthRate)

			else if (parname.eq.'densScalOvrbdn') then
				allocate(densScalOvrbdn(nargs-3))
				read (line2, *, end = 999)xx1,densScalOvrbdn(1),        &
	 &          xx2, densScalOvrbdn(2),xx3,densScalOvrbdn(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'densScalOvrbdn_') then
				
				allocate(densScalOvrbdn_sampled(nargs))
				read (line2, *, end = 999)densScalOvrbdn_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      densScalOvrbdn_sampled(1),"|",                              &
	 &      densScalOvrbdn_sampled(2),"|",                              &
	 &      densScalOvrbdn_sampled(3)
			
				deallocate(densScalOvrbdn_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          densScalOvrbdn(1),"|",                                  &
	 &          densScalOvrbdn(2),"|",                                  &
	 &          densScalOvrbdn(3)
			end if
			
			deallocate(densScalOvrbdn)
			
			
			
			else if (parname.eq.'tempScalOvrbdn') then
				allocate(tempScalOvrbdn(nargs-3))
				read (line2, *, end = 999)xx1,tempScalOvrbdn(1),        &
	 &          xx2, tempScalOvrbdn(2),xx3,tempScalOvrbdn(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'tempScalOvrbdn_') then
				
				allocate(tempScalOvrbdn_sampled(nargs))
				read (line2, *, end = 999)tempScalOvrbdn_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      tempScalOvrbdn_sampled(1),"|",                              &
	 &      tempScalOvrbdn_sampled(2),"|",                              &
	 &      tempScalOvrbdn_sampled(3)
			
				deallocate(tempScalOvrbdn_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          tempScalOvrbdn(1),"|",                                  &
	 &          tempScalOvrbdn(2),"|",                                  &
	 &          tempScalOvrbdn(3)
			end if
			
			deallocate(tempScalOvrbdn)
			
			
			
			else if (parname.eq.'baseViscosity') then
				allocate(baseViscosity(nargs-3))
				read (line2, *, end = 999)xx1,baseViscosity(1),         &
	 &          xx2, baseViscosity(2),xx3,baseViscosity(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'baseViscosity_') then
				
				allocate(baseViscosity_sampled(nargs))
				read (line2, *, end = 999)baseViscosity_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",         &
	 &      baseViscosity_sampled(1),"|",                               &
	 &      baseViscosity_sampled(2),"|",                               &
	 &      baseViscosity_sampled(3)
			
				deallocate(baseViscosity_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
				write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",     &
	 &          baseViscosity(1),"|",                                   &
	 &          baseViscosity(2),"|",                                   &
	 &          baseViscosity(3)
			end if
			
			deallocate(baseViscosity)
			
			

			else if (parname.eq.'Fcapil') then
				allocate(Fcapil(nargs-3))
				read (line2, *, end = 999)xx1,Fcapil(1),                &
	 &          xx2, Fcapil(2),xx3,Fcapil(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'Fcapil_') then
				
				allocate(Fcapil_sampled(nargs))
				read (line2, *, end = 999)Fcapil_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      Fcapil_sampled(1),"|",                                      &
	 &      Fcapil_sampled(2),"|",                                      &
	 &      Fcapil_sampled(3)
			
				deallocate(Fcapil_sampled)

					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          Fcapil(1),"|",                                          &
	 &          Fcapil(2),"|",                                          &
	 &          Fcapil(3)
			end if
			
			deallocate(Fcapil)

			else if (parname.eq.'k_snow') then
				allocate(k_snow(nargs-3))
				read (line2, *, end = 999)xx1,k_snow(1),                &
	 &          xx2, k_snow(2),xx3,k_snow(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'k_snow_') then
				
				allocate(k_snow_sampled(nargs))
				read (line2, *, end = 999)k_snow_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      k_snow_sampled(1),"|",                                      &
	 &      k_snow_sampled(2),"|",                                      &
	 &      k_snow_sampled(3)
			
				deallocate(k_snow_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          k_snow(1),"|",                                          &
	 &          k_snow(2),"|",                                          &
	 &          k_snow(3)
			end if
			
			deallocate(k_snow)
			
			
			
			else if (parname.eq.'mw_exp') then
				allocate(mw_exp(nargs-3))
				read (line2, *, end = 999)xx1,mw_exp(1),                &
	 &          xx2, mw_exp(2),xx3,mw_exp(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'mw_exp_') then
				
				allocate(mw_exp_sampled(nargs))
				read (line2, *, end = 999)mw_exp_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      mw_exp_sampled(1),"|",                                      &
	 &      mw_exp_sampled(2),"|",                                      &
	 &      mw_exp_sampled(3)
			
				deallocate(mw_exp_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          mw_exp(1),"|",                                          &
	 &          mw_exp(2),"|",                                          &
	 &          mw_exp(3)
			end if
			
			deallocate(mw_exp)

			else if (parname.eq.'z0Snow') then
				allocate(z0Snow(nargs-3))
				read (line2, *, end = 999)xx1,z0Snow(1),                &
	 &          xx2, z0Snow(2),xx3,z0Snow(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'z0Snow_') then
			
				allocate(z0Snow_sampled(nargs))
				read (line2, *, end = 999)z0Snow_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      z0Snow_sampled(1),"|",                                      &
	 &      z0Snow_sampled(2),"|",                                      &
	 &      z0Snow_sampled(3)
			
				deallocate(z0Snow_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          z0Snow(1),"|",                                          &
	 &          z0Snow(2),"|",                                          &
	 &          z0Snow(3)
			end if
			
			deallocate(z0Snow)

			else if (parname.eq.'z0Soil') then
				allocate(z0Soil(nargs-3))
				read (line2, *, end = 999)xx1,z0Soil(1),                &
	 &          xx2, z0Soil(2),xx3,z0Soil(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'z0Soil_') then
				
				allocate(z0Soil_sampled(nargs))
				read (line2, *, end = 999)z0Soil_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      z0Soil_sampled(1),"|",                                      &
	 &      z0Soil_sampled(2),"|",                                      &
	 &      z0Soil_sampled(3)
			
				deallocate(z0Soil_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          z0Soil(1),"|",                                          &
	 &          z0Soil(2),"|",                                          &
	 &          z0Soil(3)
			end if
			
			deallocate(z0Soil)

			else if (parname.eq.'z0Canopy') then
				allocate(z0Canopy(nargs-3))
				read (line2, *, end = 999)xx1,z0Canopy(1),              &
	 &          xx2, z0Canopy(2),xx3,z0Canopy(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'z0Canopy_') then
				
				allocate(z0Canopy_sampled(nargs))
				read (line2, *, end = 999)z0Canopy_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      z0Canopy_sampled(1),"|",                                    &
	 &      z0Canopy_sampled(2),"|",                                    &
	 &      z0Canopy_sampled(3)
			
				deallocate(z0Canopy_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          z0Canopy(1),"|",                                        &
	 &          z0Canopy(2),"|",                                        &
	 &          z0Canopy(3)
			end if
			
			deallocate(z0Canopy)

			else if (parname.eq.'zpdFraction') then
				allocate(zpdFraction(nargs-3))
				read (line2, *, end = 999)xx1,zpdFraction(1),           &
	 &          xx2, zpdFraction(2),xx3,zpdFraction(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'zpdFraction_') then
				
				allocate(zpdFraction_sampled(nargs))
				read (line2, *, end = 999)zpdFraction_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      zpdFraction_sampled(1),"|",                                 &
	 &      zpdFraction_sampled(2),"|",                                 &
	 &      zpdFraction_sampled(3)
			
				deallocate(zpdFraction_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          zpdFraction(1),"|",                                     &
	 &          zpdFraction(2),"|",                                     &
	 &          zpdFraction(3)
			end if
			
			deallocate(zpdFraction)

			else if (parname.eq.'critRichNumber') then
				allocate(critRichNumber(nargs-3))
				read (line2, *, end = 999)xx1,critRichNumber(1),        &
	 &          xx2, critRichNumber(2),xx3,critRichNumber(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'critRichNumber_') then
				
				allocate(critRichNumber_sampled(nargs))
				read (line2, *, end = 999)critRichNumber_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      critRichNumber_sampled(1),"|",                              &
	 &      critRichNumber_sampled(2),"|",                              &
	 &      critRichNumber_sampled(3)
			
				deallocate(critRichNumber_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          critRichNumber(1),"|",                                  &
	 &          critRichNumber(2),"|",                                  &
	 &          critRichNumber(3)
			end if
			
			deallocate(critRichNumber)

			else if (parname.eq.'Louis79_bparam') then
				allocate(Louis79_bparam(nargs-3))
				read (line2, *, end = 999)xx1,Louis79_bparam(1),        &
	 &          xx2, Louis79_bparam(2),xx3,Louis79_bparam(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'Louis79_bparam_') then
				
				allocate(Louis79_bparam_sampled(nargs))
				read (line2, *, end = 999)Louis79_bparam_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      Louis79_bparam_sampled(1),"|",                              &
	 &      Louis79_bparam_sampled(2),"|",                              &
	 &      Louis79_bparam_sampled(3)
			
				deallocate(Louis79_bparam_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          Louis79_bparam(1),"|",                                  &
	 &          Louis79_bparam(2),"|",                                  &
	 &          Louis79_bparam(3)
			end if
			
			deallocate(Louis79_bparam)

			else if (parname.eq.'Louis79_cStar') then
				allocate(Louis79_cStar(nargs-3))
				read (line2, *, end = 999)xx1,Louis79_cStar(1),         &
	 &          xx2, Louis79_cStar(2),xx3,Louis79_cStar(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'Louis79_cStar_') then
				
				allocate(Louis79_cStar_sampled(nargs))
				read (line2, *, end = 999)Louis79_cStar_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      Louis79_cStar_sampled(1),"|",                               &
	 &      Louis79_cStar_sampled(2),"|",                               &
	 &      Louis79_cStar_sampled(3)
			
				deallocate(Louis79_cStar_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          Louis79_cStar(1),"|",                                   &
	 &          Louis79_cStar(2),"|",                                   &
	 &          Louis79_cStar(3)
			end if
			
			deallocate(Louis79_cStar)

			else if (parname.eq.'Mahrt87_eScale') then
				allocate(Mahrt87_eScale(nargs-3))
				read (line2, *, end = 999)xx1,Mahrt87_eScale(1),        &
	 &          xx2, Mahrt87_eScale(2),xx3,Mahrt87_eScale(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'Mahrt87_eScale_') then
				
				allocate(Mahrt87_eScale_sampled(nargs))
				read (line2, *, end = 999)Mahrt87_eScale_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      Mahrt87_eScale_sampled(1),"|",                              &
	 &      Mahrt87_eScale_sampled(2),"|",                              &
	 &      Mahrt87_eScale_sampled(3)
			
				deallocate(Mahrt87_eScale_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          Mahrt87_eScale(1),"|",                                  &
	 &          Mahrt87_eScale(2),"|",                                  &
	 &          Mahrt87_eScale(3)
			end if
			
			deallocate(Mahrt87_eScale)

			else if (parname.eq.'leafExchangeCoeff') then
				allocate(leafExchangeCoeff(nargs-3))
				read (line2, *, end = 999)xx1,leafExchangeCoeff(1),     &
	 &          xx2, leafExchangeCoeff(2),xx3,leafExchangeCoeff(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'leafExchangeCoeff_') then
				
				allocate(leafExchangeCoeff_sampled(nargs))
				read (line2, *, end = 999)leafExchangeCoeff_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      leafExchangeCoeff_sampled(1),"|",                           &
	 &      leafExchangeCoeff_sampled(2),"|",                           &
	 &      leafExchangeCoeff_sampled(3)
			
				deallocate(leafExchangeCoeff_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          leafExchangeCoeff(1),"|",                               &
	 &          leafExchangeCoeff(2),"|",                               &
	 &          leafExchangeCoeff(3)
			end if
			
			deallocate(leafExchangeCoeff)

			else if (parname.eq.'windReductionParam') then
				allocate(windReductionParam(nargs-3))
				read (line2, *, end = 999)xx1,windReductionParam(1),    &
	 &          xx2, windReductionParam(2),xx3,windReductionParam(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'windReductionParam_') then
				
				allocate(windReductionParam_sampled(nargs))
				read (line2, *, end = 999)windReductionParam_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      windReductionParam_sampled(1),"|",                          &
	 &      windReductionParam_sampled(2),"|",                          &
	 &      windReductionParam_sampled(3)
			
				deallocate(windReductionParam_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          windReductionParam(1),"|",                              &
	 &          windReductionParam(2),"|",                              &
	 &          windReductionParam(3)
			end if
			
			deallocate(windReductionParam)

			else if (parname.eq.'Kc25') then
				allocate(Kc25(nargs-3))
				read (line2, *, end = 999)xx1,Kc25(1),                  &
	 &          xx2, Kc25(2),xx3,Kc25(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'Kc25_') then
				
				allocate(Kc25_sampled(nargs))
				read (line2, *, end = 999)Kc25_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      Kc25_sampled(1),"|",                                        &
	 &      Kc25_sampled(2),"|",                                        &
	 &      Kc25_sampled(3)
			
				deallocate(Kc25_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          Kc25(1),"|",                                            &
	 &          Kc25(2),"|",                                            &
	 &          Kc25(3)
			end if
			
			deallocate(Kc25)

			else if (parname.eq.'Ko25') then
				allocate(Ko25(nargs-3))
				read (line2, *, end = 999)xx1,Ko25(1),                  &
	 &          xx2, Ko25(2),xx3,Ko25(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'Ko25_') then
				
				allocate(Ko25_sampled(nargs))
				read (line2, *, end = 999)Ko25_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      Ko25_sampled(1),"|",                                        &
	 &      Ko25_sampled(2),"|",                                        &
	 &      Ko25_sampled(3)
			
				deallocate(Ko25_sampled)
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          Ko25(1),"|",                                            &
	 &          Ko25(2),"|",                                            &
	 &          Ko25(3)
			end if
			
			deallocate(Ko25)

			else if (parname.eq.'Kc_qFac') then
				allocate(Kc_qFac(nargs-3))
				read (line2, *, end = 999)xx1,Kc_qFac(1),               &
	 &          xx2, Kc_qFac(2),xx3,Kc_qFac(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'Kc_qFac_') then
				
				allocate(Kc_qFac_sampled(nargs))
				read (line2, *, end = 999)Kc_qFac_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      Kc_qFac_sampled(1),"|",                                     &
	 &      Kc_qFac_sampled(2),"|",                                     &
	 &      Kc_qFac_sampled(3)
			
				deallocate(Kc_qFac_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          Kc_qFac(1),"|",                                         &
	 &          Kc_qFac(2),"|",                                         &
	 &          Kc_qFac(3)
			end if
			
			deallocate(Kc_qFac)

			else if (parname.eq.'Ko_qFac') then
				allocate(Ko_qFac(nargs-3))
				read (line2, *, end = 999)xx1,Ko_qFac(1),               &
	 &          xx2, Ko_qFac(2),xx3,Ko_qFac(3)
			
		
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'Ko_qFac_') then
				
				allocate(Ko_qFac_sampled(nargs))
				read (line2, *, end = 999)Ko_qFac_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      Ko_qFac_sampled(1),"|",                                     &
	 &      Ko_qFac_sampled(2),"|",                                     &
	 &      Ko_qFac_sampled(3)
			
				deallocate(Ko_qFac_sampled)
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          Ko_qFac(1),"|",                                         &
	 &          Ko_qFac(2),"|",                                         &
	 &          Ko_qFac(3)
			end if
			
			deallocate(Ko_qFac)

			else if (parname.eq.'kc_Ha') then
				allocate(kc_Ha(nargs-3))
				read (line2, *, end = 999)xx1,kc_Ha(1),                 &
	 &          xx2, kc_Ha(2),xx3,kc_Ha(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'kc_Ha_') then
				
				allocate(kc_Ha_sampled(nargs))
				read (line2, *, end = 999)kc_Ha_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      kc_Ha_sampled(1),"|",                                       &
	 &      kc_Ha_sampled(2),"|",                                       &
	 &      kc_Ha_sampled(3)
			
				deallocate(kc_Ha_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          kc_Ha(1),"|",                                           &
	 &          kc_Ha(2),"|",                                           &
	 &          kc_Ha(3)
			end if
			
			deallocate(kc_Ha)

			else if (parname.eq.'ko_Ha') then
				allocate(ko_Ha(nargs-3))
				read (line2, *, end = 999)xx1,ko_Ha(1),                 &
	 &          xx2, ko_Ha(2),xx3,ko_Ha(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'ko_Ha_') then
				
				allocate(ko_Ha_sampled(nargs))
				read (line2, *, end = 999)ko_Ha_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      ko_Ha_sampled(1),"|",                                       &
	 &      ko_Ha_sampled(2),"|",                                       &
	 &      ko_Ha_sampled(3)
			
				deallocate(ko_Ha_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          ko_Ha(1),"|",                                           &
	 &          ko_Ha(2),"|",                                           &
	 &          ko_Ha(3)
			end if
			
			deallocate(ko_Ha)

			else if (parname.eq.'vcmax25_canopyTop') then
				allocate(vcmax25_canopyTop(nargs-3))
				read (line2, *, end = 999)xx1,vcmax25_canopyTop(1),     &
	 &          xx2, vcmax25_canopyTop(2),xx3,vcmax25_canopyTop(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'vcmax25_canopyTop_') then
				
				allocate(vcmax25_canopyTop_sampled(nargs))
				read (line2, *, end = 999)vcmax25_canopyTop_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      vcmax25_canopyTop_sampled(1),"|",                           &
	 &      vcmax25_canopyTop_sampled(2),"|",                           &
	 &      vcmax25_canopyTop_sampled(3)
			
				deallocate(vcmax25_canopyTop_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          vcmax25_canopyTop(1),"|",                               &
	 &          vcmax25_canopyTop(2),"|",                               &
	 &          vcmax25_canopyTop(3)
			end if
			
			deallocate(vcmax25_canopyTop)

			else if (parname.eq.'vcmax_qFac') then
				allocate(vcmax_qFac(nargs-3))
				read (line2, *, end = 999)xx1,vcmax_qFac(1),            &
	 &          xx2, vcmax_qFac(2),xx3,vcmax_qFac(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'vcmax_qFac_') then
				
				allocate(vcmax_qFac_sampled(nargs))
				read (line2, *, end = 999)vcmax_qFac_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      vcmax_qFac_sampled(1),"|",                                  &
	 &      vcmax_qFac_sampled(2),"|",                                  &
	 &      vcmax_qFac_sampled(3)
			
				deallocate(vcmax_qFac_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          vcmax_qFac(1),"|",                                      &
	 &          vcmax_qFac(2),"|",                                      &
	 &          vcmax_qFac(3)
			end if
			
			deallocate(vcmax_qFac)

			else if (parname.eq.'vcmax_Ha') then
				allocate(vcmax_Ha(nargs-3))
				read (line2, *, end = 999)xx1,vcmax_Ha(1),              &
	 &          xx2, vcmax_Ha(2),xx3,vcmax_Ha(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'vcmax_Ha_') then
				allocate(vcmax_Ha_sampled(nargs))
				read (line2, *, end = 999)vcmax_Ha_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      vcmax_Ha_sampled(1),"|",                                    &
	 &      vcmax_Ha_sampled(2),"|",                                    &
	 &      vcmax_Ha_sampled(3)
			
				deallocate(vcmax_Ha_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          vcmax_Ha(1),"|",                                        &
	 &          vcmax_Ha(2),"|",                                        &
	 &          vcmax_Ha(3)
			end if
			
			deallocate(vcmax_Ha)

			else if (parname.eq.'vcmax_Hd') then
				allocate(vcmax_Hd(nargs-3))
				read (line2, *, end = 999)xx1,vcmax_Hd(1),              &
	 &          xx2, vcmax_Hd(2),xx3,vcmax_Hd(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'vcmax_Hd_') then
				
				allocate(vcmax_Hd_sampled(nargs))
				read (line2, *, end = 999)vcmax_Hd_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      vcmax_Hd_sampled(1),"|",                                    &
	 &      vcmax_Hd_sampled(2),"|",                                    &
	 &      vcmax_Hd_sampled(3)
			
				deallocate(vcmax_Hd_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          vcmax_Hd(1),"|",                                        &
	 &          vcmax_Hd(2),"|",                                        &
	 &          vcmax_Hd(3)
			end if
			
			deallocate(vcmax_Hd)

			else if (parname.eq.'vcmax_Sv') then
				allocate(vcmax_Sv(nargs-3))
				read (line2, *, end = 999)xx1,vcmax_Sv(1),              &
	 &          xx2, vcmax_Sv(2),xx3,vcmax_Sv(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'vcmax_Sv_') then
				
				allocate(vcmax_Sv_sampled(nargs))
				read (line2, *, end = 999)vcmax_Sv_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      vcmax_Sv_sampled(1),"|",                                    &
	 &      vcmax_Sv_sampled(2),"|",                                    &
	 &      vcmax_Sv_sampled(3)
			
				deallocate(vcmax_Sv_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          vcmax_Sv(1),"|",                                        &
	 &          vcmax_Sv(2),"|",                                        &
	 &          vcmax_Sv(3)
			end if
			
			deallocate(vcmax_Sv)

			else if (parname.eq.'vcmax_Kn') then
				allocate(vcmax_Kn(nargs-3))
				read (line2, *, end = 999)xx1,vcmax_Kn(1),              &
	 &          xx2, vcmax_Kn(2),xx3,vcmax_Kn(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'vcmax_Kn_') then
				
				allocate(vcmax_Kn_sampled(nargs))
				read (line2, *, end = 999)vcmax_Kn_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      vcmax_Kn_sampled(1),"|",                                    &
	 &      vcmax_Kn_sampled(2),"|",                                    &
	 &      vcmax_Kn_sampled(3)
			
				deallocate(vcmax_Kn_sampled)
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          vcmax_Kn(1),"|",                                        &
	 &          vcmax_Kn(2),"|",                                        &
	 &          vcmax_Kn(3)
			end if
			
			deallocate(vcmax_Kn)

			else if (parname.eq.'jmax25_scale') then
				allocate(jmax25_scale(nargs-3))
				read (line2, *, end = 999)xx1,jmax25_scale(1),          &
	 &          xx2, jmax25_scale(2),xx3,jmax25_scale(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'jmax25_scale_') then
				
				allocate(jmax25_scale_sampled(nargs))
				read (line2, *, end = 999)jmax25_scale_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      jmax25_scale_sampled(1),"|",                                &
	 &      jmax25_scale_sampled(2),"|",                                &
	 &      jmax25_scale_sampled(3)
			
				deallocate(jmax25_scale_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          jmax25_scale(1),"|",                                    &
	 &          jmax25_scale(2),"|",                                    &
	 &          jmax25_scale(3)
			end if
			
			deallocate(jmax25_scale)

			else if (parname.eq.'jmax_Ha') then
				allocate(jmax_Ha(nargs-3))
				read (line2, *, end = 999)xx1,jmax_Ha(1),        &
	 &          xx2, jmax_Ha(2),xx3,jmax_Ha(3)
			
			counter2 = 0
			
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'jmax_Ha_') then
				
				allocate(jmax_Ha_sampled(nargs))
				read (line2, *, end = 999)jmax_Ha_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      jmax_Ha_sampled(1),"|",                                     &
	 &      jmax_Ha_sampled(2),"|",                                     &
	 &      jmax_Ha_sampled(3)
			
				deallocate(jmax_Ha_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          jmax_Ha(1),"|",                                         &
	 &          jmax_Ha(2),"|",                                         &
	 &          jmax_Ha(3)
			end if
			
			deallocate(jmax_Ha)

			else if (parname.eq.'jmax_Hd') then
				allocate(jmax_Hd(nargs-3))
				read (line2, *, end = 999)xx1,jmax_Hd(1),               &
	 &          xx2, jmax_Hd(2),xx3,jmax_Hd(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'jmax_Hd_') then
				
				allocate(jmax_Hd_sampled(nargs))
				read (line2, *, end = 999)jmax_Hd_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      jmax_Hd_sampled(1),"|",                                     &
	 &      jmax_Hd_sampled(2),"|",                                     &
	 &      jmax_Hd_sampled(3)
			
				deallocate(jmax_Hd_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          jmax_Hd(1),"|",                                         &
	 &          jmax_Hd(2),"|",                                         &
	 &          jmax_Hd(3)
			end if
			
			deallocate(jmax_Hd)

			else if (parname.eq.'jmax_Sv') then
				allocate(jmax_Sv(nargs-3))
				read (line2, *, end = 999)xx1,jmax_Sv(1),               &
	 &          xx2, jmax_Sv(2),xx3,jmax_Sv(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'jmax_Sv_') then
				
				allocate(jmax_Sv_sampled(nargs))
				read (line2, *, end = 999)jmax_Sv_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      jmax_Sv_sampled(1),"|",                                     &
	 &      jmax_Sv_sampled(2),"|",                                     &
	 &      jmax_Sv_sampled(3)
			
				deallocate(jmax_Sv_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          jmax_Sv(1),"|",                                         &
	 &          jmax_Sv(2),"|",                                         &
	 &          jmax_Sv(3)
			end if
			
			deallocate(jmax_Sv)

			else if (parname.eq.'fractionJ') then
				allocate(fractionJ(nargs-3))
				read (line2, *, end = 999)xx1,fractionJ(1),             &
	 &          xx2, fractionJ(2),xx3,fractionJ(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'fractionJ_') then
				
				allocate(fractionJ_sampled(nargs))
				read (line2, *, end = 999)fractionJ_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      fractionJ_sampled(1),"|",                                   &
	 &      fractionJ_sampled(2),"|",                                   &
	 &      fractionJ_sampled(3)
			
				deallocate(fractionJ_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          fractionJ(1),"|",                                       &
	 &          fractionJ(2),"|",                                       &
	 &          fractionJ(3)
			end if
			
			deallocate(fractionJ)

			else if (parname.eq.'quantamYield') then
				allocate(quantamYield(nargs-3))
				read (line2, *, end = 999)xx1,quantamYield(1),          &
	 &          xx2, quantamYield(2),xx3,quantamYield(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'quantamYield_') then
				
				allocate(quantamYield_sampled(nargs))
				read (line2, *, end = 999)quantamYield_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      quantamYield_sampled(1),"|",                                &
	 &      quantamYield_sampled(2),"|",                                &
	 &      quantamYield_sampled(3)
			
				deallocate(quantamYield_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          quantamYield(1),"|",                                    &
	 &          quantamYield(2),"|",                                    &
	 &          quantamYield(3)
			end if
			
			deallocate(quantamYield)

			else if (parname.eq.'vpScaleFactor') then
				allocate(vpScaleFactor(nargs-3))
				read (line2, *, end = 999)xx1,vpScaleFactor(1),         &
	 &          xx2, vpScaleFactor(2),xx3,vpScaleFactor(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'vpScaleFactor_') then
				
				allocate(vpScaleFactor_sampled(nargs))
				read (line2, *, end = 999)vpScaleFactor_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      vpScaleFactor_sampled(1),"|",                               &
	 &      vpScaleFactor_sampled(2),"|",                               &
	 &      vpScaleFactor_sampled(3)
			
				deallocate(vpScaleFactor_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          vpScaleFactor(1),"|",                                   &
	 &          vpScaleFactor(2),"|",                                   &
	 &          vpScaleFactor(3)
			end if
			
			deallocate(vpScaleFactor)

			else if (parname.eq.'cond2photo_slope') then
				allocate(cond2photo_slope(nargs-3))
				read (line2, *, end = 999)xx1,cond2photo_slope(1),      &
	 &          xx2, cond2photo_slope(2),xx3,cond2photo_slope(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'cond2photo_slope_') then
				
				allocate(cond2photo_slope_sampled(nargs))
				read (line2, *, end = 999)cond2photo_slope_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      cond2photo_slope_sampled(1),"|",                            &
	 &      cond2photo_slope_sampled(2),"|",                            &
	 &      cond2photo_slope_sampled(3)
			
				deallocate(cond2photo_slope_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          cond2photo_slope(1),"|",                                &
	 &          cond2photo_slope(2),"|",                                &
	 &          cond2photo_slope(3)
			end if
			
			deallocate(cond2photo_slope)

			else if (parname.eq.'minStomatalConductance') then
				allocate(minStomatalConductance(nargs-3))
				read (line2, *, end = 999)xx1,minStomatalConductance(1),&
	 &       xx2, minStomatalConductance(2),xx3,minStomatalConductance(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'minStomatalConductance_') then
				
				allocate(minStomatalConductance_sampled(nargs))
				read (line2, *, end = 999)minStomatalConductance_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      minStomatalConductance_sampled(1),"|",                      &
	 &      minStomatalConductance_sampled(2),"|",                      &
	 &      minStomatalConductance_sampled(3)
			
				deallocate(minStomatalConductance_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          minStomatalConductance(1),"|",                          &
	 &          minStomatalConductance(2),"|",                          &
	 &          minStomatalConductance(3)
			end if
			
			deallocate(minStomatalConductance)

			else if (parname.eq.'winterSAI') then
				allocate(winterSAI(nargs-3))
				read (line2, *, end = 999)xx1,winterSAI(1),             &
	 &          xx2, winterSAI(2),xx3,winterSAI(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'winterSAI_') then
				
				allocate(winterSAI_sampled(nargs))
				read (line2, *, end = 999)winterSAI_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      winterSAI_sampled(1),"|",                                   &
	 &      winterSAI_sampled(2),"|",                                   &
	 &      winterSAI_sampled(3)
			
				deallocate(winterSAI_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          winterSAI(1),"|",                                       &
	 &          winterSAI(2),"|",                                       &
	 &          winterSAI(3)
			end if
			
			deallocate(winterSAI)

			else if (parname.eq.'summerLAI') then
				allocate(summerLAI(nargs-3))
				read (line2, *, end = 999)xx1,summerLAI(1),             &
	 &          xx2, summerLAI(2),xx3,summerLAI(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'summerLAI_') then
				
				allocate(summerLAI_sampled(nargs))
				read (line2, *, end = 999)summerLAI_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      summerLAI_sampled(1),"|",                                   &
	 &      summerLAI_sampled(2),"|",                                   &
	 &      summerLAI_sampled(3)
			
				deallocate(summerLAI_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          summerLAI(1),"|",                                       &
	 &          summerLAI(2),"|",                                       &
	 &          summerLAI(3)
			end if
			
			deallocate(summerLAI)

			else if (parname.eq.'rootScaleFactor1') then
				allocate(rootScaleFactor1(nargs-3))
				read (line2, *, end = 999)xx1,rootScaleFactor1(1),      &
	 &          xx2, rootScaleFactor1(2),xx3,rootScaleFactor1(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'rootScaleFactor1_') then
				
				allocate(rootScaleFactor1_sampled(nargs))
				read (line2, *, end = 999)rootScaleFactor1_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      rootScaleFactor1_sampled(1),"|",                            &
	 &      rootScaleFactor1_sampled(2),"|",                            &
	 &      rootScaleFactor1_sampled(3)
			
				deallocate(rootScaleFactor1_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          rootScaleFactor1(1),"|",                                &
	 &          rootScaleFactor1(2),"|",                                &
	 &          rootScaleFactor1(3)
			end if
			
			deallocate(rootScaleFactor1)

			else if (parname.eq.'rootScaleFactor2') then
				allocate(rootScaleFactor2(nargs-3))
				read (line2, *, end = 999)xx1,rootScaleFactor2(1),      &
	 &          xx2, rootScaleFactor2(2),xx3,rootScaleFactor2(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'rootScaleFactor2_') then
				
				allocate(rootScaleFactor2_sampled(nargs))
				read (line2, *, end = 999)rootScaleFactor2_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      rootScaleFactor2_sampled(1),"|",                            &
	 &      rootScaleFactor2_sampled(2),"|",                            &
	 &      rootScaleFactor2_sampled(3)
			
				deallocate(rootScaleFactor2_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          rootScaleFactor2(1),"|",                                &
	 &          rootScaleFactor2(2),"|",                                &
	 &          rootScaleFactor2(3)
			end if
			
			deallocate(rootScaleFactor2)

			else if (parname.eq.'rootingDepth') then
				allocate(rootingDepth(nargs-3))
				read (line2, *, end = 999)xx1,rootingDepth(1),          &
	 &          xx2, rootingDepth(2),xx3,rootingDepth(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'rootingDepth_') then
				
				allocate(rootingDepth_sampled(nargs))
				read (line2, *, end = 999)rootingDepth_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      rootingDepth_sampled(1),"|",                                &
	 &      rootingDepth_sampled(2),"|",                                &
	 &      rootingDepth_sampled(3)
			
				deallocate(rootingDepth_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          rootingDepth(1),"|",                                    &
	 &          rootingDepth(2),"|",                                    &
	 &          rootingDepth(3)
			end if
			
			deallocate(rootingDepth)
			
			else if (parname.eq.'rootDistExp') then
				allocate(rootDistExp(nargs-3))
				read (line2, *, end = 999)xx1,rootDistExp(1),           &
	 &          xx2, rootDistExp(2),xx3,rootDistExp(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'rootDistExp_') then
				
				allocate(rootDistExp_sampled(nargs))
				read (line2, *, end = 999)rootDistExp_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      rootDistExp_sampled(1),"|",                                 &
	 &      rootDistExp_sampled(2),"|",                                 &
	 &      rootDistExp_sampled(3)
			
				deallocate(rootDistExp_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          rootDistExp(1),"|",                                     &
	 &          rootDistExp(2),"|",                                     &
	 &          rootDistExp(3)
			end if
			
			deallocate(rootDistExp)
			
			
			
			
			
			

			else if (parname.eq.'plantWiltPsi') then
				allocate(plantWiltPsi(nargs-3))
				read (line2, *, end = 999)xx1,plantWiltPsi(1),          &
	 &          xx2, plantWiltPsi(2),xx3,plantWiltPsi(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'plantWiltPsi_') then
				
				allocate(plantWiltPsi_sampled(nargs))
				read (line2, *, end = 999)plantWiltPsi_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      plantWiltPsi_sampled(1),"|",                                &
	 &      plantWiltPsi_sampled(2),"|",                                &
	 &      plantWiltPsi_sampled(3)
			
				deallocate(plantWiltPsi_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          plantWiltPsi(1),"|",                                    &
	 &          plantWiltPsi(2),"|",                                    &
	 &          plantWiltPsi(3)
			end if
			
			deallocate(plantWiltPsi)

			else if (parname.eq.'soilStressParam') then
				allocate(soilStressParam(nargs-3))
				read (line2, *, end = 999)xx1,soilStressParam(1),       &
	 &          xx2, soilStressParam(2),xx3,soilStressParam(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'soilStressParam_') then
			
				allocate(soilStressParam_sampled(nargs))
				read (line2, *, end = 999)soilStressParam_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      soilStressParam_sampled(1),"|",                             &
	 &      soilStressParam_sampled(2),"|",                             &
	 &      soilStressParam_sampled(3)
			
				deallocate(soilStressParam_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          soilStressParam(1),"|",                                 &
	 &          soilStressParam(2),"|",                                 &
	 &          soilStressParam(3)
			end if
			
			deallocate(soilStressParam)

			else if (parname.eq.'critSoilWilting') then
				allocate(critSoilWilting(nargs-3))
				read (line2, *, end = 999)xx1,critSoilWilting(1),       &
	 &          xx2, critSoilWilting(2),xx3,critSoilWilting(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'critSoilWilting_') then
				
				allocate(critSoilWilting_sampled(nargs))
				read (line2, *, end = 999)critSoilWilting_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      critSoilWilting_sampled(1),"|",                             &
	 &      critSoilWilting_sampled(2),"|",                             &
	 &      critSoilWilting_sampled(3)
			
				deallocate(critSoilWilting_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          critSoilWilting(1),"|",                                 &
	 &          critSoilWilting(2),"|",                                 &
	 &          critSoilWilting(3)
			end if
			
			deallocate(critSoilWilting)
			
			
			
			else if (parname.eq.'critSoilTranspire') then
				allocate(critSoilTranspire(nargs-3))
				read (line2, *, end = 999)xx1,critSoilTranspire(1),     &
	 &          xx2, critSoilTranspire(2),xx3,critSoilTranspire(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'critSoilTranspire_') then
				
				allocate(critSoilTranspire_sampled(nargs))
				read (line2, *, end = 999)critSoilTranspire_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      critSoilTranspire_sampled(1),"|",                           &
	 &      critSoilTranspire_sampled(2),"|",                           &
	 &      critSoilTranspire_sampled(3)
			
				deallocate(critSoilTranspire_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          critSoilTranspire(1),"|",                               &
	 &          critSoilTranspire(2),"|",                               &
	 &          critSoilTranspire(3)
			end if
			
			deallocate(critSoilTranspire)
			
			
			
			
			else if (parname.eq.'critAquiferTranspire') then
				allocate(critAquiferTranspire(nargs-3))
				read (line2, *, end = 999)xx1,critAquiferTranspire(1),  &
	 &          xx2, critAquiferTranspire(2),xx3,critAquiferTranspire(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'critAquiferTranspire_') then
				
				allocate(critAquiferTranspire_sampled(nargs))
				read (line2, *, end = 999)critAquiferTranspire_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      critAquiferTranspire_sampled(1),"|",                        &
	 &      critAquiferTranspire_sampled(2),"|",                        &
	 &      critAquiferTranspire_sampled(3)
			
				deallocate(critAquiferTranspire_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          critAquiferTranspire(1),"|",                            &
	 &          critAquiferTranspire(2),"|",                            &
	 &          critAquiferTranspire(3)
			end if
			
			deallocate(critAquiferTranspire)
			
			
			else if (parname.eq.'heightCanopyTop') then
				allocate(heightCanopyTop(nargs-3))
				read (line2, *, end = 999)xx1,heightCanopyTop(1),       &
	 &          xx2, heightCanopyTop(2),xx3,heightCanopyTop(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'heightCanopyTop_') then
				
				allocate(heightCanopyTop_sampled(nargs))
				read (line2, *, end = 999)heightCanopyTop_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      heightCanopyTop_sampled(1),"|",                             &
	 &      heightCanopyTop_sampled(2),"|",                             &
	 &      heightCanopyTop_sampled(3)
			
				deallocate(heightCanopyTop_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          heightCanopyTop(1),"|",                                 &
	 &          heightCanopyTop(2),"|",                                 &
	 &          heightCanopyTop(3)
			end if
			
			deallocate(heightCanopyTop)
			
			!!Additional parameters included for Lake Winnipeg
			
			
			else if (parname.eq.'specificHeatVeg') then
				allocate(specificHeatVeg(nargs-3))
				read (line2, *, end = 999)xx1,specificHeatVeg(1),       &
	 &          xx2, specificHeatVeg(2),xx3,specificHeatVeg(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'specificHeatVeg_') then
				
				allocate(specificHeatVeg_sampled(nargs))
				read (line2, *, end = 999)specificHeatVeg_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      specificHeatVeg_sampled(1),"|",                             &
	 &      specificHeatVeg_sampled(2),"|",                             &
	 &      specificHeatVeg_sampled(3)
			
				deallocate(specificHeatVeg_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          specificHeatVeg(1),"|",                                 &
	 &          specificHeatVeg(2),"|",                                 &
	 &          specificHeatVeg(3)
			end if
			
			deallocate(specificHeatVeg)
			
			else if (parname.eq.'maxMassVegetation') then
				allocate(maxMassVegetation(nargs-3))
				read (line2, *, end = 999)xx1,maxMassVegetation(1),     &
	 &          xx2, maxMassVegetation(2),xx3,maxMassVegetation(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'maxMassVegetation_') then
				
				allocate(maxMassVegetation_sampled(nargs))
				read (line2, *, end = 999)maxMassVegetation_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      maxMassVegetation_sampled(1),"|",                           &
	 &      maxMassVegetation_sampled(2),"|",                           &
	 &      maxMassVegetation_sampled(3)
			
				deallocate(maxMassVegetation_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          maxMassVegetation(1),"|",                               &
	 &          maxMassVegetation(2),"|",                               &
	 &          maxMassVegetation(3)
			end if
			
			deallocate(maxMassVegetation)
			
			
			else if (parname.eq.'throughfallScaleSnow') then
				allocate(throughfallScaleSnow(nargs-3))
				read (line2, *, end = 999)xx1,throughfallScaleSnow(1),  &
	 &          xx2, throughfallScaleSnow(2),xx3,throughfallScaleSnow(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'throughfallScaleSnow_') then
				
				allocate(throughfallScaleSnow_sampled(nargs))
				read (line2, *, end = 999)throughfallScaleSnow_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      throughfallScaleSnow_sampled(1),"|",                        &
	 &      throughfallScaleSnow_sampled(2),"|",                        &
	 &      throughfallScaleSnow_sampled(3)
			
				deallocate(throughfallScaleSnow_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          throughfallScaleSnow(1),"|",                            &
	 &          throughfallScaleSnow(2),"|",                            &
	 &          throughfallScaleSnow(3)
			end if
			
			deallocate(throughfallScaleSnow)
			
			
			
			else if (parname.eq.'throughfallScaleRain') then
				allocate(throughfallScaleRain(nargs-3))
				read (line2, *, end = 999)xx1,throughfallScaleRain(1),  &
	 &          xx2, throughfallScaleRain(2),xx3,throughfallScaleRain(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'throughfallScaleRain_') then
				
				allocate(throughfallScaleRain_sampled(nargs))
				read (line2, *, end = 999)throughfallScaleRain_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      throughfallScaleRain_sampled(1),"|",                        &
	 &      throughfallScaleRain_sampled(2),"|",                        &
	 &      throughfallScaleRain_sampled(3)
			
				deallocate(throughfallScaleRain_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          throughfallScaleRain(1),"|",                            &
	 &          throughfallScaleRain(2),"|",                            &
	 &          throughfallScaleRain(3)
			end if
			
			deallocate(throughfallScaleRain)
			
			else if (parname.eq.'refInterceptCapSnow') then
				allocate(refInterceptCapSnow(nargs-3))
				read (line2, *, end = 999)xx1,refInterceptCapSnow(1),   &
	 &          xx2, refInterceptCapSnow(2),xx3,refInterceptCapSnow(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'refInterceptCapSnow_') then
				
				allocate(refInterceptCapSnow_sampled(nargs))
				read (line2, *, end = 999)refInterceptCapSnow_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      refInterceptCapSnow_sampled(1),"|",                         &
	 &      refInterceptCapSnow_sampled(2),"|",                         &
	 &      refInterceptCapSnow_sampled(3)
			
				deallocate(refInterceptCapSnow_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          refInterceptCapSnow(1),"|",                             &
	 &          refInterceptCapSnow(2),"|",                             &
	 &          refInterceptCapSnow(3)
			end if
			
			deallocate(refInterceptCapSnow)
			
			else if (parname.eq.'refInterceptCapRain') then
				allocate(refInterceptCapRain(nargs-3))
				read (line2, *, end = 999)xx1,refInterceptCapRain(1),   &
	 &          xx2, refInterceptCapRain(2),xx3,refInterceptCapRain(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'refInterceptCapRain_') then
				
				allocate(refInterceptCapRain_sampled(nargs))
				read (line2, *, end = 999)refInterceptCapRain_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      refInterceptCapRain_sampled(1),"|",                         &
	 &      refInterceptCapRain_sampled(2),"|",                         &
	 &      refInterceptCapRain_sampled(3)
			
				deallocate(refInterceptCapRain_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          refInterceptCapRain(1),"|",                             &
	 &          refInterceptCapRain(2),"|",                             &
	 &          refInterceptCapRain(3)
			end if
			
			deallocate(refInterceptCapRain)
			
			else if (parname.eq.'snowUnloadingCoeff') then
				allocate(snowUnloadingCoeff(nargs-3))
				read (line2, *, end = 999)xx1,snowUnloadingCoeff(1),    &
	 &          xx2, snowUnloadingCoeff(2),xx3,snowUnloadingCoeff(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'snowUnloadingCoeff_') then
				
				allocate(snowUnloadingCoeff_sampled(nargs))
				read (line2, *, end = 999)snowUnloadingCoeff_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",         &
	 &      snowUnloadingCoeff_sampled(1),"|",                          &
	 &      snowUnloadingCoeff_sampled(2),"|",                          &
	 &      snowUnloadingCoeff_sampled(3)
			
				deallocate(snowUnloadingCoeff_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          snowUnloadingCoeff(1),"|",                              &
	 &          snowUnloadingCoeff(2),"|",                              &
	 &          snowUnloadingCoeff(3)
			end if
			
			deallocate(snowUnloadingCoeff)
			
			
			else if (parname.eq.'canopyDrainageCoeff') then
				allocate(canopyDrainageCoeff(nargs-3))
				read (line2, *, end = 999)xx1,canopyDrainageCoeff(1),   &
	 &          xx2, canopyDrainageCoeff(2),xx3,canopyDrainageCoeff(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'canopyDrainageCoeff_') then
				
				allocate(canopyDrainageCoeff_sampled(nargs))
				read (line2, *, end = 999)canopyDrainageCoeff_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      canopyDrainageCoeff_sampled(1),"|",                         &
	 &      canopyDrainageCoeff_sampled(2),"|",                         &
	 &      canopyDrainageCoeff_sampled(3)
			
				deallocate(canopyDrainageCoeff_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          canopyDrainageCoeff(1),"|",                             &
	 &          canopyDrainageCoeff(2),"|",                             &
	 &          canopyDrainageCoeff(3)
			end if
			
			deallocate(canopyDrainageCoeff)
			
			
			else if (parname.eq.'absConvTol_energy') then
				allocate(absConvTol_energy(nargs-3))
				read (line2, *, end = 999)xx1,absConvTol_energy(1),     &
	 &          xx2, absConvTol_energy(2),xx3,absConvTol_energy(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'absConvTol_energy_') then
				
				allocate(absConvTol_energy_sampled(nargs))
				read (line2, *, end = 999)absConvTol_energy_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      absConvTol_energy_sampled(1),"|",                           &
	 &      absConvTol_energy_sampled(2),"|",                           &
	 &      absConvTol_energy_sampled(3)
			
				deallocate(absConvTol_energy_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          absConvTol_energy(1),"|",                               &
	 &          absConvTol_energy(2),"|",                               &
	 &          absConvTol_energy(3)
			end if
			
			deallocate(absConvTol_energy)
			
			
			
			
			else if (parname.eq.'canopyWettingExp') then
				allocate(canopyWettingExp(nargs-3))
				read (line2, *, end = 999)xx1,canopyWettingExp(1),      &
	 &          xx2, canopyWettingExp(2),xx3,canopyWettingExp(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'canopyWettingExp_') then
				
				allocate(canopyWettingExp_sampled(nargs))
				read (line2, *, end = 999)canopyWettingExp_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      canopyWettingExp_sampled(1),"|",                            &
	 &      canopyWettingExp_sampled(2),"|",                            &
	 &      canopyWettingExp_sampled(3)
			
				deallocate(canopyWettingExp_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          canopyWettingExp(1),"|",                                &
	 &          canopyWettingExp(2),"|",                                &
	 &          canopyWettingExp(3)
			end if
			
			deallocate(canopyWettingExp)
			
			
			else if (parname.eq.'canopyWettingFactor') then
				allocate(canopyWettingFactor(nargs-3))
				read (line2, *, end = 999)xx1,canopyWettingFactor(1),   &
	 &          xx2, canopyWettingFactor(2),xx3,canopyWettingFactor(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'canopyWettingFactor_') then
				
				allocate(canopyWettingFactor_sampled(nargs))
				read (line2, *, end = 999)canopyWettingFactor_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      canopyWettingFactor_sampled(1),"|",                         &
	 &      canopyWettingFactor_sampled(2),"|",                         &
	 &      canopyWettingFactor_sampled(3)
			
				deallocate(canopyWettingFactor_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          canopyWettingFactor(1),"|",                             &
	 &          canopyWettingFactor(2),"|",                             &
	 &          canopyWettingFactor(3)
			end if
			
			deallocate(canopyWettingFactor)
			
			
			
			else if (parname.eq.'ratioDrip2Unloading') then
				allocate(ratioDrip2Unloading(nargs-3))
				read (line2, *, end = 999)xx1,ratioDrip2Unloading(1),   &
	 &          xx2, ratioDrip2Unloading(2),xx3,ratioDrip2Unloading(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'ratioDrip2Unloading_') then
				
				allocate(ratioDrip2Unloading_sampled(nargs))
				read (line2, *, end = 999)ratioDrip2Unloading_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      ratioDrip2Unloading_sampled(1),"|",                         &
	 &      ratioDrip2Unloading_sampled(2),"|",                         &
	 &      ratioDrip2Unloading_sampled(3)
			
				deallocate(ratioDrip2Unloading_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          ratioDrip2Unloading(1),"|",                             &
	 &          ratioDrip2Unloading(2),"|",                             &
	 &          ratioDrip2Unloading(3)
			end if
			
			deallocate(ratioDrip2Unloading)
			
			
			
			else if (parname.eq.'soil_dens_intr') then
				allocate(soil_dens_intr(nargs-3))
				read (line2, *, end = 999)xx1,soil_dens_intr(1),        &
	 &          xx2, soil_dens_intr(2),xx3,soil_dens_intr(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'soil_dens_intr_') then
				
				allocate(soil_dens_intr_sampled(nargs))
				read (line2, *, end = 999)soil_dens_intr_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      soil_dens_intr_sampled(1),"|",                              &
	 &      soil_dens_intr_sampled(2),"|",                              &
	 &      soil_dens_intr_sampled(3)
			
				deallocate(soil_dens_intr_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          soil_dens_intr(1),"|",                                  &
	 &          soil_dens_intr(2),"|",                                  &
	 &          soil_dens_intr(3)
			end if
			
			deallocate(soil_dens_intr)
			
			else if (parname.eq.'thCond_soil') then
				allocate(thCond_soil(nargs-3))
				read (line2, *, end = 999)xx1,thCond_soil(1),           &
	 &          xx2, thCond_soil(2),xx3,thCond_soil(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'thCond_soil_') then
				
				allocate(thCond_soil_sampled(nargs))
				read (line2, *, end = 999)thCond_soil_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      thCond_soil_sampled(1),"|",                                 &
	 &      thCond_soil_sampled(2),"|",                                 &
	 &      thCond_soil_sampled(3)
			
				deallocate(thCond_soil_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          thCond_soil(1),"|",                                     &
	 &          thCond_soil(2),"|",                                     &
	 &          thCond_soil(3)
			end if
			
			deallocate(thCond_soil)
			
			else if (parname.eq.'frac_sand') then
				allocate(frac_sand(nargs-3))
				read (line2, *, end = 999)xx1,frac_sand(1),             &
	 &          xx2, frac_sand(2),xx3,frac_sand(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'frac_sand_') then
				
				allocate(frac_sand_sampled(nargs))
				read (line2, *, end = 999)frac_sand_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      frac_sand_sampled(1),"|",                                   &
	 &      frac_sand_sampled(2),"|",                                   &
	 &      frac_sand_sampled(3)
			
				deallocate(frac_sand_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          frac_sand(1),"|",                                       &
	 &          frac_sand(2),"|",                                       &
	 &          frac_sand(3)
			end if
			
			deallocate(frac_sand)
			
			else if (parname.eq.'frac_silt') then
				allocate(frac_silt(nargs-3))
				read (line2, *, end = 999)xx1,frac_silt(1),             &
	 &          xx2, frac_silt(2),xx3,frac_silt(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'frac_silt_') then
				
				allocate(frac_silt_sampled(nargs))
				read (line2, *, end = 999)frac_silt_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      frac_silt_sampled(1),"|",                                   &
	 &      frac_silt_sampled(2),"|",                                   &
	 &      frac_silt_sampled(3)
			
				deallocate(frac_silt_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          frac_silt(1),"|",                                       &
	 &          frac_silt(2),"|",                                       &
	 &          frac_silt(3)
			end if
			
			deallocate(frac_silt)
			
			else if (parname.eq.'frac_clay') then
				allocate(frac_clay(nargs-3))
				read (line2, *, end = 999)xx1,frac_clay(1),             &
	 &          xx2, frac_clay(2),xx3,frac_clay(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'frac_clay_') then
				
				allocate(frac_clay_sampled(nargs))
				read (line2, *, end = 999)frac_clay_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      frac_clay_sampled(1),"|",                                   &
	 &      frac_clay_sampled(2),"|",                                   &
	 &      frac_clay_sampled(3)
			
				deallocate(frac_clay_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          frac_clay(1),"|",                                       &
	 &          frac_clay(2),"|",                                       &
	 &          frac_clay(3)
			end if
			
			deallocate(frac_clay)
			
			else if (parname.eq.'fieldCapacity') then
				allocate(fieldCapacity(nargs-3))
				read (line2, *, end = 999)xx1,fieldCapacity(1),         &
	 &          xx2, fieldCapacity(2),xx3,fieldCapacity(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'fieldCapacity_') then
				
				allocate(fieldCapacity_sampled(nargs))
				read (line2, *, end = 999)fieldCapacity_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      fieldCapacity_sampled(1),"|",                               &
	 &      fieldCapacity_sampled(2),"|",                               &
	 &      fieldCapacity_sampled(3)
			
				deallocate(fieldCapacity_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          fieldCapacity(1),"|",                                   &
	 &          fieldCapacity(2),"|",                                   &
	 &          fieldCapacity(3)
			end if
			
			deallocate(fieldCapacity)
			
			else if (parname.eq.'wettingFrontSuction') then
				allocate(wettingFrontSuction(nargs-3))
				read (line2, *, end = 999)xx1,wettingFrontSuction(1),   &
	 &          xx2, wettingFrontSuction(2),xx3,wettingFrontSuction(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'wettingFrontSuction_') then
				
				allocate(wettingFrontSuction_sampled(nargs))
				read (line2, *, end = 999)wettingFrontSuction_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      wettingFrontSuction_sampled(1),"|",                         &
	 &      wettingFrontSuction_sampled(2),"|",                         &
	 &      wettingFrontSuction_sampled(3)
			
				deallocate(wettingFrontSuction_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          wettingFrontSuction(1),"|",                             &
	 &          wettingFrontSuction(2),"|",                             &
	 &          wettingFrontSuction(3)
			end if
			
			deallocate(wettingFrontSuction)
			
			else if (parname.eq.'theta_mp') then
				allocate(theta_mp(nargs-3))
				read (line2, *, end = 999)xx1,theta_mp(1),              &
	 &          xx2, theta_mp(2),xx3,theta_mp(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'theta_mp_') then
				
				allocate(theta_mp_sampled(nargs))
				read (line2, *, end = 999)theta_mp_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      theta_mp_sampled(1),"|",                                    &
	 &      theta_mp_sampled(2),"|",                                    &
	 &      theta_mp_sampled(3)
			
				deallocate(theta_mp_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          theta_mp(1),"|",                                        &
	 &          theta_mp(2),"|",                                        &
	 &          theta_mp(3)
			end if
			
			deallocate(theta_mp)
			
			else if (parname.eq.'theta_sat') then
				allocate(theta_sat(nargs-3))
				read (line2, *, end = 999)xx1,theta_sat(1),             &
	 &          xx2, theta_sat(2),xx3,theta_sat(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'theta_sat_') then
				
				allocate(theta_sat_sampled(nargs))
				read (line2, *, end = 999)theta_sat_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      theta_sat_sampled(1),"|",                                   &
	 &      theta_sat_sampled(2),"|",                                   &
	 &      theta_sat_sampled(3)
			
				deallocate(theta_sat_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          theta_sat(1),"|",                                       &
	 &          theta_sat(2),"|",                                       &
	 &          theta_sat(3)
			end if
			
			deallocate(theta_sat)
			
			else if (parname.eq.'theta_res') then
				allocate(theta_res(nargs-3))
				read (line2, *, end = 999)xx1,theta_res(1),             &
	 &          xx2, theta_res(2),xx3,theta_res(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'theta_res_') then
				
				allocate(theta_res_sampled(nargs))
				read (line2, *, end = 999)theta_res_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      theta_res_sampled(1),"|",                                   &
	 &      theta_res_sampled(2),"|",                                   &
	 &      theta_res_sampled(3)
			
				deallocate(theta_res_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          theta_res(1),"|",                                       &
	 &          theta_res(2),"|",                                       &
	 &          theta_res(3)
			end if
			
			deallocate(theta_res)
			
			
			else if (parname.eq.'vGn_alpha') then
				allocate(vGn_alpha(nargs-3))
				read (line2, *, end = 999)xx1,vGn_alpha(1),             &
	 &          xx2, vGn_alpha(2),xx3,vGn_alpha(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'vGn_alpha_') then
				
				allocate(vGn_alpha_sampled(nargs))
				read (line2, *, end = 999)vGn_alpha_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      vGn_alpha_sampled(1),"|",                                   &
	 &      vGn_alpha_sampled(2),"|",                                   &
	 &      vGn_alpha_sampled(3)
			
				deallocate(vGn_alpha_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          vGn_alpha(1),"|",                                       &
	 &          vGn_alpha(2),"|",                                       &
	 &          vGn_alpha(3)
			end if
			
			deallocate(vGn_alpha)
			
			else if (parname.eq.'vGn_n') then
				allocate(vGn_n(nargs-3))
				read (line2, *, end = 999)xx1,vGn_n(1),                 &
	 &          xx2, vGn_n(2),xx3,vGn_n(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'vGn_n_') then
				
				allocate(vGn_n_sampled(nargs))
				read (line2, *, end = 999)vGn_n_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      vGn_n_sampled(1),"|",                                       &
	 &      vGn_n_sampled(2),"|",                                       &
	 &      vGn_n_sampled(3)
			
				deallocate(vGn_n_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          vGn_n(1),"|",                                           &
	 &          vGn_n(2),"|",                                           &
	 &          vGn_n(3)
			end if
			
			deallocate(vGn_n)
			
			else if (parname.eq.'mpExp') then
				allocate(mpExp(nargs-3))
				read (line2, *, end = 999)xx1,mpExp(1),                 &
	 &          xx2, mpExp(2),xx3,mpExp(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'mpExp_') then
				
				allocate(mpExp_sampled(nargs))
				read (line2, *, end = 999)mpExp_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      mpExp_sampled(1),"|",                                       &
	 &      mpExp_sampled(2),"|",                                       &
	 &      mpExp_sampled(3)
			
				deallocate(mpExp_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          mpExp(1),"|",                                           &
	 &          mpExp(2),"|",                                           &
	 &          mpExp(3)
			end if
			
			deallocate(mpExp)
			
			else if (parname.eq.'k_soil') then
				allocate(k_soil(nargs-3))
				read (line2, *, end = 999)xx1,k_soil(1),                &
	 &          xx2, k_soil(2),xx3,k_soil(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'k_soil_') then
				
				allocate(k_soil_sampled(nargs))
				read (line2, *, end = 999)k_soil_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",         &
	 &      k_soil_sampled(1),"|",                                      &
	 &      k_soil_sampled(2),"|",                                      &
	 &      k_soil_sampled(3)
			
				deallocate(k_soil_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
				write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",     &
	 &          k_soil(1),"|",                                          &
	 &          k_soil(2),"|",                                          &
	 &          k_soil(3)
			end if
			
			deallocate(k_soil)
			
			else if (parname.eq.'k_macropore') then
				allocate(k_macropore(nargs-3))
				read (line2, *, end = 999)xx1,k_macropore(1),           &
	 &          xx2, k_macropore(2),xx3,k_macropore(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'k_macropore_') then
				
				allocate(k_macropore_sampled(nargs))
				read (line2, *, end = 999)k_macropore_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",         &
	 &      k_macropore_sampled(1),"|",                                 &
	 &      k_macropore_sampled(2),"|",                                 &
	 &      k_macropore_sampled(3)
			
				deallocate(k_macropore_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
				write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",     &
	 &          k_macropore(1),"|",                                     &
	 &          k_macropore(2),"|",                                     &
	 &          k_macropore(3)
			end if
			
			deallocate(k_macropore)
			
			else if (parname.eq.'kAnisotropic') then
				allocate(kAnisotropic(nargs-3))
				read (line2, *, end = 999)xx1,kAnisotropic(1),          &
	 &          xx2, kAnisotropic(2),xx3,kAnisotropic(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'kAnisotropic_') then
				
				allocate(kAnisotropic_sampled(nargs))
				read (line2, *, end = 999)kAnisotropic_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      kAnisotropic_sampled(1),"|",                                &
	 &      kAnisotropic_sampled(2),"|",                                &
	 &      kAnisotropic_sampled(3)
			
				deallocate(kAnisotropic_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          kAnisotropic(1),"|",                                    &
	 &          kAnisotropic(2),"|",                                    &
	 &          kAnisotropic(3)
			end if
			
			deallocate(kAnisotropic)
			
			else if (parname.eq.'zScale_TOPMODEL') then
				allocate(zScale_TOPMODEL(nargs-3))
				read (line2, *, end = 999)xx1,zScale_TOPMODEL(1),       &
	 &          xx2, zScale_TOPMODEL(2),xx3,zScale_TOPMODEL(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'zScale_TOPMODEL_') then
				
				allocate(zScale_TOPMODEL_sampled(nargs))
				read (line2, *, end = 999)zScale_TOPMODEL_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      zScale_TOPMODEL_sampled(1),"|",                             &
	 &      zScale_TOPMODEL_sampled(2),"|",                             &
	 &      zScale_TOPMODEL_sampled(3)
			
				deallocate(zScale_TOPMODEL_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          zScale_TOPMODEL(1),"|",                                 &
	 &          zScale_TOPMODEL(2),"|",                                 &
	 &          zScale_TOPMODEL(3)
			end if
			
			deallocate(zScale_TOPMODEL)
			
			else if (parname.eq.'compactedDepth') then
				allocate(compactedDepth(nargs-3))
				read (line2, *, end = 999)xx1,compactedDepth(1),        &
	 &          xx2, compactedDepth(2),xx3,compactedDepth(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'compactedDepth_') then
				
				allocate(compactedDepth_sampled(nargs))
				read (line2, *, end = 999)compactedDepth_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      compactedDepth_sampled(1),"|",                              &
	 &      compactedDepth_sampled(2),"|",                              &
	 &      compactedDepth_sampled(3)
			
				deallocate(compactedDepth_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          compactedDepth(1),"|",                                  &
	 &          compactedDepth(2),"|",                                  &
	 &          compactedDepth(3)
			end if
			
			deallocate(compactedDepth)
			
			else if (parname.eq.'aquiferBaseflowRate') then
				allocate(aquiferBaseflowRate(nargs-3))
				read (line2, *, end = 999)xx1,aquiferBaseflowRate(1),   &
	 &          xx2, aquiferBaseflowRate(2),xx3,aquiferBaseflowRate(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'aquiferBaseflowRate_') then
				
				allocate(aquiferBaseflowRate_sampled(nargs))
				read (line2, *, end = 999)aquiferBaseflowRate_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      aquiferBaseflowRate_sampled(1),"|",                         &
	 &      aquiferBaseflowRate_sampled(2),"|",                         &
	 &      aquiferBaseflowRate_sampled(3)
			
				deallocate(aquiferBaseflowRate_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          aquiferBaseflowRate(1),"|",                             &
	 &          aquiferBaseflowRate(2),"|",                             &
	 &          aquiferBaseflowRate(3)
			end if
			
			deallocate(aquiferBaseflowRate)
			
			else if (parname.eq.'aquiferScaleFactor') then
				allocate(aquiferScaleFactor(nargs-3))
				read (line2, *, end = 999)xx1,aquiferScaleFactor(1),    &
	 &          xx2, aquiferScaleFactor(2),xx3,aquiferScaleFactor(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'aquiferScaleFactor_') then
				
				allocate(aquiferScaleFactor_sampled(nargs))
				read (line2, *, end = 999)aquiferScaleFactor_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      aquiferScaleFactor_sampled(1),"|",                          &
	 &      aquiferScaleFactor_sampled(2),"|",                          &
	 &      aquiferScaleFactor_sampled(3)
			
				deallocate(aquiferScaleFactor_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          aquiferScaleFactor(1),"|",                              &
	 &          aquiferScaleFactor(2),"|",                              &
	 &          aquiferScaleFactor(3)
			end if
			
			deallocate(aquiferScaleFactor)
			
			else if (parname.eq.'aquiferBaseflowExp') then
				allocate(aquiferBaseflowExp(nargs-3))
				read (line2, *, end = 999)xx1,aquiferBaseflowExp(1),    &
	 &          xx2, aquiferBaseflowExp(2),xx3,aquiferBaseflowExp(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'aquiferBaseflowExp_') then
				
				allocate(aquiferBaseflowExp_sampled(nargs))
				read (line2, *, end = 999)aquiferBaseflowExp_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      aquiferBaseflowExp_sampled(1),"|",                          &
	 &      aquiferBaseflowExp_sampled(2),"|",                          &
	 &      aquiferBaseflowExp_sampled(3)
			
				deallocate(aquiferBaseflowExp_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          aquiferBaseflowExp(1),"|",                              &
	 &          aquiferBaseflowExp(2),"|",                              &
	 &          aquiferBaseflowExp(3)
			end if
			
			deallocate(aquiferBaseflowExp)
			
			else if (parname.eq.'qSurfScale') then
				allocate(qSurfScale(nargs-3))
				read (line2, *, end = 999)xx1,qSurfScale(1),            &
	 &          xx2, qSurfScale(2),xx3,qSurfScale(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'qSurfScale_') then
                                     
				
				allocate(qSurfScale_sampled(nargs))
				read (line2, *, end = 999)qSurfScale_sampled
                
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      qSurfScale_sampled(1),"|",                                  &
	 &      qSurfScale_sampled(2),"|",                                  &
	 &      qSurfScale_sampled(3)
			
				deallocate(qSurfScale_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          qSurfScale(1),"|",                                      &
	 &          qSurfScale(2),"|",                                      &
	 &          qSurfScale(3)
			end if
			
			deallocate(qSurfScale)
			
			else if (parname.eq.'specificYield') then
				allocate(specificYield(nargs-3))
				read (line2, *, end = 999)xx1,specificYield(1),         &
	 &          xx2, specificYield(2),xx3,specificYield(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'specificYield_') then
				
				allocate(specificYield_sampled(nargs))
				read (line2, *, end = 999)specificYield_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      specificYield_sampled(1),"|",                               &
	 &      specificYield_sampled(2),"|",                               &
	 &      specificYield_sampled(3)
			
				deallocate(specificYield_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          specificYield(1),"|",                                   &
	 &          specificYield(2),"|",                                   &
	 &          specificYield(3)
			end if
			
			deallocate(specificYield)
			
			else if (parname.eq.'specificStorage') then
				allocate(specificStorage(nargs-3))
				read (line2, *, end = 999)xx1,specificStorage(1),       &
	 &          xx2, specificStorage(2),xx3,specificStorage(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'specificStorage_') then
				
				allocate(specificStorage_sampled(nargs))
				read (line2, *, end = 999)specificStorage_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",         &
	 &      specificStorage_sampled(1),"|",                             &
	 &      specificStorage_sampled(2),"|",                             &
	 &      specificStorage_sampled(3)
			
				deallocate(specificStorage_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
				write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",     &
	 &          specificStorage(1),"|",                                 &
	 &          specificStorage(2),"|",                                 &
	 &          specificStorage(3)
			end if
			
			deallocate(specificStorage)
			
			else if (parname.eq.'f_impede') then
				allocate(f_impede(nargs-3))
				read (line2, *, end = 999)xx1,f_impede(1),              &
	 &          xx2, f_impede(2),xx3,f_impede(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'f_impede_') then
				
				allocate(f_impede_sampled(nargs))
				read (line2, *, end = 999)f_impede_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      f_impede_sampled(1),"|",                                    &
	 &      f_impede_sampled(2),"|",                                    &
	 &      f_impede_sampled(3)
			
				deallocate(f_impede_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          f_impede(1),"|",                                        &
	 &          f_impede(2),"|",                                        &
	 &          f_impede(3)
			end if
			
			deallocate(f_impede)
			
			else if (parname.eq.'soilIceScale') then
				allocate(soilIceScale(nargs-3))
				read (line2, *, end = 999)xx1,soilIceScale(1),          &
	 &          xx2, soilIceScale(2),xx3,soilIceScale(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'soilIceScale_') then
				
				allocate(soilIceScale_sampled(nargs))
				read (line2, *, end = 999)soilIceScale_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      soilIceScale_sampled(1),"|",                                &
	 &      soilIceScale_sampled(2),"|",                                &
	 &      soilIceScale_sampled(3)
			
				deallocate(soilIceScale_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          soilIceScale(1),"|",                                    &
	 &          soilIceScale(2),"|",                                    &
	 &          soilIceScale(3)
			end if
			
			deallocate(soilIceScale)
			
			else if (parname.eq.'soilIceCV') then
				allocate(soilIceCV(nargs-3))
				read (line2, *, end = 999)xx1,soilIceCV(1),             &
	 &          xx2, soilIceCV(2),xx3,soilIceCV(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'soilIceCV_') then
				
				allocate(soilIceCV_sampled(nargs))
				read (line2, *, end = 999)soilIceCV_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      soilIceCV_sampled(1),"|",                                   &
	 &      soilIceCV_sampled(2),"|",                                   &
	 &      soilIceCV_sampled(3)
			
				deallocate(soilIceCV_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          soilIceCV(1),"|",                                       &
	 &          soilIceCV(2),"|",                                       &
	 &          soilIceCV(3)
			end if
			
			deallocate(soilIceCV)
			
			else if (parname.eq.'minwind') then
				allocate(minwind(nargs-3))
				read (line2, *, end = 999)xx1,minwind(1),               &
	 &          xx2, minwind(2),xx3,minwind(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'minwind_') then
				
				allocate(minwind_sampled(nargs))
				read (line2, *, end = 999)minwind_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      minwind_sampled(1),"|",                                     &
	 &      minwind_sampled(2),"|",                                     &
	 &      minwind_sampled(3)
			
				deallocate(minwind_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          minwind(1),"|",                                         &
	 &          minwind(2),"|",                                         &
	 &          minwind(3)
			end if
			
			deallocate(minwind)
			
			else if (parname.eq.'minstep') then
				allocate(minstep(nargs-3))
				read (line2, *, end = 999)xx1,minstep(1),               &
	 &          xx2, minstep(2),xx3,minstep(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'minstep_') then
				
				allocate(minstep_sampled(nargs))
				read (line2, *, end = 999)minstep_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      minstep_sampled(1),"|",                                     &
	 &      minstep_sampled(2),"|",                                     &
	 &      minstep_sampled(3)
			
				deallocate(minstep_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          minstep(1),"|",                                         &
	 &          minstep(2),"|",                                         &
	 &          minstep(3)
			end if
			
			deallocate(minstep)
			
			else if (parname.eq.'maxstep') then
				allocate(maxstep(nargs-3))
				read (line2, *, end = 999)xx1,maxstep(1),               &
	 &          xx2, maxstep(2),xx3,maxstep(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'maxstep_') then
				
				allocate(maxstep_sampled(nargs))
				read (line2, *, end = 999)maxstep_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      maxstep_sampled(1),"|",                                     &
	 &      maxstep_sampled(2),"|",                                     &
	 &      maxstep_sampled(3)
			
				deallocate(maxstep_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          maxstep(1),"|",                                         &
	 &          maxstep(2),"|",                                         &
	 &          maxstep(3)
			end if
			
			deallocate(maxstep)
			
			else if (parname.eq.'wimplicit') then
				allocate(wimplicit(nargs-3))
				read (line2, *, end = 999)xx1,wimplicit(1),             &
	 &          xx2, wimplicit(2),xx3,wimplicit(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'wimplicit_') then
				
				allocate(wimplicit_sampled(nargs))
				read (line2, *, end = 999)wimplicit_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      wimplicit_sampled(1),"|",                                   &
	 &      wimplicit_sampled(2),"|",                                   &
	 &      wimplicit_sampled(3)
			
				deallocate(wimplicit_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          wimplicit(1),"|",                                       &
	 &          wimplicit(2),"|",                                       &
	 &          wimplicit(3)
			end if
			
			deallocate(wimplicit)
			
			else if (parname.eq.'maxiter') then
				allocate(maxiter(nargs-3))
				read (line2, *, end = 999)xx1,maxiter(1),               &
	 &          xx2, maxiter(2),xx3,maxiter(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'maxiter_') then
				
				allocate(maxiter_sampled(nargs))
				read (line2, *, end = 999)maxiter_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      maxiter_sampled(1),"|",                                     &
	 &      maxiter_sampled(2),"|",                                     &
	 &      maxiter_sampled(3)
			
				deallocate(maxiter_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          maxiter(1),"|",                                         &
	 &          maxiter(2),"|",                                         &
	 &          maxiter(3)
			end if
			
			deallocate(maxiter)
			
			else if (parname.eq.'relConvTol_liquid') then
				allocate(relConvTol_liquid(nargs-3))
				read (line2, *, end = 999)xx1,relConvTol_liquid(1),     &
	 &          xx2, relConvTol_liquid(2),xx3,relConvTol_liquid(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'relConvTol_liquid_') then
				
				allocate(relConvTol_liquid_sampled(nargs))
				read (line2, *, end = 999)relConvTol_liquid_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",         &
	 &      relConvTol_liquid_sampled(1),"|",                           &
	 &      relConvTol_liquid_sampled(2),"|",                           &
	 &      relConvTol_liquid_sampled(3)
			
				deallocate(relConvTol_liquid_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
				write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",     &
	 &          relConvTol_liquid(1),"|",                               &
	 &          relConvTol_liquid(2),"|",                               &
	 &          relConvTol_liquid(3)
			end if
			
			deallocate(relConvTol_liquid)
			
			else if (parname.eq.'absConvTol_liquid') then
				allocate(absConvTol_liquid(nargs-3))
				read (line2, *, end = 999)xx1,absConvTol_liquid(1),     &
	 &          xx2, absConvTol_liquid(2),xx3,absConvTol_liquid(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'absConvTol_liquid_') then
				
				allocate(absConvTol_liquid_sampled(nargs))
				read (line2, *, end = 999)absConvTol_liquid_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",         &
	 &      absConvTol_liquid_sampled(1),"|",                           &
	 &      absConvTol_liquid_sampled(2),"|",                           &
	 &      absConvTol_liquid_sampled(3)
			
				deallocate(absConvTol_liquid_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
				write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",     &
	 &          absConvTol_liquid(1),"|",                               &
	 &          absConvTol_liquid(2),"|",                               &
	 &          absConvTol_liquid(3)
			end if
			
			deallocate(absConvTol_liquid)
			
			else if (parname.eq.'relConvTol_matric') then
				allocate(relConvTol_matric(nargs-3))
				read (line2, *, end = 999)xx1,relConvTol_matric(1),     &
	 &          xx2, relConvTol_matric(2),xx3,relConvTol_matric(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'relConvTol_matric_') then
				
				allocate(relConvTol_matric_sampled(nargs))
				read (line2, *, end = 999)relConvTol_matric_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",         &
	 &      relConvTol_matric_sampled(1),"|",                           &
	 &      relConvTol_matric_sampled(2),"|",                           &
	 &      relConvTol_matric_sampled(3)
			
				deallocate(relConvTol_matric_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
				write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",     &
	 &          relConvTol_matric(1),"|",                               &
	 &          relConvTol_matric(2),"|",                               &
	 &          relConvTol_matric(3)
			end if
			
			deallocate(relConvTol_matric)
			
			else if (parname.eq.'absConvTol_matric') then
				allocate(absConvTol_matric(nargs-3))
				read (line2, *, end = 999)xx1,absConvTol_matric(1),     &
	 &          xx2, absConvTol_matric(2),xx3,absConvTol_matric(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'absConvTol_matric_') then
				
				allocate(absConvTol_matric_sampled(nargs))
				read (line2, *, end = 999)absConvTol_matric_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",         &
	 &      absConvTol_matric_sampled(1),"|",                           &
	 &      absConvTol_matric_sampled(2),"|",                           &
	 &      absConvTol_matric_sampled(3)
			
				deallocate(absConvTol_matric_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
				write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",     &
	 &          absConvTol_matric(1),"|",                               &
	 &          absConvTol_matric(2),"|",                               &
	 &          absConvTol_matric(3)
			end if
			
			deallocate(absConvTol_matric)
			
			
			else if (parname.eq.'relConvTol_energy') then
				allocate(relConvTol_energy(nargs-3))
				read (line2, *, end = 999)xx1,relConvTol_energy(1),     &
	 &          xx2, relConvTol_energy(2),xx3,relConvTol_energy(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'relConvTol_energy_') then
				
				allocate(relConvTol_energy_sampled(nargs))
				read (line2, *, end = 999)relConvTol_energy_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",         &
	 &      relConvTol_energy_sampled(1),"|",                           &
	 &      relConvTol_energy_sampled(2),"|",                           &
	 &      relConvTol_energy_sampled(3)
			
				deallocate(relConvTol_energy_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
				write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",     &
	 &          relConvTol_energy(1),"|",                               &
	 &          relConvTol_energy(2),"|",                               &
	 &          relConvTol_energy(3)
			end if
			
			deallocate(relConvTol_energy)
			

			
			
			
			else if (parname.eq.'relConvTol_aquifr') then
				allocate(relConvTol_aquifr(nargs-3))
				read (line2, *, end = 999)xx1,relConvTol_aquifr(1),     &
	 &          xx2, relConvTol_aquifr(2),xx3,relConvTol_aquifr(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'relConvTol_aquifr_') then
				
				allocate(relConvTol_aquifr_sampled(nargs))
				read (line2, *, end = 999)relConvTol_aquifr_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",        &        
	 &      relConvTol_aquifr_sampled(1),"|",                           &
	 &      relConvTol_aquifr_sampled(2),"|",                           &
	 &      relConvTol_aquifr_sampled(3)
			
				deallocate(relConvTol_aquifr_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
				write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",    &
	 &          relConvTol_aquifr(1),"|",                               &
	 &          relConvTol_aquifr(2),"|",                               &
	 &          relConvTol_aquifr(3)
			end if
			
			deallocate(relConvTol_aquifr)
			
			
			
			else if (parname.eq.'absConvTol_aquifr') then
				allocate(absConvTol_aquifr(nargs-3))
				read (line2, *, end = 999)xx1,absConvTol_aquifr(1),     &
	 &          xx2, absConvTol_aquifr(2),xx3,absConvTol_aquifr(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'absConvTol_aquifr_') then
				
				allocate(absConvTol_aquifr_sampled(nargs))
				read (line2, *, end = 999)absConvTol_aquifr_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",        &         
	 &      absConvTol_aquifr_sampled(1),"|",                           &
	 &      absConvTol_aquifr_sampled(2),"|",                           &
	 &      absConvTol_aquifr_sampled(3)
			
				deallocate(absConvTol_aquifr_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
				write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",     &
	 &          absConvTol_aquifr(1),"|",                               &
	 &          absConvTol_aquifr(2),"|",                               &
	 &          absConvTol_aquifr(3)
			end if
			
			deallocate(absConvTol_aquifr)
			
			else if (parname.eq.'zmin') then
				allocate(zmin(nargs-3))
				read (line2, *, end = 999)xx1,zmin(1),                  &
	 &          xx2, zmin(2),xx3,zmin(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'zmin_') then
				
				allocate(zmin_sampled(nargs))
				read (line2, *, end = 999)zmin_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      zmin_sampled(1),"|",                                        &
	 &      zmin_sampled(2),"|",                                        &
	 &      zmin_sampled(3)
			
				deallocate(zmin_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          zmin(1),"|",                                            &
	 &          zmin(2),"|",                                            &
	 &          zmin(3)
			end if
			
			deallocate(zmin)
			
			else if (parname.eq.'zmax') then
				allocate(zmax(nargs-3))
				read (line2, *, end = 999)xx1,zmax(1),                  &
	 &          xx2, zmax(2),xx3,zmax(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'zmax_') then
				
				allocate(zmax_sampled(nargs))
				read (line2, *, end = 999)zmax_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      zmax_sampled(1),"|",                                        &
	 &      zmax_sampled(2),"|",                                        &
	 &      zmax_sampled(3)
			
				deallocate(zmax_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          zmax(1),"|",                                            &
	 &          zmax(2),"|",                                            &
	 &          zmax(3)
			end if
			
			deallocate(zmax)
			
			else if (parname.eq.'zminLayer1') then
				allocate(zminLayer1(nargs-3))
				read (line2, *, end = 999)xx1,zminLayer1(1),            &
	 &          xx2, zminLayer1(2),xx3,zminLayer1(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'zminLayer1_') then
				
				allocate(zminLayer1_sampled(nargs))
				read (line2, *, end = 999)zminLayer1_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      zminLayer1_sampled(1),"|",                                  &
	 &      zminLayer1_sampled(2),"|",                                  &
	 &      zminLayer1_sampled(3)
			
				deallocate(zminLayer1_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          zminLayer1(1),"|",                                      &
	 &          zminLayer1(2),"|",                                      &
	 &          zminLayer1(3)
			end if
			
			deallocate(zminLayer1)
			
			else if (parname.eq.'zminLayer2') then
				allocate(zminLayer2(nargs-3))
				read (line2, *, end = 999)xx1,zminLayer2(1),            &
	 &          xx2, zminLayer2(2),xx3,zminLayer2(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'zminLayer2_') then
				
				allocate(zminLayer2_sampled(nargs))
				read (line2, *, end = 999)zminLayer2_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      zminLayer2_sampled(1),"|",                                  &
	 &      zminLayer2_sampled(2),"|",                                  &
	 &      zminLayer2_sampled(3)
			
				deallocate(zminLayer2_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          zminLayer2(1),"|",                                      &
	 &          zminLayer2(2),"|",                                      &
	 &          zminLayer2(3)
			end if
			
			deallocate(zminLayer2)
			
			else if (parname.eq.'zminLayer3') then
				allocate(zminLayer3(nargs-3))
				read (line2, *, end = 999)xx1,zminLayer3(1),            &
	 &          xx2, zminLayer3(2),xx3,zminLayer3(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'zminLayer3_') then
				
				allocate(zminLayer3_sampled(nargs))
				read (line2, *, end = 999)zminLayer3_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      zminLayer3_sampled(1),"|",                                  &
	 &      zminLayer3_sampled(2),"|",                                  &
	 &      zminLayer3_sampled(3)
			
				deallocate(zminLayer3_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          zminLayer3(1),"|",                                      &
	 &          zminLayer3(2),"|",                                      &
	 &          zminLayer3(3)
			end if
			
			deallocate(zminLayer3)
			
			else if (parname.eq.'zminLayer4') then
				allocate(zminLayer4(nargs-3))
				read (line2, *, end = 999)xx1,zminLayer4(1),            &
	 &          xx2, zminLayer4(2),xx3,zminLayer4(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'zminLayer4_') then
				
				allocate(zminLayer4_sampled(nargs))
				read (line2, *, end = 999)zminLayer4_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      zminLayer4_sampled(1),"|",                                  &
	 &      zminLayer4_sampled(2),"|",                                  &
	 &      zminLayer4_sampled(3)
			
				deallocate(zminLayer4_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          zminLayer4(1),"|",                                      &
	 &          zminLayer4(2),"|",                                      &
	 &          zminLayer4(3)
			end if
			
			deallocate(zminLayer4)
			
			else if (parname.eq.'zminLayer5') then
				allocate(zminLayer5(nargs-3))
				read (line2, *, end = 999)xx1,zminLayer5(1),            &
	 &          xx2, zminLayer5(2),xx3,zminLayer5(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'zminLayer5_') then
				
				allocate(zminLayer5_sampled(nargs))
				read (line2, *, end = 999)zminLayer5_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      zminLayer5_sampled(1),"|",                                  &
	 &      zminLayer5_sampled(2),"|",                                  &
	 &      zminLayer5_sampled(3)
			
				deallocate(zminLayer5_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          zminLayer5(1),"|",                                      &
	 &          zminLayer5(2),"|",                                      &
	 &          zminLayer5(3)
			end if
			
			deallocate(zminLayer5)
			
			else if (parname.eq.'zmaxLayer1_lower') then
				allocate(zmaxLayer1_lower(nargs-3))
				read (line2, *, end = 999)xx1,zmaxLayer1_lower(1),      &
	 &          xx2, zmaxLayer1_lower(2),xx3,zmaxLayer1_lower(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'zmaxLayer1_lower_') then
				
				allocate(zmaxLayer1_lower_sampled(nargs))
				read (line2, *, end = 999)zmaxLayer1_lower_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      zmaxLayer1_lower_sampled(1),"|",                            &
	 &      zmaxLayer1_lower_sampled(2),"|",                            &
	 &      zmaxLayer1_lower_sampled(3)
			
				deallocate(zmaxLayer1_lower_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          zmaxLayer1_lower(1),"|",                                &
	 &          zmaxLayer1_lower(2),"|",                                &
	 &          zmaxLayer1_lower(3)
			end if
			
			deallocate(zmaxLayer1_lower)
			
			else if (parname.eq.'zmaxLayer2_lower') then
				allocate(zmaxLayer2_lower(nargs-3))
				read (line2, *, end = 999)xx1,zmaxLayer2_lower(1),      &
	 &          xx2, zmaxLayer2_lower(2),xx3,zmaxLayer2_lower(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'zmaxLayer2_lower_') then
				
				allocate(zmaxLayer2_lower_sampled(nargs))
				read (line2, *, end = 999)zmaxLayer2_lower_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      zmaxLayer2_lower_sampled(1),"|",                            &
	 &      zmaxLayer2_lower_sampled(2),"|",                            &
	 &      zmaxLayer2_lower_sampled(3)
			
				deallocate(zmaxLayer2_lower_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          zmaxLayer2_lower(1),"|",                                &
	 &          zmaxLayer2_lower(2),"|",                                &
	 &          zmaxLayer2_lower(3)
			end if
			
			deallocate(zmaxLayer2_lower)
			
			else if (parname.eq.'zmaxLayer3_lower') then
				allocate(zmaxLayer3_lower(nargs-3))
				read (line2, *, end = 999)xx1,zmaxLayer3_lower(1),      &
	 &          xx2, zmaxLayer3_lower(2),xx3,zmaxLayer3_lower(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'zmaxLayer3_lower_') then
				
				allocate(zmaxLayer3_lower_sampled(nargs))
				read (line2, *, end = 999)zmaxLayer3_lower_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      zmaxLayer3_lower_sampled(1),"|",                            &
	 &      zmaxLayer3_lower_sampled(2),"|",                            &
	 &      zmaxLayer3_lower_sampled(3)
			
				deallocate(zmaxLayer3_lower_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          zmaxLayer3_lower(1),"|",                                &
	 &          zmaxLayer3_lower(2),"|",                                &
	 &          zmaxLayer3_lower(3)
			end if
			
			deallocate(zmaxLayer3_lower)
			
			else if (parname.eq.'zmaxLayer4_lower') then
				allocate(zmaxLayer4_lower(nargs-3))
				read (line2, *, end = 999)xx1,zmaxLayer4_lower(1),      &
	 &          xx2, zmaxLayer4_lower(2),xx3,zmaxLayer4_lower(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'zmaxLayer4_lower_') then
				
				allocate(zmaxLayer4_lower_sampled(nargs))
				read (line2, *, end = 999)zmaxLayer4_lower_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      zmaxLayer4_lower_sampled(1),"|",                            &
	 &      zmaxLayer4_lower_sampled(2),"|",                            &
	 &      zmaxLayer4_lower_sampled(3)
			
				deallocate(zmaxLayer4_lower_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          zmaxLayer4_lower(1),"|",                                &
	 &          zmaxLayer4_lower(2),"|",                                &
	 &          zmaxLayer4_lower(3)
			end if
			
			deallocate(zmaxLayer4_lower)
			
			else if (parname.eq.'zmaxLayer1_upper') then
				allocate(zmaxLayer1_upper(nargs-3))
				read (line2, *, end = 999)xx1,zmaxLayer1_upper(1),      &
	 &          xx2, zmaxLayer1_upper(2),xx3,zmaxLayer1_upper(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'zmaxLayer1_upper_') then
				
				allocate(zmaxLayer1_upper_sampled(nargs))
				read (line2, *, end = 999)zmaxLayer1_upper_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      zmaxLayer1_upper_sampled(1),"|",                            &
	 &      zmaxLayer1_upper_sampled(2),"|",                            &
	 &      zmaxLayer1_upper_sampled(3)
			
				deallocate(zmaxLayer1_upper_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          zmaxLayer1_upper(1),"|",                                &
	 &          zmaxLayer1_upper(2),"|",                                &
	 &          zmaxLayer1_upper(3)
			end if
			
			deallocate(zmaxLayer1_upper)
			
			else if (parname.eq.'zmaxLayer2_upper') then
				allocate(zmaxLayer2_upper(nargs-3))
				read (line2, *, end = 999)xx1,zmaxLayer2_upper(1),      &
	 &          xx2, zmaxLayer2_upper(2),xx3,zmaxLayer2_upper(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'zmaxLayer2_upper_') then
				
				allocate(zmaxLayer2_upper_sampled(nargs))
				read (line2, *, end = 999)zmaxLayer2_upper_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      zmaxLayer2_upper_sampled(1),"|",                            &
	 &      zmaxLayer2_upper_sampled(2),"|",                            &
	 &      zmaxLayer2_upper_sampled(3)
			
				deallocate(zmaxLayer2_upper_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          zmaxLayer2_upper(1),"|",                                &
	 &          zmaxLayer2_upper(2),"|",                                &
	 &          zmaxLayer2_upper(3)
			end if
			
			deallocate(zmaxLayer2_upper)
			
			else if (parname.eq.'zmaxLayer3_upper') then
				allocate(zmaxLayer3_upper(nargs-3))
				read (line2, *, end = 999)xx1,zmaxLayer3_upper(1),      &
	 &          xx2, zmaxLayer3_upper(2),xx3,zmaxLayer3_upper(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'zmaxLayer3_upper_') then
				
				allocate(zmaxLayer3_upper_sampled(nargs))
				read (line2, *, end = 999)zmaxLayer3_upper_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      zmaxLayer3_upper_sampled(1),"|",                            &
	 &      zmaxLayer3_upper_sampled(2),"|",                            &
	 &      zmaxLayer3_upper_sampled(3)
			
				deallocate(zmaxLayer3_upper_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          zmaxLayer3_upper(1),"|",                                &
	 &          zmaxLayer3_upper(2),"|",                                &
	 &          zmaxLayer3_upper(3)
			end if
			
			deallocate(zmaxLayer3_upper)
			
			else if (parname.eq.'zmaxLayer4_upper') then
				allocate(zmaxLayer4_upper(nargs-3))
				read (line2, *, end = 999)xx1,zmaxLayer4_upper(1),      &
	 &          xx2, zmaxLayer4_upper(2),xx3,zmaxLayer4_upper(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'zmaxLayer4_upper_') then
				
				allocate(zmaxLayer4_upper_sampled(nargs))
				read (line2, *, end = 999)zmaxLayer4_upper_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      zmaxLayer4_upper_sampled(1),"|",                            &
	 &      zmaxLayer4_upper_sampled(2),"|",                            &
	 &      zmaxLayer4_upper_sampled(3)
			
				deallocate(zmaxLayer4_upper_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          zmaxLayer4_upper(1),"|",                                &
	 &          zmaxLayer4_upper(2),"|",                                &
	 &          zmaxLayer4_upper(3)
			end if
			
			deallocate(zmaxLayer4_upper)
			
			else if (parname.eq.'minTempUnloading') then
				allocate(minTempUnloading(nargs-3))
				read (line2, *, end = 999)xx1,minTempUnloading(1),      &
	 &          xx2, minTempUnloading(2),xx3,minTempUnloading(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'minTempUnloading_') then
				
				allocate(minTempUnloading_sampled(nargs))
				read (line2, *, end = 999)minTempUnloading_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      minTempUnloading_sampled(1),"|",                            &
	 &      minTempUnloading_sampled(2),"|",                            &
	 &      minTempUnloading_sampled(3)
			
				deallocate(minTempUnloading_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          minTempUnloading(1),"|",                                &
	 &          minTempUnloading(2),"|",                                &
	 &          minTempUnloading(3)
			end if
			
			deallocate(minTempUnloading)
			
			else if (parname.eq.'minWindUnloading') then
				allocate(minWindUnloading(nargs-3))
				read (line2, *, end = 999)xx1,minWindUnloading(1),      &
	 &          xx2, minWindUnloading(2),xx3,minWindUnloading(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'minWindUnloading_') then
				
				allocate(minWindUnloading_sampled(nargs))
				read (line2, *, end = 999)minWindUnloading_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      minWindUnloading_sampled(1),"|",                            &
	 &      minWindUnloading_sampled(2),"|",                            &
	 &      minWindUnloading_sampled(3)
			
				deallocate(minWindUnloading_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          minWindUnloading(1),"|",                                &
	 &          minWindUnloading(2),"|",                                &
	 &          minWindUnloading(3)
			end if
			
			deallocate(minWindUnloading)
			
			else if (parname.eq.'rateTempUnloading') then
				allocate(rateTempUnloading(nargs-3))
				read (line2, *, end = 999)xx1,rateTempUnloading(1),     &
	 &          xx2, rateTempUnloading(2),xx3,rateTempUnloading(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'rateTempUnloading_') then
				
				allocate(rateTempUnloading_sampled(nargs))
				read (line2, *, end = 999)rateTempUnloading_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",         &
	 &      rateTempUnloading_sampled(1),"|",                           &
	 &      rateTempUnloading_sampled(2),"|",                           &
	 &      rateTempUnloading_sampled(3)
			
				deallocate(rateTempUnloading_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
				write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",     &
	 &          rateTempUnloading(1),"|",                               &
	 &          rateTempUnloading(2),"|",                               &
	 &          rateTempUnloading(3)
			end if
			
			deallocate(rateTempUnloading)
			
			else if (parname.eq.'rateWindUnloading') then
				allocate(rateWindUnloading(nargs-3))
				read (line2, *, end = 999)xx1,rateWindUnloading(1),     &
	 &          xx2, rateWindUnloading(2),xx3,rateWindUnloading(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'rateWindUnloading_') then
				
				allocate(rateWindUnloading_sampled(nargs))
				read (line2, *, end = 999)rateWindUnloading_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",         &
	 &      rateWindUnloading_sampled(1),"|",                           &
	 &      rateWindUnloading_sampled(2),"|",                           &
	 &      rateWindUnloading_sampled(3)
			
				deallocate(rateWindUnloading_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
				write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",     &
	 &          rateWindUnloading(1),"|",                               &
	 &          rateWindUnloading(2),"|",                               &
	 &          rateWindUnloading(3)
			end if
			
			deallocate(rateWindUnloading)
			
			
			!Begin Sundials parameters
			!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
			
			else if (parname.eq.'relTolTempCas') then
				allocate(relTolTempCas(nargs-3))
				read (line2, *, end = 999)xx1,relTolTempCas(1),     &
	 &          xx2, relTolTempCas(2),xx3,relTolTempCas(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'relTolTempCas_') then
				
				allocate(relTolTempCas_sampled(nargs))
				read (line2, *, end = 999)relTolTempCas_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",         &
	 &      relTolTempCas_sampled(1),"|",                           &
	 &      relTolTempCas_sampled(2),"|",                           &
	 &      relTolTempCas_sampled(3)
			
				deallocate(relTolTempCas_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
				write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",     &
	 &          relTolTempCas(1),"|",                               &
	 &          relTolTempCas(2),"|",                               &
	 &          relTolTempCas(3)
			end if
			
			deallocate(relTolTempCas)
			
			
			
			else if (parname.eq.'absTolTempCas') then
				allocate(absTolTempCas(nargs-3))
				read (line2, *, end = 999)xx1,absTolTempCas(1),     &
	 &          xx2, absTolTempCas(2),xx3,absTolTempCas(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'absTolTempCas_') then
				
				allocate(absTolTempCas_sampled(nargs))
				read (line2, *, end = 999)absTolTempCas_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",         &
	 &      absTolTempCas_sampled(1),"|",                           &
	 &      absTolTempCas_sampled(2),"|",                           &
	 &      absTolTempCas_sampled(3)
			
				deallocate(absTolTempCas_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
				write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",     &
	 &          absTolTempCas(1),"|",                               &
	 &          absTolTempCas(2),"|",                               &
	 &          absTolTempCas(3)
			end if
			
			deallocate(absTolTempCas)
			
			else if (parname.eq.'relTolTempVeg') then
				allocate(relTolTempVeg(nargs-3))
				read (line2, *, end = 999)xx1,relTolTempVeg(1),     &
	 &          xx2, relTolTempVeg(2),xx3,relTolTempVeg(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'relTolTempVeg_') then
				
				allocate(relTolTempVeg_sampled(nargs))
				read (line2, *, end = 999)relTolTempVeg_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",         &
	 &      relTolTempVeg_sampled(1),"|",                           &
	 &      relTolTempVeg_sampled(2),"|",                           &
	 &      relTolTempVeg_sampled(3)
			
				deallocate(relTolTempVeg_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
				write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",     &
	 &          relTolTempVeg(1),"|",                               &
	 &          relTolTempVeg(2),"|",                               &
	 &          relTolTempVeg(3)
			end if
			
			deallocate(relTolTempVeg)
			
			else if (parname.eq.'absTolTempVeg') then
				allocate(absTolTempVeg(nargs-3))
				read (line2, *, end = 999)xx1,absTolTempVeg(1),     &
	 &          xx2, absTolTempVeg(2),xx3,absTolTempVeg(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'absTolTempVeg_') then
				
				allocate(absTolTempVeg_sampled(nargs))
				read (line2, *, end = 999)absTolTempVeg_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",         &
	 &      absTolTempVeg_sampled(1),"|",                           &
	 &      absTolTempVeg_sampled(2),"|",                           &
	 &      absTolTempVeg_sampled(3)
			
				deallocate(absTolTempVeg_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
				write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",     &
	 &          absTolTempVeg(1),"|",                               &
	 &          absTolTempVeg(2),"|",                               &
	 &          absTolTempVeg(3)
			end if
			
			deallocate(absTolTempVeg)
			
			
			else if (parname.eq.'relTolWatVeg') then
				allocate(relTolWatVeg(nargs-3))
				read (line2, *, end = 999)xx1,relTolWatVeg(1),     &
	 &          xx2, relTolWatVeg(2),xx3,relTolWatVeg(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'relTolWatVeg_') then
				
				allocate(relTolWatVeg_sampled(nargs))
				read (line2, *, end = 999)relTolWatVeg_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",         &
	 &      relTolWatVeg_sampled(1),"|",                           &
	 &      relTolWatVeg_sampled(2),"|",                           &
	 &      relTolWatVeg_sampled(3)
			
				deallocate(relTolWatVeg_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
				write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",     &
	 &          relTolWatVeg(1),"|",                               &
	 &          relTolWatVeg(2),"|",                               &
	 &          relTolWatVeg(3)
			end if
			
			deallocate(relTolWatVeg)
			
			
			else if (parname.eq.'absTolWatVeg') then
				allocate(absTolWatVeg(nargs-3))
				read (line2, *, end = 999)xx1,absTolWatVeg(1),     &
	 &          xx2, absTolWatVeg(2),xx3,absTolWatVeg(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'absTolWatVeg_') then
				
				allocate(absTolWatVeg_sampled(nargs))
				read (line2, *, end = 999)absTolWatVeg_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",         &
	 &      absTolWatVeg_sampled(1),"|",                           &
	 &      absTolWatVeg_sampled(2),"|",                           &
	 &      absTolWatVeg_sampled(3)
			
				deallocate(absTolWatVeg_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
				write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",     &
	 &          absTolWatVeg(1),"|",                               &
	 &          absTolWatVeg(2),"|",                               &
	 &          absTolWatVeg(3)
			end if
			
			deallocate(absTolWatVeg)
			
			
			else if (parname.eq.'relTolTempSoilSnow') then
				allocate(relTolTempSoilSnow(nargs-3))
				read (line2, *, end = 999)xx1,relTolTempSoilSnow(1),     &
	 &          xx2, relTolTempSoilSnow(2),xx3,relTolTempSoilSnow(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'relTolTempSoilSnow_') then
				
				allocate(relTolTempSoilSnow_sampled(nargs))
				read (line2, *, end = 999)relTolTempSoilSnow_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",         &
	 &      relTolTempSoilSnow_sampled(1),"|",                           &
	 &      relTolTempSoilSnow_sampled(2),"|",                           &
	 &      relTolTempSoilSnow_sampled(3)
			
				deallocate(relTolTempSoilSnow_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
				write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",     &
	 &          relTolTempSoilSnow(1),"|",                               &
	 &          relTolTempSoilSnow(2),"|",                               &
	 &          relTolTempSoilSnow(3)
			end if
			
			deallocate(relTolTempSoilSnow)
			
			else if (parname.eq.'absTolTempSoilSnow') then
				allocate(absTolTempSoilSnow(nargs-3))
				read (line2, *, end = 999)xx1,absTolTempSoilSnow(1),     &
	 &          xx2, absTolTempSoilSnow(2),xx3,absTolTempSoilSnow(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'absTolTempSoilSnow_') then
				
				allocate(absTolTempSoilSnow_sampled(nargs))
				read (line2, *, end = 999)absTolTempSoilSnow_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",         &
	 &      absTolTempSoilSnow_sampled(1),"|",                           &
	 &      absTolTempSoilSnow_sampled(2),"|",                           &
	 &      absTolTempSoilSnow_sampled(3)
			
				deallocate(absTolTempSoilSnow_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
				write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",     &
	 &          absTolTempSoilSnow(1),"|",                               &
	 &          absTolTempSoilSnow(2),"|",                               &
	 &          absTolTempSoilSnow(3)
			end if
			
			deallocate(absTolTempSoilSnow)
			
			else if (parname.eq.'relTolWatSnow') then
				allocate(relTolWatSnow(nargs-3))
				read (line2, *, end = 999)xx1,relTolWatSnow(1),     &
	 &          xx2, relTolWatSnow(2),xx3,relTolWatSnow(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'relTolWatSnow_') then
				
				allocate(relTolWatSnow_sampled(nargs))
				read (line2, *, end = 999)relTolWatSnow_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",         &
	 &      relTolWatSnow_sampled(1),"|",                           &
	 &      relTolWatSnow_sampled(2),"|",                           &
	 &      relTolWatSnow_sampled(3)
			
				deallocate(relTolWatSnow_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
				write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",     &
	 &          relTolWatSnow(1),"|",                               &
	 &          relTolWatSnow(2),"|",                               &
	 &          relTolWatSnow(3)
			end if
			
			deallocate(relTolWatSnow)
			
			
			else if (parname.eq.'absTolWatSnow') then
				allocate(absTolWatSnow(nargs-3))
				read (line2, *, end = 999)xx1,absTolWatSnow(1),     &
	 &          xx2, absTolWatSnow(2),xx3,absTolWatSnow(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'absTolWatSnow_') then
				
				allocate(absTolWatSnow_sampled(nargs))
				read (line2, *, end = 999)absTolWatSnow_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",         &
	 &      absTolWatSnow_sampled(1),"|",                           &
	 &      absTolWatSnow_sampled(2),"|",                           &
	 &      absTolWatSnow_sampled(3)
			
				deallocate(absTolWatSnow_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
				write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",     &
	 &          absTolWatSnow(1),"|",                               &
	 &          absTolWatSnow(2),"|",                               &
	 &          absTolWatSnow(3)
			end if
			
			deallocate(absTolWatSnow)
			
			
			else if (parname.eq.'relTolMatric') then
				allocate(relTolMatric(nargs-3))
				read (line2, *, end = 999)xx1,relTolMatric(1),     &
	 &          xx2, relTolMatric(2),xx3,relTolMatric(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'relTolMatric_') then
				
				allocate(relTolMatric_sampled(nargs))
				read (line2, *, end = 999)relTolMatric_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",         &
	 &      relTolMatric_sampled(1),"|",                           &
	 &      relTolMatric_sampled(2),"|",                           &
	 &      relTolMatric_sampled(3)
			
				deallocate(relTolMatric_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
				write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",     &
	 &          relTolMatric(1),"|",                               &
	 &          relTolMatric(2),"|",                               &
	 &          relTolMatric(3)
			end if
			
			deallocate(relTolMatric)
			
			
			else if (parname.eq.'absTolMatric') then
				allocate(absTolMatric(nargs-3))
				read (line2, *, end = 999)xx1,absTolMatric(1),     &
	 &          xx2, absTolMatric(2),xx3,absTolMatric(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'absTolMatric_') then
				
				allocate(absTolMatric_sampled(nargs))
				read (line2, *, end = 999)absTolMatric_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",         &
	 &      absTolMatric_sampled(1),"|",                           &
	 &      absTolMatric_sampled(2),"|",                           &
	 &      absTolMatric_sampled(3)
			
				deallocate(absTolMatric_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
				write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",     &
	 &          absTolMatric(1),"|",                               &
	 &          absTolMatric(2),"|",                               &
	 &          absTolMatric(3)
			end if
			
			deallocate(absTolMatric)
			
			
			else if (parname.eq.'relTolAquifr') then
				allocate(relTolAquifr(nargs-3))
				read (line2, *, end = 999)xx1,relTolAquifr(1),     &
	 &          xx2, relTolAquifr(2),xx3,relTolAquifr(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'relTolAquifr_') then
				
				allocate(relTolAquifr_sampled(nargs))
				read (line2, *, end = 999)relTolAquifr_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",         &
	 &      relTolAquifr_sampled(1),"|",                           &
	 &      relTolAquifr_sampled(2),"|",                           &
	 &      relTolAquifr_sampled(3)
			
				deallocate(relTolAquifr_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
				write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",     &
	 &          relTolAquifr(1),"|",                               &
	 &          relTolAquifr(2),"|",                               &
	 &          relTolAquifr(3)
			end if
			
			deallocate(relTolAquifr)
			
			
			else if (parname.eq.'absTolAquifr') then
				allocate(absTolAquifr(nargs-3))
				read (line2, *, end = 999)xx1,absTolAquifr(1),     &
	 &          xx2, absTolAquifr(2),xx3,absTolAquifr(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'absTolAquifr_') then
				
				allocate(absTolAquifr_sampled(nargs))
				read (line2, *, end = 999)absTolAquifr_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
			write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",         &
	 &      absTolAquifr_sampled(1),"|",                           &
	 &      absTolAquifr_sampled(2),"|",                           &
	 &      absTolAquifr_sampled(3)
			
				deallocate(absTolAquifr_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				!write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
				write(200,'(a25,1x,3(a1,1x,d12.4,1x))')parname,"|",     &
	 &          absTolAquifr(1),"|",                               &
	 &          absTolAquifr(2),"|",                               &
	 &          absTolAquifr(3)
			end if
			
			deallocate(absTolAquifr)
			
			!End Sundials parameters
			!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
			
			else if (parname.eq.'ep_corr') then
				allocate(ep_corr(nargs-3))
				read (line2, *, end = 999)xx1,ep_corr(1),               &
	 &          xx2, ep_corr(2),xx3,ep_corr(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'ep_corr_') then
				
				allocate(ep_corr_sampled(nargs))
				read (line2, *, end = 999)ep_corr_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      ep_corr_sampled(1),"|",                                     &
	 &      ep_corr_sampled(2),"|",                                     &
	 &      ep_corr_sampled(3)
			
				deallocate(ep_corr_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          ep_corr(1),"|",                                         &
	 &          ep_corr(2),"|",                                         &
	 &          ep_corr(3)
			end if
			
			deallocate(ep_corr)
			
			else if (parname.eq.'lowerBoundHead_corr') then
				allocate(lowerBoundHead_corr(nargs-3))
				read (line2, *, end = 999)xx1,lowerBoundHead_corr(1),   &
	 &          xx2, lowerBoundHead_corr(2),xx3,lowerBoundHead_corr(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'lowerBoundHead_corr_') then
				
				allocate(lowerBoundHead_corr_sampled(nargs))
				read (line2, *, end = 999)lowerBoundHead_corr_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      lowerBoundHead_corr_sampled(1),"|",                         &
	 &      lowerBoundHead_corr_sampled(2),"|",                         &
	 &      lowerBoundHead_corr_sampled(3)
			
				deallocate(lowerBoundHead_corr_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          lowerBoundHead_corr(1),"|",                             &
	 &          lowerBoundHead_corr(2),"|",                             &
	 &          lowerBoundHead_corr(3)
			end if
			
			deallocate(lowerBoundHead_corr)
			
			else if (parname.eq.'surfm_corr') then
				allocate(surfm_corr(nargs-3))
				read (line2, *, end = 999)xx1,surfm_corr(1),            &
	 &          xx2, surfm_corr(2),xx3,surfm_corr(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'surfm_corr_') then
				
				allocate(surfm_corr_sampled(nargs))
				read (line2, *, end = 999)surfm_corr_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      surfm_corr_sampled(1),"|",                                  &
	 &      surfm_corr_sampled(2),"|",                                  &
	 &      surfm_corr_sampled(3)
			
				deallocate(surfm_corr_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          surfm_corr(1),"|",                                      &
	 &          surfm_corr(2),"|",                                      &
	 &          surfm_corr(3)
			end if
			
			deallocate(surfm_corr)
			
			else if (parname.eq.'deprl_corr') then
				allocate(deprl_corr(nargs-3))
				read (line2, *, end = 999)xx1,deprl_corr(1),            &
	 &          xx2, deprl_corr(2),xx3,deprl_corr(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'deprl_corr_') then
				
				allocate(deprl_corr_sampled(nargs))
				read (line2, *, end = 999)deprl_corr_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      deprl_corr_sampled(1),"|",                                  &
	 &      deprl_corr_sampled(2),"|",                                  &
	 &      deprl_corr_sampled(3)
			
				deallocate(deprl_corr_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          deprl_corr(1),"|",                                      &
	 &          deprl_corr(2),"|",                                      &
	 &          deprl_corr(3)
			end if
			
			deallocate(deprl_corr)
			
			
			
			
			
			else if (parname.eq.'upper2deep') then
				allocate(upper2deep(nargs-3))
				read (line2, *, end = 999)xx1,upper2deep(1),            &
	 &          xx2, upper2deep(2),xx3,upper2deep(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'upper2deep_') then
				
				allocate(upper2deep_sampled(nargs))
				read (line2, *, end = 999)upper2deep_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      upper2deep_sampled(1),"|",                                  &
	 &      upper2deep_sampled(2),"|",                                  &
	 &      upper2deep_sampled(3)
			
				deallocate(upper2deep_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          upper2deep(1),"|",                                      &
	 &          upper2deep(2),"|",                                      &
	 &          upper2deep(3)
			end if
			
			deallocate(upper2deep)
			
			else if (parname.eq.'tcfriver') then
				allocate(tcfriver(nargs-3))
				read (line2, *, end = 999)xx1,tcfriver(1),              &
	 &          xx2, tcfriver(2),xx3,tcfriver(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'tcfriver_') then
				
				allocate(tcfriver_sampled(nargs))
				read (line2, *, end = 999)tcfriver_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      tcfriver_sampled(1),"|",                                    &
	 &      tcfriver_sampled(2),"|",                                    &
	 &      tcfriver_sampled(3)
			
				deallocate(tcfriver_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          tcfriver(1),"|",                                        &
	 &          tcfriver(2),"|",                                        &
	 &          tcfriver(3)
			end if
			
			deallocate(tcfriver)
			
			else if (parname.eq.'scfriver') then
				allocate(scfriver(nargs-3))
				read (line2, *, end = 999)xx1,scfriver(1),              &
	 &          xx2, scfriver(2),xx3,scfriver(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'scfriver_') then
				
				allocate(scfriver_sampled(nargs))
				read (line2, *, end = 999)scfriver_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      scfriver_sampled(1),"|",                                    &
	 &      scfriver_sampled(2),"|",                                    &
	 &      scfriver_sampled(3)
			
				deallocate(scfriver_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          scfriver(1),"|",                                        &
	 &          scfriver(2),"|",                                        &
	 &          scfriver(3)
			end if
			
			deallocate(scfriver)
			
			else if (parname.eq.'ccfriver') then
				allocate(ccfriver(nargs-3))
				read (line2, *, end = 999)xx1,ccfriver(1),              &
	 &          xx2, ccfriver(2),xx3,ccfriver(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'ccfriver_') then
				
				allocate(ccfriver_sampled(nargs))
				read (line2, *, end = 999)ccfriver_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      ccfriver_sampled(1),"|",                                    &
	 &      ccfriver_sampled(2),"|",                                    &
	 &      ccfriver_sampled(3)
			
				deallocate(ccfriver_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          ccfriver(1),"|",                                        &
	 &          ccfriver(2),"|",                                        &
	 &          ccfriver(3)
			end if
			
			deallocate(ccfriver)
			
			else if (parname.eq.'lcfriver') then
				allocate(lcfriver(nargs-3))
				read (line2, *, end = 999)xx1,lcfriver(1),              &
	 &          xx2, lcfriver(2),xx3,lcfriver(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'lcfriver_') then
				
				allocate(lcfriver_sampled(nargs))
				read (line2, *, end = 999)lcfriver_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      lcfriver_sampled(1),"|",                                    &
	 &      lcfriver_sampled(2),"|",                                    &
	 &      lcfriver_sampled(3)
			
				deallocate(lcfriver_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          lcfriver(1),"|",                                        &
	 &          lcfriver(2),"|",                                        &
	 &          lcfriver(3)
			end if
			
			deallocate(lcfriver)
			
			else if (parname.eq.'tcflake') then
				allocate(tcflake(nargs-3))
				read (line2, *, end = 999)xx1,tcflake(1),               &
	 &          xx2, tcflake(2),xx3,tcflake(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'tcflake_') then
				
				allocate(tcflake_sampled(nargs))
				read (line2, *, end = 999)tcflake_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      tcflake_sampled(1),"|",                                     &
	 &      tcflake_sampled(2),"|",                                     &
	 &      tcflake_sampled(3)
			
				deallocate(tcflake_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          tcflake(1),"|",                                         &
	 &          tcflake(2),"|",                                         &
	 &          tcflake(3)
			end if
			
			deallocate(tcflake)
			
			else if (parname.eq.'scflake') then
				allocate(scflake(nargs-3))
				read (line2, *, end = 999)xx1,scflake(1),               &
	 &          xx2, scflake(2),xx3,scflake(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'scflake_') then
				
				allocate(scflake_sampled(nargs))
				read (line2, *, end = 999)scflake_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      scflake_sampled(1),"|",                                     &
	 &      scflake_sampled(2),"|",                                     &
	 &      scflake_sampled(3)
			
				deallocate(scflake_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          scflake(1),"|",                                         &
	 &          scflake(2),"|",                                         &
	 &          scflake(3)
			end if
			
			deallocate(scflake)
			
			else if (parname.eq.'ccflake') then
				allocate(ccflake(nargs-3))
				read (line2, *, end = 999)xx1,ccflake(1),               &
	 &          xx2, ccflake(2),xx3,ccflake(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'ccflake_') then
				
				allocate(ccflake_sampled(nargs))
				read (line2, *, end = 999)ccflake_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      ccflake_sampled(1),"|",                                     &
	 &      ccflake_sampled(2),"|",                                     &
	 &      ccflake_sampled(3)
			
				deallocate(ccflake_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          ccflake(1),"|",                                         &
	 &          ccflake(2),"|",                                         &
	 &          ccflake(3)
			end if
			
			deallocate(ccflake)
			
			else if (parname.eq.'lcflake') then
				allocate(lcflake(nargs-3))
				read (line2, *, end = 999)xx1,lcflake(1),               &
	 &          xx2, lcflake(2),xx3,lcflake(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'lcflake_') then
				
				allocate(lcflake_sampled(nargs))
				read (line2, *, end = 999)lcflake_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      lcflake_sampled(1),"|",                                     &
	 &      lcflake_sampled(2),"|",                                     &
	 &      lcflake_sampled(3)
			
				deallocate(lcflake_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          lcflake(1),"|",                                         &
	 &          lcflake(2),"|",                                         &
	 &          lcflake(3)
			end if
			
			deallocate(lcflake)
			
			else if (parname.eq.'stbcorr1') then
				allocate(stbcorr1(nargs-3))
				read (line2, *, end = 999)xx1,stbcorr1(1),              &
	 &          xx2, stbcorr1(2),xx3,stbcorr1(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'stbcorr1_') then
				
				allocate(stbcorr1_sampled(nargs))
				read (line2, *, end = 999)stbcorr1_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      stbcorr1_sampled(1),"|",                                    &
	 &      stbcorr1_sampled(2),"|",                                    &
	 &      stbcorr1_sampled(3)
			
				deallocate(stbcorr1_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          stbcorr1(1),"|",                                        &
	 &          stbcorr1(2),"|",                                        &
	 &          stbcorr1(3)
			end if
			
			deallocate(stbcorr1)
			
			else if (parname.eq.'stbcorr2') then
				allocate(stbcorr2(nargs-3))
				read (line2, *, end = 999)xx1,stbcorr2(1),              &
	 &          xx2, stbcorr2(2),xx3,stbcorr2(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'stbcorr2_') then
				
				allocate(stbcorr2_sampled(nargs))
				read (line2, *, end = 999)stbcorr2_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      stbcorr2_sampled(1),"|",                                    &
	 &      stbcorr2_sampled(2),"|",                                    &
	 &      stbcorr2_sampled(3)
			
				deallocate(stbcorr2_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          stbcorr2(1),"|",                                        &
	 &          stbcorr2(2),"|",                                        &
	 &          stbcorr2(3)
			end if
			
			deallocate(stbcorr2)
			
			else if (parname.eq.'licewme') then
				allocate(licewme(nargs-3))
				read (line2, *, end = 999)xx1,licewme(1),               &
	 &          xx2, licewme(2),xx3,licewme(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'licewme_') then
				
				allocate(licewme_sampled(nargs))
				read (line2, *, end = 999)licewme_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      licewme_sampled(1),"|",                                     &
	 &      licewme_sampled(2),"|",                                     &
	 &      licewme_sampled(3)
			
				deallocate(licewme_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          licewme(1),"|",                                         &
	 &          licewme(2),"|",                                         &
	 &          licewme(3)
			end if
			
			deallocate(licewme)
			
			else if (parname.eq.'licetf') then
				allocate(licetf(nargs-3))
				read (line2, *, end = 999)xx1,licetf(1),                &
	 &          xx2, licetf(2),xx3,licetf(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'licetf_') then
				
				allocate(licetf_sampled(nargs))
				read (line2, *, end = 999)licetf_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      licetf_sampled(1),"|",                                      &
	 &      licetf_sampled(2),"|",                                      &
	 &      licetf_sampled(3)
			
				deallocate(licetf_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          licetf(1),"|",                                          &
	 &          licetf(2),"|",                                          &
	 &          licetf(3)
			end if
			
			deallocate(licetf)
			
			else if (parname.eq.'licesndens') then
				allocate(licesndens(nargs-3))
				read (line2, *, end = 999)xx1,licesndens(1),            &
	 &          xx2, licesndens(2),xx3,licesndens(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'licesndens_') then
				
				allocate(licesndens_sampled(nargs))
				read (line2, *, end = 999)licesndens_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      licesndens_sampled(1),"|",                                  &
	 &      licesndens_sampled(2),"|",                                  &
	 &      licesndens_sampled(3)
			
				deallocate(licesndens_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          licesndens(1),"|",                                      &
	 &          licesndens(2),"|",                                      &
	 &          licesndens(3)
			end if
			
			deallocate(licesndens)
			
			else if (parname.eq.'licekika') then
				allocate(licekika(nargs-3))
				read (line2, *, end = 999)xx1,licekika(1),              &
	 &          xx2, licekika(2),xx3,licekika(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'licekika_') then
				
				allocate(licekika_sampled(nargs))
				read (line2, *, end = 999)licekika_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      licekika_sampled(1),"|",                                    &
	 &      licekika_sampled(2),"|",                                    &
	 &      licekika_sampled(3)
			
				deallocate(licekika_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          licekika(1),"|",                                        &
	 &          licekika(2),"|",                                        &
	 &          licekika(3)
			end if
			
			deallocate(licekika)
			
			else if (parname.eq.'licekexp') then
				allocate(licekexp(nargs-3))
				read (line2, *, end = 999)xx1,licekexp(1),              &
	 &          xx2, licekexp(2),xx3,licekexp(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'licekexp_') then
				
				allocate(licekexp_sampled(nargs))
				read (line2, *, end = 999)licekexp_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      licekexp_sampled(1),"|",                                    &
	 &      licekexp_sampled(2),"|",                                    &
	 &      licekexp_sampled(3)
			
				deallocate(licekexp_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          licekexp(1),"|",                                        &
	 &          licekexp(2),"|",                                        &
	 &          licekexp(3)
			end if
			
			deallocate(licekexp)
			
			else if (parname.eq.'licetmelt') then
				allocate(licetmelt(nargs-3))
				read (line2, *, end = 999)xx1,licetmelt(1),             &
	 &          xx2, licetmelt(2),xx3,licetmelt(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'licetmelt_') then
				
				allocate(licetmelt_sampled(nargs))
				read (line2, *, end = 999)licetmelt_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      licetmelt_sampled(1),"|",                                   &
	 &      licetmelt_sampled(2),"|",                                   &
	 &      licetmelt_sampled(3)
			
				deallocate(licetmelt_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          licetmelt(1),"|",                                       &
	 &          licetmelt(2),"|",                                       &
	 &          licetmelt(3)
			end if
			
			deallocate(licetmelt)
			
			else if (parname.eq.'licewcorr') then
				allocate(licewcorr(nargs-3))
				read (line2, *, end = 999)xx1,licewcorr(1),             &
	 &          xx2, licewcorr(2),xx3,licewcorr(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'licewcorr_') then
				
				allocate(licewcorr_sampled(nargs))
				read (line2, *, end = 999)licewcorr_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      licewcorr_sampled(1),"|",                                   &
	 &      licewcorr_sampled(2),"|",                                   &
	 &      licewcorr_sampled(3)
			
				deallocate(licewcorr_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          licewcorr(1),"|",                                       &
	 &          licewcorr(2),"|",                                       &
	 &          licewcorr(3)
			end if
			
			deallocate(licewcorr)
			
			else if (parname.eq.'ricewme') then
				allocate(ricewme(nargs-3))
				read (line2, *, end = 999)xx1,ricewme(1),               &
	 &          xx2, ricewme(2),xx3,ricewme(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'ricewme_') then
				
				allocate(ricewme_sampled(nargs))
				read (line2, *, end = 999)ricewme_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      ricewme_sampled(1),"|",                                     &
	 &      ricewme_sampled(2),"|",                                     &
	 &      ricewme_sampled(3)
			
				deallocate(ricewme_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          ricewme(1),"|",                                         &
	 &          ricewme(2),"|",                                         &
	 &          ricewme(3)
			end if
			
			deallocate(ricewme)
			
			else if (parname.eq.'ricetf') then
				allocate(ricetf(nargs-3))
				read (line2, *, end = 999)xx1,ricetf(1),                &
	 &          xx2, ricetf(2),xx3,ricetf(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'ricetf_') then
				
				allocate(ricetf_sampled(nargs))
				read (line2, *, end = 999)ricetf_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      ricetf_sampled(1),"|",                                      &
	 &      ricetf_sampled(2),"|",                                      &
	 &      ricetf_sampled(3)
			
				deallocate(ricetf_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          ricetf(1),"|",                                          &
	 &          ricetf(2),"|",                                          &
	 &          ricetf(3)
			end if
			
			deallocate(ricetf)
			
			else if (parname.eq.'ricesndens') then
				allocate(ricesndens(nargs-3))
				read (line2, *, end = 999)xx1,ricesndens(1),            &
	 &          xx2, ricesndens(2),xx3,ricesndens(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'ricesndens_') then
				
				allocate(ricesndens_sampled(nargs))
				read (line2, *, end = 999)ricesndens_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      ricesndens_sampled(1),"|",                                  &
	 &      ricesndens_sampled(2),"|",                                  &
	 &      ricesndens_sampled(3)
			
				deallocate(ricesndens_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          ricesndens(1),"|",                                      &
	 &          ricesndens(2),"|",                                      &
	 &          ricesndens(3)
			end if
			
			deallocate(ricesndens)
			
			else if (parname.eq.'ricekika') then
				allocate(ricekika(nargs-3))
				read (line2, *, end = 999)xx1,ricekika(1),              &
	 &          xx2, ricekika(2),xx3,ricekika(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'ricekika_') then
				
				allocate(ricekika_sampled(nargs))
				read (line2, *, end = 999)ricekika_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      ricekika_sampled(1),"|",                                    &
	 &      ricekika_sampled(2),"|",                                    &
	 &      ricekika_sampled(3)
			
				deallocate(ricekika_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          ricekika(1),"|",                                        &
	 &          ricekika(2),"|",                                        &
	 &          ricekika(3)
			end if
			
			deallocate(ricekika)
			
			else if (parname.eq.'ricekexp') then
				allocate(ricekexp(nargs-3))
				read (line2, *, end = 999)xx1,ricekexp(1),              &
	 &          xx2, ricekexp(2),xx3,ricekexp(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'ricekexp_') then
				
				allocate(ricekexp_sampled(nargs))
				read (line2, *, end = 999)ricekexp_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      ricekexp_sampled(1),"|",                                    &
	 &      ricekexp_sampled(2),"|",                                    &
	 &      ricekexp_sampled(3)
			
				deallocate(ricekexp_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          ricekexp(1),"|",                                        &
	 &          ricekexp(2),"|",                                        &
	 &          ricekexp(3)
			end if
			
			deallocate(ricekexp)
			
			else if (parname.eq.'ricetmelt') then
			
			allocate(ricetmelt(nargs-3))
			read (line2, *, end = 999)ricetmelt
			read (line2, *, end = 999)xx1,ricetmelt(1),           &
	 &          xx2, ricetmelt(2),xx3,ricetmelt(3)
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'ricetmelt_') then
				
				allocate(ricetmelt_sampled(nargs))
				read (line2, *, end = 999)ricetmelt_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      ricetmelt_sampled(1),"|",                                   &
	 &      ricetmelt_sampled(2),"|",                                   &
	 &      ricetmelt_sampled(3)
			
				deallocate(ricetmelt_sampled)
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          ricetmelt(1),"|",                                       &
	 &          ricetmelt(2),"|",                                       &
	 &          ricetmelt(3)
			end if
			
			deallocate(ricetmelt)
			
			

			
			!/*//*/*/*/*/*/*/*/*/*/*/*/*/
			
			else if (parname.eq.'heightCanopyBottom') then
			allocate(heightCanopyBottom(nargs-3))
			read (line2, *, end = 999)xx1,heightCanopyBottom(1),        &
	 &          xx2, heightCanopyBottom(2),xx3,heightCanopyBottom(3)
			
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'heightCanopyBottom_') then
				
				allocate(heightCanopyBottom_sampled(nargs))
				read (line2, *, end = 999)heightCanopyBottom_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      heightCanopyBottom_sampled(1),"|",                          &
	 &      heightCanopyBottom_sampled(2),"|",                          &
	 &      heightCanopyBottom_sampled(3)
			
				deallocate(heightCanopyBottom_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(200,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          heightCanopyBottom(1),"|",                              &
	 &          heightCanopyBottom(2),"|",                              &
	 &          heightCanopyBottom(3)
			end if
			
			deallocate(heightCanopyBottom)
			
			!/*/*/*/*/*/*/*/*/*/*/*/*/*/
			
			!Basin parameters (begin)
			else if (parname.eq.'basin__aquiferHydCond') then
			allocate(basin__aquiferHydCond(nargs-3))
			read (line2, *, end = 999)xx1,basin__aquiferHydCond(1),        &
	 &          xx2, basin__aquiferHydCond(2),xx3,basin__aquiferHydCond(3)
			
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'basin__aquiferHydCond_') then
				
				allocate(basin__aquiferHydCond_sampled(nargs))
				read (line2, *, end = 999)basin__aquiferHydCond_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(300,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      basin__aquiferHydCond_sampled(1),"|",                          &
	 &      basin__aquiferHydCond_sampled(2),"|",                          &
	 &      basin__aquiferHydCond_sampled(3)
			
				deallocate(basin__aquiferHydCond_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(300,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          basin__aquiferHydCond(1),"|",                              &
	 &          basin__aquiferHydCond(2),"|",                              &
	 &          basin__aquiferHydCond(3)
			end if
			
			deallocate(basin__aquiferHydCond)
			
			else if (parname.eq.'routingGammaShape') then
			allocate(routingGammaShape(nargs-3))
			read (line2, *, end = 999)xx1,routingGammaShape(1),        &
	 &          xx2, routingGammaShape(2),xx3,routingGammaShape(3)
			
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'routingGammaShape_') then
				
				allocate(routingGammaShape_sampled(nargs))
				read (line2, *, end = 999)routingGammaShape_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(300,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      routingGammaShape_sampled(1),"|",                          &
	 &      routingGammaShape_sampled(2),"|",                          &
	 &      routingGammaShape_sampled(3)
			
				deallocate(routingGammaShape_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(300,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          routingGammaShape(1),"|",                              &
	 &          routingGammaShape(2),"|",                              &
	 &          routingGammaShape(3)
			end if
			
			deallocate(routingGammaShape)
			
			else if (parname.eq.'routingGammaScale') then
			allocate(routingGammaScale(nargs-3))
			read (line2, *, end = 999)xx1,routingGammaScale(1),        &
	 &          xx2, routingGammaScale(2),xx3,routingGammaScale(3)
			
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'routingGammaScale_') then
				
				allocate(routingGammaScale_sampled(nargs))
				read (line2, *, end = 999)routingGammaScale_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(300,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      routingGammaScale_sampled(1),"|",                          &
	 &      routingGammaScale_sampled(2),"|",                          &
	 &      routingGammaScale_sampled(3)
			
				deallocate(routingGammaScale_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(300,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          routingGammaScale(1),"|",                              &
	 &          routingGammaScale(2),"|",                              &
	 &          routingGammaScale(3)
			end if
			
			deallocate(routingGammaScale)
			
			!Basin parameters (end)
			else

			!print*,'parameter name ',parname, 'is not listed in the default parameter file'

			end if
			
			else                           !!skip the lines that do not begin with a parameter name (continue)
			write(200,'(a70)')trim(adjustl(line))
			end if                         !!skip the lines that do not begin with a parameter name (end)

		end do
		
999		continue


        close(200)
		
		
		
		
		!Basin parameters::::
		
		
		do


			read (2000, '(A)', iostat = eof) line
			!write(*,*)"printing the line chunk"
			!write(*,*)line
			if (eof < 0) exit
			
			line = adjustl(line)
            read(line,'(a1)')char1
			char1 = trim(char1)
			!write(*,*)"printing char1"
			!write(*,*)char1
			
            if((char1.ne.'!').or.(char1.ne."'")) then                              !skip the lines that do not begin with a parameter name (begin)
			  
			
			
			
			
			call parse(line,delims,args,nargs)
			
			parname = trim(args(1))
			!write(*,*)"printing parname"
			!write(*,*)parname
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			parnamexx = trim(parname) // trim(xx)
			!write(*,*)"printing parnamexx"
			!write(*,*)parnamexx	
				
			counter2 = 0
			
			
			if (parname.eq.'basin__aquiferHydCond') then
			allocate(basin__aquiferHydCond(nargs-3))
			read (line2, *, end = 9999)xx1,basin__aquiferHydCond(1),        &
	 &          xx2, basin__aquiferHydCond(2),xx3,basin__aquiferHydCond(3)
			
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'basin__aquiferHydCond_') then
				
				allocate(basin__aquiferHydCond_sampled(nargs))
				read (line2, *, end = 9999)basin__aquiferHydCond_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(300,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      basin__aquiferHydCond_sampled(1),"|",                          &
	 &      basin__aquiferHydCond_sampled(2),"|",                          &
	 &      basin__aquiferHydCond_sampled(3)
			
				deallocate(basin__aquiferHydCond_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(300,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          basin__aquiferHydCond(1),"|",                              &
	 &          basin__aquiferHydCond(2),"|",                              &
	 &          basin__aquiferHydCond(3)
			end if
			
			deallocate(basin__aquiferHydCond)
			
			else if (parname.eq.'routingGammaShape') then
			allocate(routingGammaShape(nargs-3))
			read (line2, *, end = 9999)xx1,routingGammaShape(1),        &
	 &          xx2, routingGammaShape(2),xx3,routingGammaShape(3)
			
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'routingGammaShape_') then
				
				allocate(routingGammaShape_sampled(nargs))
				read (line2, *, end = 9999)routingGammaShape_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(300,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      routingGammaShape_sampled(1),"|",                          &
	 &      routingGammaShape_sampled(2),"|",                          &
	 &      routingGammaShape_sampled(3)
			
				deallocate(routingGammaShape_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(300,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          routingGammaShape(1),"|",                              &
	 &          routingGammaShape(2),"|",                              &
	 &          routingGammaShape(3)
			end if
			
			deallocate(routingGammaShape)
			
			else if (parname.eq.'routingGammaScale') then
			allocate(routingGammaScale(nargs-3))
			read (line2, *, end = 9999)xx1,routingGammaScale(1),        &
	 &          xx2, routingGammaScale(2),xx3,routingGammaScale(3)
			
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'routingGammaScale_') then
				
				allocate(routingGammaScale_sampled(nargs))
				read (line2, *, end = 9999)routingGammaScale_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(300,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      routingGammaScale_sampled(1),"|",                          &
	 &      routingGammaScale_sampled(2),"|",                          &
	 &      routingGammaScale_sampled(3)
			
				deallocate(routingGammaScale_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(300,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          routingGammaScale(1),"|",                              &
	 &          routingGammaScale(2),"|",                              &
	 &          routingGammaScale(3)
			end if
			
			deallocate(routingGammaScale)
			
			
			            else if (parname.eq.'basin__aquiferScaleFactor') then
			allocate(basin__aquiferScaleFactor(nargs-3))
			read (line2, *, end = 9999)xx1,basin__aquiferScaleFactor(1),        &
	 &          xx2, basin__aquiferScaleFactor(2),xx3,basin__aquiferScaleFactor(3)
			
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'basin__aquiferScaleFactor_') then
				
				allocate(basin__aquiferScaleFactor_sampled(nargs))
				read (line2, *, end = 9999)basin__aquiferScaleFactor_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(300,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      basin__aquiferScaleFactor_sampled(1),"|",                          &
	 &      basin__aquiferScaleFactor_sampled(2),"|",                          &
	 &      basin__aquiferScaleFactor_sampled(3)
			
				deallocate(basin__aquiferScaleFactor_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(300,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          basin__aquiferScaleFactor(1),"|",                              &
	 &          basin__aquiferScaleFactor(2),"|",                              &
	 &          basin__aquiferScaleFactor(3)
			end if
			
			deallocate(basin__aquiferScaleFactor)
			
			
			            else if (parname.eq.'basin__aquiferBaseflowExp') then
			allocate(basin__aquiferBaseflowExp(nargs-3))
			read (line2, *, end = 9999)xx1,basin__aquiferBaseflowExp(1),        &
	 &          xx2, basin__aquiferBaseflowExp(2),xx3,basin__aquiferBaseflowExp(3)
			
			
			
			counter2 = 0
		if(parname_list(summa_parameter_ident(trim(parnamexx))) .ne. " ") then
			
			
			!----------------------------------------------
			
			
			open(100,file="model.in", status = "old")

		
		eof=0
		
		do
			read(100,'(A)',iostat=eof) line
			if (eof<0) exit
			call parse(line,delims,args,nargs)
			
			parname2 = trim(args(1))
			line2 = line((len(trim(args(1)))+1):len(line))
			call parse(line2,delims,args,nargs)
			
			
			
			
			
			if (parname2.eq.'basin__aquiferBaseflowExp_') then
				
				allocate(basin__aquiferBaseflowExp_sampled(nargs))
				read (line2, *, end = 9999)basin__aquiferBaseflowExp_sampled
				
			
			end if
			
		
			
			
		end do
		

		close(100)
		
			!----------------------------------------------
		
			
			write(300,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",         &
	 &      basin__aquiferBaseflowExp_sampled(1),"|",                          &
	 &      basin__aquiferBaseflowExp_sampled(2),"|",                          &
	 &      basin__aquiferBaseflowExp_sampled(3)
			
				deallocate(basin__aquiferBaseflowExp_sampled)
				
				
					counter2 = counter2 + 1
		end if
			
			if(counter2 == 0) then
				write(300,'(a25,1x,3(a1,1x,f12.4,1x))')parname,"|",     &
	 &          basin__aquiferBaseflowExp(1),"|",                              &
	 &          basin__aquiferBaseflowExp(2),"|",                              &
	 &          basin__aquiferBaseflowExp(3)
			end if
			
			deallocate(basin__aquiferBaseflowExp)
			
			
			
			!Basin parameters (end)
			else

			!print*,'parameter name ',parname, 'is not listed in the default parameter file'

			end if
			
			else                           !!skip the lines that do not begin with a parameter name (continue)
			write(300,'(a70)')trim(adjustl(line))
			end if                         !!skip the lines that do not begin with a parameter name (end)

		end do

		
9999		continue


        close(300)
		



end program summa_parameter_editor

