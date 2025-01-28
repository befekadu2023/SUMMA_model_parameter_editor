      function summa_parameter_ident(name)
    
      

    
      integer :: summa_parameter_ident
      
      character(len=*), intent (in) :: name
      
      summa_parameter_ident = 0
      
      if (name == 'upperBoundHead_') then					!!upper boundary condition for matric head (m)
          summa_parameter_ident = 1
      else if (name == 'lowerBoundHead_') then			!!lower boundary condition for matric head (m)
          summa_parameter_ident = 2
      else if (name == 'upperBoundTheta_') then			!!upper boundary condition for volumetric liquid water content (-)
          summa_parameter_ident = 3
          
      else if (name == 'lowerBoundTheta_') then			!!lower boundary condition for volumetric liquid water content (-)
          summa_parameter_ident = 4
          
      else if (name == 'upperBoundTemp_') then			!!temperature of the upper boundary (K)
          summa_parameter_ident = 5
          
      else if (name == 'lowerBoundTemp_') then			!!temperature of the lower boundary (K)
          summa_parameter_ident = 6
          
      else if (name == 'tempCritRain_') then			!!critical temperature where precipitation is rain (k)
          summa_parameter_ident = 7
          
     
      else if (name == 'tempRangeTimestep_') then		!!temperature range over the time step (k)
          summa_parameter_ident = 8
          
       else if (name == 'frozenPrecipMultip_') then		!!frozen precipitation multiplier (-)
          summa_parameter_ident = 9
          
      else if (name == 'snowfrz_scale_') then			!!scaling parameter for the freezing curve for snow (K-1)
          summa_parameter_ident = 10
          
      else if (name == 'fixedThermalCond_snow_') then	!!temporally constant thermal conductivity for snow (W m-1 K-1)
          summa_parameter_ident = 11
          
      else if (name == 'albedoMax_') then			    !!maximum snow albedo (single spectral band (-)
          summa_parameter_ident = 12
          
      else if (name == 'albedoMinWinter_') then			!!minimum snow albedo during winter (single spectral band) (-)
          summa_parameter_ident = 13
          
      else if (name == 'albedoMinSpring_') then			!!minimum snow albedo during spring (single spectral band) (-)
          summa_parameter_ident = 14
          
       else if (name == 'albedoMaxVisible_') then		!!maximum snow albedo in the visible part of the spectrum (-)
          summa_parameter_ident = 15
          
      else if (name == 'albedoMinVisible_') then		!!minimum snow albedo in the visible part of the spectrum (-)
          summa_parameter_ident = 16
          
      else if (name == 'albedoMaxNearIR_') then			!!maximum snow albedo in the near infra-red part of the spectrum (-)
          summa_parameter_ident = 17
          
      else if (name == 'albedoMinNearIR_') then			!!minimum snow albedo in the near infra-red part of the spectrum (-)
          summa_parameter_ident = 18
          
      else if (name == 'albedoDecayRate_') then			!!albedo decay rate (-)
          summa_parameter_ident = 19
          
       
      else if (name == 'albedoSootLoad_') then		    !!soot load factor (-)
          summa_parameter_ident = 20
          
      else if (name == 'albedoRefresh_') then			!!critical mass necessary for albedo refreshment (kg m-2')
          summa_parameter_ident = 21
          
      else if (name == 'radExt_snow_') then			    !!extinction coefficient for radiation penetration into snowpack (m-1)
          summa_parameter_ident = 22
		  
	   else if (name == 'directScale_') then			!!scaling factor for fractional driect radiaion parameterization (-)
          summa_parameter_ident = 23
          
      else if (name == 'Frad_direct_') then			    !!fraction direct solar radiation (-)
          summa_parameter_ident = 24
          
      else if (name == 'Frad_vis_') then		        !!fraction radiation in visible part of spectrum (-)
          summa_parameter_ident = 25
	 else if (name == 'newSnowDenMin_') then			!!minimum new snow density (kg m-3)
          summa_parameter_ident = 26
          
      else if (name == 'newSnowDenMult_') then			!!multiplier for new snow density (kg m-3)
         
      else if (name == 'newSnowDenScal_') then			!!scaling factor for new snow density (k)
          summa_parameter_ident = 28
          
      else if (name == 'constSnowDen_') then			!!Constant new snow density (kg m-3)
          summa_parameter_ident = 29
		  
	 else if (name == 'newSnowDenAdd_') then			!!Pahaut 1976, additive factor for new snow density (kg m-3)
          summa_parameter_ident = 30
         
      else if (name == 'newSnowDenMultTemp_') then		!!Pahaut 1976, multiplier for new snow density for air temperature (kg m-3 K-1)
          summa_parameter_ident = 31
          
      else if (name == 'newSnowDenMultWind_') then		!!Pahaut 1976, multiplier for new snow density for wind speed (kg m-7/2 s-1/2)
          summa_parameter_ident = 32
	  else if (name == 'newSnowDenMultAnd_') then		!!Anderson 1976, multiplier for new snow density (Anderson func) (K-1)
          summa_parameter_ident = 33
	  
	  else if (name == 'newSnowDenBase_') then		    !!Anderson 1976, base value that is rasied to the (3/2) power (K)
          continue
			summa_parameter_ident = 34
	  else if(name == 'densScalGrowth_') then			!!density scaling factor for grain growth (kg-1 m3)
	  summa_parameter_ident = 35
	  
	  else if(name == 'tempScalGrowth_') then			!!temperature scaling factor for grain growth (K-1)
	  summa_parameter_ident = 36
	  
	  else if(name == 'grainGrowthRate_') then			!!rate of grain growth (s-1)
	  summa_parameter_ident = 37
	  
	  else if(name == 'densScalOvrbdn_') then			!!density scaling factor for overburden pressure (kg-1 m3)
	  summa_parameter_ident = 38
	  
	  else if(name == 'tempScalOvrbdn_') then			!!temperature scaling factor for overburden pressure (K-1)
	  summa_parameter_ident = 39
	  
	  else if(name == 'baseViscosity_') then			!!viscosity coefficient at T=T_frz and snow density=0 (kg s m-2)
	  summa_parameter_ident = 40
	  
	  else if(name == 'Fcapil_') then			        !!capillary retention (fraction of total pore volume) (-)
	  summa_parameter_ident = 41
	  
	  else if(name == 'k_snow_') then			        !!hydraulic conductivity of snow (m s-1)
	  summa_parameter_ident = 42
	  
	  else if(name == 'mw_exp_') then			        !!exponent for meltwater flow (-)
	  summa_parameter_ident = 43
	  
	  else if(name == 'z0Snow_') then			        !!roughness length of snow (m)
	  summa_parameter_ident = 44
	  
	  else if(name == 'z0Soil_') then			        !!roughness length of bare soil below the canopy (m)
	  summa_parameter_ident = 45
	  
	  else if(name == 'z0Canopy_') then			        !!roughness length of the canopy (m)
	  summa_parameter_ident = 46

	  else if (name == 'zpdFraction_') then			    !! zero plane displacement / canopy height (-)
          summa_parameter_ident = 47
	  else if (name == 'critRichNumber_') then			!!critical value for the bulk Richardson number (-)
          summa_parameter_ident = 48
      else if (name == 'Louis79_bparam_') then			!!arameter in Louis (1979) stability function (-)
          summa_parameter_ident = 49
          
      else if (name == 'Louis79_cStar_') then			!!parameter in Louis (1979) stability function (-)
          summa_parameter_ident = 50
          
      else if (name == 'Mahrt87_eScale_') then			!!exponential scaling factor in the Mahrt (1987) stability function (-)
          summa_parameter_ident = 51
          
      else if (name == 'leafExchangeCoeff_') then		!!turbulent exchange coeff between canopy surface and canopy air (m s-(1/2))  
          summa_parameter_ident = 52
          
      else if (name == 'windReductionParam_') then		!!canopy wind reduction parameter (-)
          summa_parameter_ident = 53
          
     
      else if (name == 'Kc25_') then			        !!Michaelis-Menten constant for CO2 at 25 degrees C (umol mol-1)
          summa_parameter_ident = 54
          
       else if (name == 'Ko25_') then		            !!Michaelis-Menten constant for O2 at 25 degrees C (mol mol-1)
          summa_parameter_ident = 55
          
      else if (name == 'Kc_qFac_') then			        !!factor in the q10 function defining temperature controls on Kc (-)
          summa_parameter_ident = 56
          
      else if (name == 'Ko_qFac_') then			        !!actor in the q10 function defining temperature controls on Ko (-)
          summa_parameter_ident = 57
          
      else if (name == 'kc_Ha_') then			        !!activation energy for the Michaelis-Menten constant for CO2 (J mol-1)
          summa_parameter_ident = 58
          
      else if (name == 'ko_Ha_') then			        !!activation energy for the Michaelis-Menten constant for O2 (J mol-1)
          summa_parameter_ident = 59
          
      else if (name == 'vcmax25_canopyTop_') then		!!potential carboxylation rate at 25 degrees C at the canopy top (umol co2 m-2 s-1)
          summa_parameter_ident = 60
          
       else if (name == 'vcmax_qFac_') then		        !!factor in the q10 function defining temperature controls on vcmax (-)
          summa_parameter_ident = 61
          
      else if (name == 'vcmax_Ha_') then			    !!activation energy in the vcmax function (J mol-1)
          summa_parameter_ident = 62
          
      else if (name == 'vcmax_Hd_') then		        !!deactivation energy in the vcmax function (J mol-1)
          summa_parameter_ident = 63
          
      else if (name == 'vcmax_Sv_') then		        !!entropy term in the vcmax function (J mol-1 K-1)
          summa_parameter_ident = 64
          
      else if (name == 'vcmax_Kn_') then		       !!foliage nitrogen decay coefficient (-)
          summa_parameter_ident = 65
          
       
      else if (name == 'jmax25_scale_') then		   !!scaling factor to relate jmax25 to vcmax25 (-)
          summa_parameter_ident = 66
          
      else if (name == 'jmax_Ha_') then			       !!activation energy in the jmax function (J mol-1)
          summa_parameter_ident = 67
          
      else if (name == 'jmax_Hd_') then		           !!deactivation energy in the jmax function (J mol-1)
          summa_parameter_ident = 68
		  
	   else if (name == 'jmax_Sv_') then		       !!entropy term in the jmax function (J mol-1 K-1)
          summa_parameter_ident = 69
          
      else if (name == 'fractionJ_') then		       !!fraction of light lost by other than the chloroplast lamellae (-)
          summa_parameter_ident = 70
          
      else if (name == 'quantamYield_') then		   !!quantam yield (mol e mol-1 q)
          summa_parameter_ident = 71
	 else if (name == 'vpScaleFactor_') then		   !!vapor pressure scaling factor in stomatal conductance function (Pa)
          summa_parameter_ident = 72
          
      else if (name == 'cond2photo_slope_') then	   !!slope of conductance-photosynthesis relationship (-)
          summa_parameter_ident = 73
		  
		  
          
      else if (name == 'minStomatalConductance_') then	!!minimum stomatal conductance (umol H2O m-2 s-1)
          summa_parameter_ident = 74
          
      else if (name == 'winterSAI_') then		        !!stem area index prior to the start of the growing season (m2 m-2)
          summa_parameter_ident = 75
		  
	 else if (name == 'summerLAI_') then			    !!maximum leaf area index at the peak of the growing season (m2 m-2)
          summa_parameter_ident = 76
		  
      else if (name == 'rootScaleFactor1_') then		!!1st scaling factor (a) in Y = 1 - 0.5*( exp(-aZ) + exp(-bZ) )(m-1)
          summa_parameter_ident = 77
		
	  else if (name == 'rootScaleFactor2_') then		!!2nd scaling factor (b) in Y = 1 - 0.5*( exp(-aZ) + exp(-bZ) ) (m-1)
          summa_parameter_ident = 78
		  
	   else if (name == 'rootingDepth_') then			!!rooting depth (m)
          summa_parameter_ident = 79
		  
	  else if (name == 'rootDistExp_') then				!!exponent for the vertical distribution of root density (-)
          summa_parameter_ident = 80
		  
	   else if (name == 'plantWiltPsi_') then			!!matric head at wilting point (m)
          summa_parameter_ident = 81

	  !------------------------
	  else if (name == 'soilStressParam_') then			!!parameter in the exponential soil stress function (-)
          summa_parameter_ident = 82
		  
	  else if (name == 'critSoilWilting_') then			!!critical vol. liq. water content when plants are wilting (-)
          summa_parameter_ident = 83
	  
	  else if (name == 'critSoilTranspire_') then		!!critical vol. liq. water content when transpiration is limited (-)
          summa_parameter_ident = 84
	  else if (name == 'critAquiferTranspire_') then	!!critical aquifer storage value when transpiration is limited (m)
          summa_parameter_ident = 85
	  else if (name == 'minStomatalResistance_') then	!!minimum stomatal resistance (s m-1)
          summa_parameter_ident = 86
	   else if (name == 'leafDimension_') then			!!characteristic leaf dimension (m)
          summa_parameter_ident = 87
	  else if (name == 'heightCanopyTop_') then		    !!height of top of the vegetation canopy above ground surface (m)
          summa_parameter_ident = 88
      else if (name == 'heightCanopyBottom_') then		!!height of bottom of the vegetation canopy above ground surface (m)
          summa_parameter_ident = 89 
	  else if (name == 'specificHeatVeg_') then	        !!specific heat of vegetation (J kg-1 K-1)			            
          summa_parameter_ident = 90
	  else if (name == 'maxMassVegetation_') then	    !!maximum mass of vegetation (full foliage) (kg m-2)			            
          summa_parameter_ident = 91
	  else if (name == 'throughfallScaleSnow_') then    !!scaling factor for throughfall (snow) (-)				            
          summa_parameter_ident = 92
	  else if (name == 'throughfallScaleRain_') then    !!scaling factor for throughfall (rain) (-)				            
          summa_parameter_ident = 93
	  else if (name == 'refInterceptCapSnow_') then	    !!reference canopy interception capacity per unit leaf area (snow) (kg m-2)			            
          summa_parameter_ident = 94
	  else if (name == 'refInterceptCapRain_') then		!!canopy interception capacity per unit leaf area (rain) (kg m-2)		            
          summa_parameter_ident = 95
	  else if (name == 'snowUnloadingCoeff_') then	    !!time constant for unloading of snow from the forest canopy (s-1)			            
          summa_parameter_ident = 96
	  else if (name == 'canopyDrainageCoeff_') then	    !!time constant for drainage of liquid water from the forest canopy (s-1)			            
          summa_parameter_ident = 97
	  else if (name == 'ratioDrip2Unloading_') then	    !!ratio of canopy drip to unloading of snow from the forest canopy (-)			            
          summa_parameter_ident = 98
	  else if (name == 'canopyWettingFactor_') then	    !!maximum wetted fraction of the canopy (-)			            
          summa_parameter_ident = 99
	  else if (name == 'canopyWettingExp_') then	    !!exponent in canopy wetting function (-)			            
          summa_parameter_ident = 100
	  else if (name == 'soil_dens_intr_') then	        !!intrinsic soil density (kg m-3)			            
          summa_parameter_ident = 101
	  else if (name == 'thCond_soil_') then	            !!thermal conductivity of soil (includes quartz and other minerals) (W m-1 K-1)			            
          summa_parameter_ident = 102
	  else if (name == 'frac_sand_') then	            !!fraction of sand (-)			            
          summa_parameter_ident = 103
	  else if (name == 'frac_silt_') then	            !!fraction of silt (-)			            
          summa_parameter_ident = 104
	  else if (name == 'frac_clay_') then	            !!fraction of clay (-)			            
          summa_parameter_ident = 105
	  else if (name == 'fieldCapacity_') then	        !!soil field capacity (vol liq water content when baseflow begins) (-)			            
          summa_parameter_ident = 106
	  else if (name == 'wettingFrontSuction_') then	    !!Green-Ampt wetting front suction (-)			            
          summa_parameter_ident = 107
	  else if (name == 'theta_mp_') then	            !!volumetric liquid water content when macropore flow begins (-)			            
          summa_parameter_ident = 108
	  else if (name == 'theta_sat_') then	            !!soil porosity (-)			            
          summa_parameter_ident = 109
	  else if (name == 'theta_res_') then	            !!volumetric residual water content (-)			            
          summa_parameter_ident = 110
	  else if (name == 'vGn_alpha_') then			    !!van Genuchten "alpha" parameter (m s-1)	            
          summa_parameter_ident = 111
	  else if (name == 'vGn_n_') then	                !!van Genuchten "n" parameter (-)			            
          summa_parameter_ident = 112                   
	  else if (name == 'mpExp_') then				    !!empirical exponent in macropore flow equation (-)      
          summa_parameter_ident = 113
	  else if (name == 'k_soil_') then	                !!saturated hydraulic conductivity (m s-1)			            
          summa_parameter_ident = 114
	  else if (name == 'k_macropore_') then	            !!saturated hydraulic conductivity for macropores (m s-1)			            
          summa_parameter_ident = 115
	  else if (name == 'kAnisotropic_') then	        !!anisotropy factor for lateral hydraulic conductivity (-)			            
          summa_parameter_ident = 116
	  else if (name == 'zScale_TOPMODEL_') then	        !!TOPMODEL scaling factor used in lower boundary condition for soil (m)			            
          summa_parameter_ident = 117
	  else if (name == 'compactedDepth_') then	        !!depth where k_soil reaches the compacted value given by CH78 (m)			            
          summa_parameter_ident = 118
	  else if (name == 'aquiferBaseflowRate_') then		!!baseflow rate when aquifer storage = aquiferScaleFactor (m s-1)		            
          summa_parameter_ident = 119
	  else if (name == 'aquiferScaleFactor_') then		!!scaling factor for aquifer storage in the big bucket (m)		            
          summa_parameter_ident = 120
	  else if (name == 'aquiferBaseflowExp_') then	    !!baseflow exponent (-)			            
          summa_parameter_ident = 121
	  else if (name == 'qSurfScale_') then	            !!scaling factor in the surface runoff parameterization (-)			            
          summa_parameter_ident = 122
	  else if (name == 'specificYield_') then	        !!specific yield (-)			            
          summa_parameter_ident = 123
	  else if (name == 'specificStorage_') then		    !!specific storage coefficient (m-1)		            
          summa_parameter_ident = 124
	  else if (name == 'f_impede_') then			    !!ice impedence factor (-)	            
          summa_parameter_ident = 125
	  else if (name == 'soilIceScale_') then	        !!scaling factor for depth of soil ice, used to get frozen fraction (m)			            
          summa_parameter_ident = 126
	  else if (name == 'soilIceCV_') then	            !!CV of depth of soil ice, used to get frozen fraction (-)			            
          summa_parameter_ident = 127
	  else if (name == 'minwind_') then	                !!minimum wind speed (m s-1)			            
          summa_parameter_ident = 128
	  else if (name == 'minstep_') then		            	            
          summa_parameter_ident = 129
	  else if (name == 'maxstep_') then				            
          summa_parameter_ident = 130
	  else if (name == 'wimplicit_') then				            
          summa_parameter_ident = 131
	  else if (name == 'maxiter_') then				            
          summa_parameter_ident = 132
	  else if (name == 'relConvTol_liquid_') then				            
          summa_parameter_ident = 133
	  else if (name == 'absConvTol_liquid_') then				            
          summa_parameter_ident = 134
	  else if (name == 'relConvTol_matric_') then				            
          summa_parameter_ident = 135
	  else if (name == 'absConvTol_matric_') then				            
          summa_parameter_ident = 136
	  else if (name == 'relConvTol_energy_') then				            
          summa_parameter_ident = 137
	  else if (name == 'absConvTol_energy_') then				            
          summa_parameter_ident = 138
	  else if (name == 'relConvTol_aquifr_') then				            
          summa_parameter_ident = 139
	  else if (name == 'absConvTol_aquifr_') then				            
          summa_parameter_ident = 140
	  else if (name == 'zmin_') then	              !!minimum layer depth (m)			            
          summa_parameter_ident = 141
	  else if (name == 'zmax_') then	              !!maximum layer depth (m)			            
          summa_parameter_ident = 142
	  else if (name == 'zminLayer1_') then		      !!minimum layer depth for the 1st (top) layer (m)		            
          summa_parameter_ident = 143
	  else if (name == 'zminLayer2_') then	          !!minimum layer depth for the 2nd layer (m)			            
          summa_parameter_ident = 144
	  else if (name == 'zminLayer3_') then	          !!minimum layer depth for the 3rd layer (m)				            
          summa_parameter_ident = 145
	  else if (name == 'zminLayer4_') then	          !!minimum layer depth for the 4th layer (m)			            
          summa_parameter_ident = 146
	  else if (name == 'zminLayer5_') then	          !!minimum layer depth for the 5th layer (m)				            
          summa_parameter_ident = 147
	  else if (name == 'zmaxLayer1_lower_') then      !!maximum layer depth for the 1st (top) layer when only 1 layer (m)				            
          summa_parameter_ident = 148
	  else if (name == 'zmaxLayer2_lower_') then      !!maximum layer depth for the 2nd (top) layer when only 1 layer (m)				            
          summa_parameter_ident = 149
	  else if (name == 'zmaxLayer3_lower_') then      !!maximum layer depth for the 3rd (top) layer when only 1 layer (m)				            
          summa_parameter_ident = 150
	  else if (name == 'zmaxLayer4_lower_') then      !!maximum layer depth for the 4th (top) layer when only 1 layer (m)				            
          summa_parameter_ident = 151
	  else if (name == 'zmaxLayer1_upper_') then      !!maximum layer depth for the 1st (top) layer when > 1 laye (m)				            
          summa_parameter_ident = 152
	  else if (name == 'zmaxLayer2_upper_') then      !!maximum layer depth for the 2nd (top) layer when > 2 laye (m)				            
          summa_parameter_ident = 153
	  else if (name == 'zmaxLayer3_upper_') then      !!maximum layer depth for the 3rd (top) layer when > 3 laye (m)				            
          summa_parameter_ident = 154
	  else if (name == 'zmaxLayer4_upper_') then      !!maximum layer depth for the 4th (top) layer when > 4 laye (m)				            
          summa_parameter_ident = 155
	  else if (name == 'minTempUnloading_') then		    !!min temp for unloading in windySnow (K)		            
          summa_parameter_ident = 156
	  else if (name == 'minWindUnloading_') then	        !!min wind speed for unloading in windySnow (m s-1)			            
          summa_parameter_ident = 157
	  else if (name == 'rateTempUnloading_') then		    !!how quickly to unload due to temperature (K s)		            
          summa_parameter_ident = 158
	  else if (name == 'rateWindUnloading_') then		    !!how quickly to unload due to wind (m)		            
          summa_parameter_ident = 159
		!End of local summa parameters
        !!The following parameters are not used by summa. They will be adapted or modified for use with OPENWQ
	  else if (name == 'upper2deepx_') then				            
          summa_parameter_ident = 160
	  else if (name == 'tcfriverx_') then				            
          summa_parameter_ident = 161
	  else if (name == 'scfriverx_') then				            
          summa_parameter_ident = 162
	  else if (name == 'ccfriverx_') then				            
          summa_parameter_ident = 163
	  else if (name == 'lcfriverx_') then				            
          summa_parameter_ident = 164
	  else if (name == 'tcflakex_') then				            
          summa_parameter_ident = 165
	  else if (name == 'scflakex_') then				            
          summa_parameter_ident = 166
	  else if (name == 'ccflakex_') then				            
          summa_parameter_ident = 167
	  else if (name == 'lcflakex_') then				            
          summa_parameter_ident = 168
	  else if (name == 'stbcorr1x_') then				            
          summa_parameter_ident = 169
	  else if (name == 'stbcorr2x_') then				            
          summa_parameter_ident = 170
	  else if (name == 'licewmex_') then				            
          summa_parameter_ident = 171
	  else if (name == 'licetfx_') then				            
          summa_parameter_ident = 172
	  else if (name == 'licesndensx_') then				            
          summa_parameter_ident = 173
	  else if (name == 'licekikax_') then				            
          summa_parameter_ident = 174
	  else if (name == 'licekexpx_') then				            
          summa_parameter_ident = 175
	  else if (name == 'licetmeltx_') then				            
          summa_parameter_ident = 176
	  else if (name == 'licewcorrx_') then				            
          summa_parameter_ident = 177
	  else if (name == 'ricewmex_') then				            
          summa_parameter_ident = 178
	  else if (name == 'ricetfx_') then				            
          summa_parameter_ident = 179
	  else if (name == 'ricesndensx_') then				            
          summa_parameter_ident = 180
	  else if (name == 'ricekikax_') then				            
          summa_parameter_ident = 181
	  else if (name == 'ricekexpx_') then				            
          summa_parameter_ident = 182
	  else if (name == 'ricetmeltx_') then				            
          summa_parameter_ident = 183
	  else if (name == 'pcelevaddx_') then				            
          summa_parameter_ident = 184
	  else if (name == 'bfroznsoilx_') then				            
          summa_parameter_ident = 185
	  else if (name == 'stbcorr3x_') then				            
          summa_parameter_ident = 186
	  else if (name == 'depthrelx_') then				            
          summa_parameter_ident = 187
	  else if (name == 'iniT2x_') then				            
          summa_parameter_ident = 188
	  else if (name == 'litterdaysx_') then				            
          summa_parameter_ident = 189
	  else if (name == 'innerfiltx_') then
		  summa_parameter_ident = 190
	  else if (name == 'drydepppx_') then				    !!drydeppp (-) dry atmospheric deposition parameter for total phosphorus (landuse type)
          summa_parameter_ident = 191
		  
	  !Basin parameters used by SUMMA
	  else if (name == 'basin__aquiferHydCond_') then       !!hydraulic conductivity of the aquifer (m s-1)				    
          summa_parameter_ident = 192

      else if (name == 'routingGammaShape_') then	        !!shape parameter in Gamma distribution used for sub-grid routing (-)			    
          summa_parameter_ident = 193

      else if (name == 'routingGammaScale_') then	        !!scale parameter in Gamma distribution used for sub-grid routing (s)			    
          summa_parameter_ident = 194
		  
	  else if (name == 'basin__aquiferScaleFactor_') then	!!scaling factor for aquifer storage in the big bucket (m)			    
          summa_parameter_ident = 195
	  else if (name == 'basin__aquiferBaseflowExp_') then	!!baseflow exponent for the big bucket (-)			    
          summa_parameter_ident = 196

          !Sundials parameters
      else if (name == 'relTolTempCas_') then				            
          summa_parameter_ident = 197
          !write(*,*)"This is not a calibration parameter. The hard-coded default parameter value is used."
	  else if (name == 'absTolTempCas_') then				            
          summa_parameter_ident = 198
          !write(*,*)"This is not a calibration parameter. The hard-coded default parameter value is used."
	  else if (name == 'relTolTempVeg_') then				            
          summa_parameter_ident = 199
          !write(*,*)"This is not a calibration parameter. The hard-coded default parameter value is used."
	  else if (name == 'absTolTempVeg_') then				            
          summa_parameter_ident = 200
          !write(*,*)"This is not a calibration parameter. The hard-coded default parameter value is used."
	  else if (name == 'relTolWatVeg_') then				            
          summa_parameter_ident = 201
          !write(*,*)"This is not a calibration parameter. The hard-coded default parameter value is used."
	  else if (name == 'absTolWatVeg_') then				            
          summa_parameter_ident = 202
          !write(*,*)"This is not a calibration parameter. The hard-coded default parameter value is used."
	  else if (name == 'relTolTempSoilSnow_') then				            
          summa_parameter_ident = 203
          !write(*,*)"This is not a calibration parameter. The hard-coded default parameter value is used."
	  else if (name == 'absTolTempSoilSnow_') then				            
          summa_parameter_ident = 204
          !write(*,*)"This is not a calibration parameter. The hard-coded default parameter value is used."
	  else if (name == 'relTolWatSnow_') then				            
          summa_parameter_ident = 205
          !write(*,*)"This is not a calibration parameter. The hard-coded default parameter value is used."
	  else if (name == 'absTolWatSnow_') then				            
          summa_parameter_ident = 206
          !write(*,*)"This is not a calibration parameter. The hard-coded default parameter value is used."
	  else if (name == 'relTolMatric_') then				            
          summa_parameter_ident = 207
          !write(*,*)"This is not a calibration parameter. The hard-coded default parameter value is used."
	  else if (name == 'absTolMatric_') then
		  summa_parameter_ident = 208
          !write(*,*)"This is not a calibration parameter. The hard-coded default parameter value is used."
	  else if (name == 'relTolAquifr_') then
          summa_parameter_ident = 209
          !write(*,*)"This is not a calibration parameter. The hard-coded default parameter value is used."
      else if (name == 'absTolAquifr_') then
          summa_parameter_ident = 210
          !write(*,*)"This is not a calibration parameter. The hard-coded default parameter value is used." 

	  else
		  write(*,'(a19,2x,a6,2x,a17)')'The parameter name:',name,'is not recognized.'
          write(*,*)'Parameter ignored'
          

      endif

      return
      
      end
