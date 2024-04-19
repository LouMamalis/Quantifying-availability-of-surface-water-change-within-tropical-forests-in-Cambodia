# Quantifying-availability-of-surface-water-change-within-tropical-forests-in-Cambodia
This repository includes the scripts relevant to the research article titled 'Quantifying the availability of seasonal surface water and identifying the drivers of change within tropical forests in Cambodia' which is published in PLOS ONE. 

This article can be found here: ...DOI...

This README file outlines a summary of the different scripts included within this repository. 

We conducted much of the analysis for this reserach using Google Earth Engine and we are making the scripts available here in .txt format so that they are accessible to anyone.

**GEE_trapeangs classifier-cleaned.txt**: this script was used to create the surface water classifier. Here we conducted some pre-processsing of the satellite images and then we created the actual classifier which assesssed each pixel as either water or non-water. This was then used to map these changes over time. 

*Raw data files needed: Thaumatibis_gigantea.shp, npa_en_16022021-polygon.shp, CWS_trapeangs_coords.csv, KPWS_trapeang_coords.csv, allpoints.shp*

**GEE_surface water transition mapping-cleaned.txt**: this script creates the surface water transition maps used for the analysis. For this research we wanted to evaluate the change in surface water availability across the northeast of Cambodia. To do this we used LandSAt 7 images between 2000 and 2020 and looked at how the surface water changed over time. 

*Raw data files needed: Thaumatibis_gigantea.shp*

**GEE_surface water nest analysis-cleaned.txt**: this script computes the distance to surface water of the giant ibis nest points and the randomly generated non-nest points. Within this research we were assessing the impact of changes to surface water availability on the giant ibis. To do this we looked at the importance of surface water to the giant ibis nest locations. We then compared this relationship to surface water with the relationship between randomly generated non-nest points. 

For the second stage of our analysis we used R and we have included the scripts of the analysis we completed in the below scripts:

**Surface Water&Precipitation-cleaned.R**: this script explores and creates plots of the change in surface water availability across the study site. It also looks at the changes in precipitation over time. It uses exports of the surface water availability from the Google Earth Engine analysis. We also conducted analysis of correlations and linear regressions of the surface water and preciptation data. 

*Raw data files needed: exportdsw_sum.csv, exportsw_sum.csv and year_precip.csv*

INLA Modelling scripts
**INLA Model-data-prep-cleaned.R**: this script contains the code that we used to prepare the covariate data to be used in the INLA modelling. This included processes such as rasterising shapefiles and ensuring that the resolution of the data was uniform.

*Raw data files needed: transition_map_500R.Rdata, initial_sw_500R.Rdata, elevation_map_500.tif, PAs_latlon.shp, osm_road_2020.shp, ELC_latlon.shp*

**INLA Model1-cleaned.R**: actual code that runs the Integrated Nested LaPlace Model 1 looks at interactions between the selected covariates and any general changes of wetting or drying of land within study site and is a gaussian model type.

*Raw data files needed covariate_stack_new.Rdata*

**INLA Model2-drier-cleaned.R**: actual code that runs the Integrated Nested LaPlace Model 2 looks at interactions between the selected covariates and areas of drying land within the study site and is a binomial model type.

*Raw data files needed covariate_stack_bino.Rdata*

**INLA Model3-drier-cleaned**: actual code that runs the Integrated Nested LaPlace Model 3 looks at interactions between the selected covariates and areas of flooding land within the study site and is a binomial model type.

*Raw data files needed covariate_stack_bino.Rdata*

Suitable giant ibis habitat loss scripts
**Gibis Habitat-Sensitivity Checks-cleaned.R**: this script was used to complete a sensitivity check on the chosen percentile threshold for the distance to surface water. Here we looked at the distance to surface water of nest and non-nest points for the 65th and 85th percentile to compare with the chosen 75th percentile. 

*Raw data files needed: FinalResults_sensitivity.Rdata*

**Gibis Habitat-Analysis-cleaned.R**: this script uses data from GEE_surface water nest analysis-cleaned.txt GEE script which calculates the distance of nest and non-nest points to surface water within the study region. These results are then used in this script to look at the area within 75th percentile values of nest and non-nest points to surface water. 

*Raw data files needed: all_dists_im_1000.tif*

**gi_nests_WaterDist.ipynb**: this script computes, summarises and presents the distance to surface water of nest and non-nest points. It includes calculation of the 75th percentile distance of nest and non-nest points to surface water. 

*Raw data files needed: nests_dists.csv and random_dists.csv*

**Gibis Habitat-Plot-cleaned.R**: this script follows on from gi_nests_WaterDist.ipynb and creates a plot to show the loss of suitable habitat for the giant ibis over time within the study site. 

*Raw data files needed: AllDists_merged.Rdata, FinalResults.Rdata and AllDists.Rdata*




