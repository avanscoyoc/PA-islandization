[![DOI](https://zenodo.org/badge/294547191.svg)](https://zenodo.org/badge/latestdoi/294547191) 
[![CC BY-NC-SA 4.0][cc-by-nc-sa-shield]][cc-by-nc-sa]
![VERSION](https://badgen.net/static/Version/1.0.0-beta.1/orange)

## PA_Boundary - detect edges of PAs using remote sensing 
- Idea concieved by Wenjing Xu and Amy Van Scoyoc
- This README file serves as a technical report for each coding step
- Created August 2020
- Last updated June 2023
- This work is licensed under a
[Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License][cc-by-nc-sa].
_____________

## **Part 1**
### **`1_filter_clean_WDPA_polygons.R`**

*Input files:*
- 1.1 `WDPA_Jun2021_Public_csv.csv` _World database of protected areas, non-spatial information (September 2020)_
- 1.2 `WDPA_Jun2021_Public_shp.shp` _World database of protected areas, spatial information (September 2020)_
- 1.3 `Ecoregions2017.shp` _Biomes of the world, spatial information_
- 1.4 `gdal_polygonizeR.R` _Function for fast conversion of raster to polygon_

*Method:*
- We followed the protocol of _Jones et al. (2018) Science, 360(6390), 788-791_.
- We removed points, marine areas, areas less than 200km^2. 
- We joined the spatial geometries to the filtered protected area IDs. 
- We removed the narrowest protected areas (by removing protected areas in the top quartile of the perimeter-area ratio). 
- We filled holes inside of protected areas that were smaller than three 500m pixels (by rasterizing polygons, filling protected area rasters using a 3-pixel focal window, transforming the rasters back to polygons using the `gdal_polygonizeR.R` function, then shrinking protected area boundaries back to their original dimensions using a buffer). This step removed the interior protected area boundaries, resulting in the outermost protected area footprints [`Global_wdpa_footprint_June2021.shp`]. 
- We assigned biomes to each protected area; when multiple biomes were present we retained the biome label with the largest area [`Global_wdpa_wInfo_June2021.shp`].   

*Output files:*
- 1.1 `Global_wdpa_footprint_June2021.shp` _Filtered protected areas, spatial geometries only_
- 1.2 `Global_wdpa_wInfo_June2021.shp` _Filtered protected areas, spatial dataset_

_____________

## **Part 2**
### **`2_create_boundary_transects.R`**

*Input files:*
- 2.1 `Global_wdpa_footprint_June2021.shp` _Filtered protected areas, spatial geometries only_
- 2.2 `Global_wdpa_wInfo_June2021.shp` _Filtered protected areas, spatial dataset_
- 2.3 `evenspace.R` - _Function to create points along lines at a set distance_
- 2.4 `transect.R` - _Function to create transects perpendicular to lines_

*Method:*
- We created boundary points 500m apart, circumscribing each protected area frontier using the `evenspace.R` function. 
- We created transects perpendicular to the boundary at each boundary point using the `transect.R` function (the transects were 5km on either side of the boundary, with 2 interior points and 2 exterior points 2.5km apart). 
- We spatially joined protected area attribute information to each transect point [`GLOBAL_ori_transects_w_info.csv`].  
- We summarized the number of transects per protected area [`GLOBAL_ori_transects_count.csv`]. 
- We created a 5.5km (5km + 1 pixel size) buffer inside each boundary to filter inner transect points that crossed the inner buffer or were outside of the PA boundary. This problem was common to the narrow sections or sections with large curvature of protected areas. 
- We removed any transect point that crossed the boundary of adjacent protected areas. This problem was common when protected area boundaries overlapped or protected areas were within 5km of one another. 
- We removed any transect with a boundary point within 750m (1.5 500m MODIS pixels) of another protected area boundary. This ensured that gradient calculation on the boundary was not influenced by another protected area's boundary. 
- We exported [`gee_transects_ptXXX.shp`] to be filtered for water and to calculate spectral gradient in Google Earth Engine.

*Output files:*
- 2.1 `GLOBAL_ori_transects_w_info.csv` _Protected area transect and boundary points with attribute information, pre-filtering_
- 2.2 `GLOBAL_ori_transects_count.csv` _Summary of number of transects per protected area, raw counts_
- 2.3 `gee_transects_ptXXX.shp` _Filtered transect points, spatial geometries only_
- 2.4 `gee_transects_w_info.csv` _Filtered transect points, spatial and attribute information_

_____________

## **Part 3**
### **3_gee_water_mask_gradient_extract.js**

*Input files:*
- 3.1 Image Collection: `JRC/GSW1_2/GlobalSurfaceWater` #Maps global surface water location from 1984 to 2021 (_Pekel et al. (2016) Nature, 540, 418-422_). 
- 3.2 Image Collection: `MODIS/006/MOD09A1` #Surface spectral reflectance images, Terra MODIS bands 1-7, 500m resolution, corrected for atmospheric conditions
- 3.3 `gee_transects_ptXXX.shp` #Filtered transect points, spatial geometries only

*Method:*
- We used the maximum extent band of the global surface water image, which displays, for each pixel, where water has ever been detected between 1984 to 2021.
- We reduced all transect points to the maximum extent of global surface water and removed any transects where water was ever detected.
- We grouped the MODIS image collection by year, and reduced each year to a single, 7-band image by taking the median pixel values. This resulted in a total of 20 images corresponding to 20 years. 
- We calculated the gradient value each transect point, for the 20 images, by applying a spectral angle mapper with a square kernel size of 3 pixels. This computed the spectral gradient over all 7-bands by computing the per pixel difference between spectral erosion and dialation with the specified kernel. 
- We exported a table of all land-only transect points, with 20 years of associated spectral gradient values for each point [`transects_waterfiltered_gradient_ptXXX.csv`]. 

*Output files:*
- 3.1 `transects_waterfiltered_gradient_ptXXX.csv` _Land-only transects points with 20 years of associated spectral gradient values for each point_ 

_____________

## **Part 4**
### **4_gee_output_cleaning.R**

*Input files:*
- 4.1 `transects_waterfiltered_gradient_ptXXX.csv` _Land-only transects points with 20 years of associated spectral gradient values for each point_ 

*Method:*
- We removed the geometry columns from the Google Earth Engine output.
- We removed entire transects if they had any points filtered out for water (i.e., we removed transects with less than 5 points). 
- We combined all `transects_waterfiltered_gradient_ptXXX.csv` files into a single file.  

*Output files:*
- 4.1 `GLOBAL_sim_transects_landOnly.csv` _Land-only transects with 20 years of associated spectral gradient values for each point_

_____________

## **Part 5**
### **5_gee_output_analysis_1_edge_summary.R**

*Input files:*
- 5.1 `GLOBAL_sim_transects_landOnly.csv` _Land-only transects with 20 years of associated spectral gradient values for each point_
- 5.2 `GLOBAL_ori_transects_count.csv` _Summary of number of transects per protected area, raw counts_
- 5.3 `gee_transects_w_info.csv` _Filtered transect points, spatial and attribute information_

*Method:*
- We joined the attribute information from the protected area info file [`gee_transects_w_info.csv`] to the final filtered transects file [`GLOBAL_sim_transects_landOnly.csv`].
- We summarized the final number of transects remaining per protected area after water filtering. This allowed us to calculate the percent of transects evaluated relative to the total possible transects per protected area [from `GLOBAL_ori_transects_count.csv`]. 
- We calculated whether a transect had a habitat edge at the protected area boundary point for every year. To be classified as a boundary edge, the gradient values of any interior-exterior pair had to both be lower than the boundary point value.   
- We calculated the percent of transects classified as boundary edges for each protected area in 2020 (*Figure 5*). 
- We also calculated the percent of transects classified as boundary edges for each protected area in 2001.
- We exported a dataframe summarizing percent of transects evaluated and the edge levels for each protected area, `` for analysis
- We compared whether the percent of transects classified as boundary edges was significantly different from random for 2001 and 2020 (*figure S4*). To construct the null model, we reclassified habitat edges of all transects by comparing the gradient value of boundary points to random interior-exterior pairs within the same protected area – as opposed to interior-exterior pairs of the same transect. 

*Output files:*
- 5.1 `GLOBAL_edge_level_2020_2001_w_info.csv` - _Total transects, percent boundary edges of each protected area for 2001 and 2020_

_____________

## **Part 6**
### **6_gee_output_analysis_2_trend_estimate.R**

*Input files:*
- 6.1 `GLOBAL_sim_transects_landOnly.csv` _Land-only transects with 20 years of associated spectral gradient values for each point_
- 6.2 `GLOBAL_edge_level_2020_2001_w_info.csv` - _Total transects, percent boundary edges of each protected area for 2001 and 2020_
- 6.3 `Global_wdpa_wInfo_June2021.shp` - _Filtered protected areas, spatial dataset_

*Method:*
- We calculate a linear regression for each park to estimate a 20-year trend in its boundary gradient values. We determined significant PA trend estimates using a p-value < 0.05 (*Fig. 2*). 
- We merged the results of the linear regressions with the `GLOBAL_edge_level_2020_2001_w_info.csv'. 
- We exported the final results as `GLOBAL_edge_all_results.csv` and `GLOBAL_edge_all_results.shp`.

*Output files:*
- 6.1 `GLOBAL_edge_all_results.csv` - _Total transects, % boundary edges of each protected area for 2001 and 2020, 20 year trend in boundary edges with p-values_
- 6.2 `GLOBAL_edge_all_results.shp` - _Total transects, % boundary edges of each protected area for 2001 and 2020, 20 year trend in boundary edges with p-values_

_____________

## **Part 7**
### **7_MODIS_vs_LS_visualization.js**

*Input files:*
- 7.1 Image Collection: `MODIS/006/MOD09A1` _Surface spectral reflectance images, Terra MODIS bands 1-7, 500m resolution, corrected for atmospheric conditions_
- 7.2 Image Collection: `LANDSAT/LC08/C01/T1_TOA` _Surface spectral reflectance images, Landsat 8 bands 1-7, 30m resolution, corrected for atmospheric conditions_

*Method:*
- We created *Figure S2* by calculating the gradient value of a single protected area using Landsat and MODIS imagery. To do this, we applied a spectral angle mapper with a square kernel size of 3 pixels. This computed the spectral gradient over all 7-bands by computing the per pixel difference between spectral erosion and dialation with the specified kernel.

*Output files:*
- `Figure S2`

_____________

### **7_global_stats**

*Input files:*
- `GLOBAL_edge_all_results.csv` - _Total transects, % boundary edges of each protected area for 2001 and 2020, 20 year trend in boundary edges with p-values_

*Method:*
- We grouped protected area ecoregions into eight biomes (Tropical Forest, Temperate Forest, Boreal Forest, Grassland-Shrubland, Desert, Tundra, Mangroves, Rock & Ice).
- To calculate islandization at the global and biome levels, we summarized the percent of all transects (n = 1,516,412) showing habitat discontinuity for each of 20 years (*Figure 3A*). 
- We summarized significant protected area trend estimates by biome using p-value < 0.05 (*Figure 3B*).
- We extracted the coefficients for global and biome level protected area trends (*Table S1*).  

*Output files:*
- `Figure 3A`- Protected area islandization across the world’s biomes.
- `Figure 3B` - Protected area islandization across the world’s biomes.
- `Table S1` - Estimates of global protected area (PA) islandization by biome from 2001-2020. 

_____________

### **7_pa_stats**

*Input files:*
- `GLOBAL_edge_all_results.csv` - _Total transects, % boundary edges of each protected area for 2001 and 2020, 20 year trend in boundary edges with p-values_

*Method:*
- We appended country and biome to the edge trend dataset.
- We calculated descriptive results for number and median of transects, countries, biomes, and size of protected areas. 
- We examined whether islandization trends varied by IUCN protection category, biome, protected area size using ANOVA and linear regression. 
- We examined whether the percent of boundary edge in 2020 varied by IUCN protection category, biome, protected area size, and protected area establishment year using ANOVA and linear regression. 

*Output:*
- None. 

_____________
## **Part 8**
### **8_1_1_sensitivity_create_boundary_transects_5km**, **8_1_2_sensitivity_create_boundary_transects_15km**, **8_2_sensitivity_gee_output_cleaning**, **8_3_sensitivity_edge_summary**, **8_4_sensitivity_stats**

*Input files:*
- 8.1 `WDPA_Jun2021_Public_csv.csv` _World database of protected areas, non-spatial information (September 2020)_
- 8.2 `WDPA_Jun2021_Public_shp.shp` _World database of protected areas, spatial information (September 2020)_
- 8.3 `Ecoregions2017.shp` _Biomes of the world, spatial information_
- 8.4 `gdal_polygonizeR.R` _Function for fast conversion of raster to polygon_
- 8.5 `evenspace.R` - _Function to create points along lines at a set distance_
- 8.6 `transect.R` - _Function to create transects perpendicular to lines_

*Method:*
- We conducted sensitivity analysis by repeating the above analysis using 5 km transects and 15 km transects. 
- Transects of different lengths were generated using code `8_1_1` and `8_1_2`. The method is identical as `2_create_boundary_transects.R`. 
- `8_2` is identical as `4_gee_output_cleaning.R`
- `8_3` is identical as `5_gee_output_analysis_1_edge_summary.R`
- `8_4` summarizes transects and percent habitat discontinuity of different transect lengths per biome. 

_____________

[![CC BY-NC-SA 4.0][cc-by-nc-sa-image]][cc-by-nc-sa]

[cc-by-nc-sa]: http://creativecommons.org/licenses/by-nc-sa/4.0/
[cc-by-nc-sa-shield]: https://img.shields.io/badge/License-CC%20BY--NC--SA%204.0-lightgrey.svg
[cc-by-nc-sa-image]: https://licensebuttons.net/l/by-nc-sa/4.0/88x31.png
