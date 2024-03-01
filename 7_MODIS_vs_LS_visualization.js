var modis = ee.ImageCollection("MODIS/006/MOD09A1"),
    imageVisParam1 = {"opacity":1,"bands":["sur_refl_b01","sur_refl_b04","sur_refl_b03"],"min":442.32000000000005,"max":3154.08,"gamma":1},
    Phu_Khiew = 
    /* color: #bf04c2 */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[101.11026002461948, 16.66529056399136],
          [101.11026002461948, 15.891492179050038],
          [102.32699586446323, 15.891492179050038],
          [102.32699586446323, 16.66529056399136]]], null, false),
    landsat = ee.ImageCollection("LANDSAT/LC08/C01/T1_TOA");
	
	var years = ee.List.sequence(2020, 2020);

// Group by year, and then reduce within groups by max();
// the result is an ImageCollection with one image for each year.
var Gradient_by_Year = ee.ImageCollection.fromImages(
      years.map(function (y) {
        return modis.filter(ee.Filter.calendarRange(y, y, 'year'))
                    .select(['sur_refl_b01', 'sur_refl_b02', 'sur_refl_b03', 'sur_refl_b04',
                    'sur_refl_b05', 'sur_refl_b06', 'sur_refl_b07'])
                    .median()
                    .spectralGradient("sam",ee.Kernel.square(3, "pixels")).multiply(100).set('year', y);
}));

Map.addLayer(Gradient_by_Year.first(),  {}, "MODIS");

//demonstrate LS imagery
var Gradient_by_Year_LS = ee.ImageCollection.fromImages(
      years.map(function (y) {
        return landsat.filter(ee.Filter.calendarRange(y, y, 'year'))
                    .select(['B1', 'B2', 'B3', 'B4',
                    'B5', 'B6', 'B7'])
                    .median()
                    .spectralGradient("sam",ee.Kernel.square(3, "pixels")).multiply(100).set('year', y);
}));
Map.addLayer(Gradient_by_Year_LS.first(), {}, "LANDSAT");

// Export image
Export.image.toDrive({
  image: Gradient_by_Year.first(),
  region: Phu_Khiew,
  description: 'Phu_Khiew_gradient_MODIS',
  scale: 500,
  maxPixels: 10E11,
  fileFormat: 'GeoTIFF',
});

// Export image
Export.image.toDrive({
  image: Gradient_by_Year_LS.first(),
  region: Phu_Khiew,
  description: 'Phu_Khiew_gradient_LS',
  scale: 30,
  maxPixels: 10E11,
  fileFormat: 'GeoTIFF',
});