//import 
var modis = ee.ImageCollection("MODIS/006/MOD09A1"),
    imageVisParam1 = {"opacity":1,"bands":["sur_refl_b01","sur_refl_b04","sur_refl_b03"],"min":442.32000000000005,"max":3154.08,"gamma":1},
    gsw = ee.Image("JRC/GSW1_2/GlobalSurfaceWater"),
    samples = ee.FeatureCollection("users/wenjingxu/PA_boundary/gee_transects_pt3");  // pt3 as an example. repeat for each file. 


//reduce points to water mask
var land_pts = gsw.reduceRegions({
  collection: samples,
  reducer: ee.Reducer.max(),
  scale: 30,//used 30m bc 500m misses many water pts
}).filter(ee.Filter.eq('max_extent', 0));

//print(samples);
//print(land_pts);

var years = ee.List.sequence(2001, 2020);

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

print(Gradient_by_Year);
// Map.addLayer(Gradient_by_Year, {}, "grandient");
// Map.addLayer(Gradient_by_Year.first());

// extract gradient value to land points 
var ft = ee.FeatureCollection(ee.List([])); // empty collection to fill 
var fill = function(img, ini) {
  var inift = ee.FeatureCollection(ini); // type cast
  var ft2 = img.reduceRegions(land_pts, ee.Reducer.first(), 500); //make the number bigger to avoid 0
  var date = img.get("year");
  var ft3 = ft2.map(function(f){return f.set("year", date)});
  return inift.merge(ft3);
};
var newft = ee.FeatureCollection(Gradient_by_Year.iterate(fill, ft)); //iterate over the image collection

//print(newft);
//Map.addLayer(newft, {color: 'FF0000'}, "extracted");

Export.table.toDrive({
  collection: newft,
  description:'GLOBAL_transects_LandOnly',
  fileFormat: 'CSV'
});
