Elevation Data Compilation:

Sources (Date compiled into single production database: elevation_prod.csv):

-Epoch 3 (Jan 31 2019...database created...still need BTI4-1, BTI-6, and all NNI,LCI,LCN transects)
-Epoch 2 (data needs located)
-Epoch 1 (March 12, 2019)
	-data linked with transect data
	-reformatted to match elevation_prod.csv format
	-removed stations with no elevation  data
	-renamed stations
	-removed duplicate elevation data applied to multiple transect sampling events
	-notes from cleanup:  CKO3: no 2.5m tran_length, CRN1 data entered twice (effect oyster/elevation estimates derived from this data?), and deleted HBO1 due to inconsistent elevations across transect sampling events (no way to tell which is correct).  
-Preconstruction Benchmarks:  (Feb 12 2019)
	-convert coordinate system to UTM
	-convert elevation to meters
	-assign point names/reef elements 
	-applied date based on email receipt of revised survey
-Preconstruction Bathymetry Survey, Young:  convert coordinate system to UTM, convert elevation to meters, assign reef elements
-Postconstruction As-built Survey, Boone: (Feb 12 2019)
	-convert coordinate system to UTM in Arc
	-convert elevation to meters in Arc
	-Daniel Gore transects are not the same as UF transects
	-Arc sets elements with letters as 0 (ex element 9B), had to manually rename by point label
	-applied dates based on certification letters 
