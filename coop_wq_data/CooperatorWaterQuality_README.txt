Description of CooperatorWaterQuality.csv

[Code to re-create the file from original data files is in combine_cooperator_WQ.R]

Contains data from FWC, FDACS, and Frazier on water quality in the Cedar Key area.
Columns:
  Year
  Month
  Day
  Lat 		in decimal degrees
  Lon 		in decimal degrees
  Depth 	bottom depth, in m
  Bottom_temp 	bottom temperature, Celcius
  Bottom_sal  	bottom salinity
  Bottom_DO	bottom dissolved oxygen, mg/L
  Surface_temp	surface temperature, Celcius
  Surface_sal	surface salinity
  Surface_DO	surface dissolved oxygen, mg/L
  pH
  Data_source	source of data (FWC, FDACS, Frazier)


Notes:
  - missing values indicated by 'NA'
  - FWC data: multiple measurements were taken per gps point, deepest measurements were taken to be bottom and shallowest taken as surface
  - Frazier data: each measurement is associated with a Depth, it was assumed this was bottom depth

