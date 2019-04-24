Spring 2019:
-Transect and quadrat reformatted and compiled completed by Erica/Katie. 
-Reformatting confirmed and spot-checked by Steve with datasheets.
-Erica provided basic Git setup guidance.

Summer 2019:
-Mel compiles all Epoch 1 and 2 datasheets.
-Steve provided station renaming guidance maps and spreadsheet based on GIS files. This was to ensure accurate spatial-temporal identification of data.
-Katie began to apply renaming conventions to transect data (priority in preparation for upcoming transect sampling).

Fall 2019:
-Transect power analysis completed by Bill.  Quadrat data now priority.
-Mel/Jennifer attempted to apply renaming conventions to quadrat data.

December 2019:
-Mel consults with Steve about issues with renaming effort.
-Steve looks into this, provides renaming guidance, notices transect station name change issues.   

March 13 2019:
-Steve, Mel, Jennifer meet
-Looks like some old station names were updated multiple times and thus incorrect.  The new station names appear correct, with the exception of Epoch 3 (new name) LCO2 and LCO4.  
-Jennifer is going to restore original station names for datasheet reference and make corrections.
-Steve will do one final comparison with raw data sheets to ensure no data has been effected by renaming effort.

Steve finalizing rename effort.
April 22, 2019:
-Housing production files on personal Git page until guidance provided on how to incorporate into project Git.
-Corrected remaining transect name issues in "transect_data_production2.csv"
-Created "quadrat_data_production.csv", matching transect_data_production as much as possible, building from "quadrat_combined_newstations.csv" file Jen provided
	-substation = quadrat # on a bar (synonomous with transect column).  Changed this column to "quadrat"  
	-It will take a massive effort to apply specific quadrat # to epoch 1 and 2 quad data.  Not necessary needed since this data not used for deriving density.  For now, desginated Epoch 1-2 quadrat# as -999.  Assigned quadrat # only to Epoch 3.
	-corrected LCI,LCN name issues
	-consistent -999/NA values for numeric/text columns, respectively
	-delete location column/apply letters to station name
	-reformat