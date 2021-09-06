- CONUS data via website
https://portal.edirepository.org/nis/mapbrowse?packageid=edi.481.5

- cleaning: 
  - trait data 
  - meta information


- Vieira et al. 2006 as a .rds 
- Harmonisation? User should decide?
- Only look on the genus-lvl

Structure:
- fork & clone stefandev
- getTraitData(parameter = c("Conus", "Vieira", "Both"), taxonLevel = "Genus"){}
