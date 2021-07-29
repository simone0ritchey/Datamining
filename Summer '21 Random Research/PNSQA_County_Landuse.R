### Which land use variables explain the most difference between PNSQA counties? 
### Simone Ritchey

### Do First ###################################################################

# Load Packages
library(ggbiplot)
library(dplyr)

# Load Data
# wd for this file: "/Users/simone/Documents/UT/UrbanEco/Datamining/Summer '21 Random Research"
setwd("/Users/simone/Documents/UT/UrbanEco/Datamining/LandUse_RSQA_Data")
watershed_data <- read.csv("RSQA_Characteristics_Data_WatershedData.csv")

################################################################################

### PCA of land use grouped by county

# Loading watershed land use data
setwd("/Users/simone/Documents/UT/UrbanEco/Datamining/LandUse_RSQA_Data")
watershed_data <- read.csv("RSQA_Characteristics_Data_WatershedData.csv")

# Clean data for PCA

  # Extract PNSQA land use
  PNSQA_watershed_data <- filter(watershed_data, RSQA_Study = "PNSQA")

