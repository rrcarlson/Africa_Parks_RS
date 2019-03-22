### Bazaruto project, ESS241, MARCH 22, 2019
### Rachel Carlson

# Load relevant libraries
library(raster) # package for raster manipulation
library(sf) # package for managing shapefiles as dataframes
library(dplyr) # package for data wrangling
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting vector data
library(units)
library(purrr)
library(rlist)
library(lubridate) # package for date-time
library(rlist)
library(rasterVis) # package for plotting rasters
library(classInt)
library(cmvnorm) # package for raster stats
library(Kendall)

# Create shape for mask from bathymetry <= 200 m polygon
setwd("/Users/rachelcarlson/Documents/github/Africa_Parks_RS/")
bathy <- raster("data/Bazaruto_data/bathymetry_GEBCO.tif")
bathy[bathy>0] <- NA
bathy[bathy<(-100)] <- NA
plot(bathy)
CRS <- crs(bathy)

# Convert bathymetry raster to polygon (area of low mixing, <200m).
# Doing this in R messed up the projection, strangely, so I kicked it over to Arc to quickly convert to a polygon.

writeRaster(bathy, "bathy_100m_poly.tif")
st_write(bazaruto, "bazaruto.shp")
bathy_mask <- st_read("data/Bazaruto_data/bathy_simple/bathy_200m_simple.shp")
plot(bathy_mask['geometry'], add = TRUE)

# Read in Bazaruto shapefile
all_mpas <- st_read("data/Bazaruto_data/MPA_shape/WDPA_Feb2019-shapefile-polygons.shp")
bazaruto <- all_mpas %>% filter(NAME=="Bazaruto")

setwd("/Users/rachelcarlson/Documents/github/Africa_Parks_RS/data/667_8day_ALL/")
fp <- getwd()

## Find variance of 8-day means and max Rrs per 8-day means
# Create empty lists to store variance and max Rrs667 values
var_annual <- list()
max_max_annual <- list()
mean_max_annual <- list()
mean_mean_annual <- list()
raster_stack_list <- list()

for (i in 1:14){
  patt_string <- as.character(i+2004)
  # Read filepaths for all 8-day rasters per year
  files_extract <- list.files(path = fp, pattern = patt_string)
  # Create 46-layer raster stack per year (one layer per 8 days). Projection is already aligned with mask: EPSG 4326.
  raster_stack <- raster::stack(files_extract)
  # Clip rasters within stack to bathy mask
  raster_stack_bazaruto <- raster_stack %>% raster::crop(bathy_mask) %>% raster::mask(bathy_mask)
  # Create mask for no data due to clouds, other missing data
  raster_stack_bazaruto[raster_stack_bazaruto <= 0] <- NA
  # Calculate mean pixel value within mask for each 8-day raster. Find annual variance of mean pixel value.
  variance <- var(cellStats(raster_stack_bazaruto, stat='mean'))
  raster_stack_df <- data.frame(raster_stack_bazaruto.mean=cellStats(raster_stack_bazaruto, "mean"))
  # Calculate max pixel value within mask for each 8-day raster. Find max of all pixel values.
  max <- max(cellStats(raster_stack_bazaruto, stat='max'))
  mean <- mean(cellStats(raster_stack_bazaruto, stat='max'))
  mean_mean <- mean(cellStats(raster_stack_bazaruto, stat='mean'))
  # Write variance and max 8-day Rrs667 to lists
  var_annual[i] <- variance
  max_max_annual[i] <- max
  mean_max_annual[i] <- mean
  mean_mean_annual[i] <- mean_mean
  raster_stack_list[i] <- raster_stack_df
}

# Couldn't figure out how to extract a list of (unequal length) lists to a dataframe. So here's some ugly hard-coding.
# Compile rasters into dataframe of 46 8-week rasters x 14 years
df1 <- data.frame("turb" = raster_stack_list[[1]], year = c("2005"))
df2 <- data.frame("turb" = raster_stack_list[[2]], year = c("2006"))
df3 <- data.frame("turb" = raster_stack_list[[3]], year = c("2007"))
df4 <- data.frame("turb" = raster_stack_list[[4]], year = c("2008"))
df5 <- data.frame("turb" = raster_stack_list[[5]], year = c("2009"))
df6 <- data.frame("turb" = raster_stack_list[[6]], year = c("2010"))
df7 <- data.frame("turb" = raster_stack_list[[7]], year = c("2011"))
df8 <- data.frame("turb" = raster_stack_list[[8]], year = c("2012"))
df9 <- data.frame("turb" = raster_stack_list[[9]], year = c("2013"))
df10 <- data.frame("turb" = raster_stack_list[[10]], year = c("2014"))
df11 <- data.frame("turb" = raster_stack_list[[11]], year = c("2015"))
df12 <- data.frame("turb" = raster_stack_list[[12]], year = c("2016"))
df13 <- data.frame("turb" = raster_stack_list[[13]], year = c("2017"))
df14 <- data.frame("turb" = raster_stack_list[[14]], year = c("2018"))

# More ugly hard-coding (how do you union 14 dataframes at once?)
master_df <- 
  dplyr::union(df1,df2) %>% 
  dplyr::union(df3) %>% 
  dplyr::union(df4) %>% 
  dplyr::union(df5) %>% 
  dplyr::union(df6) %>% 
  dplyr::union(df7) %>% 
  dplyr::union(df8) %>% 
  dplyr::union(df9) %>% 
  dplyr::union(df10) %>% 
  dplyr::union(df11) %>% 
  dplyr::union(df12) %>% 
  dplyr::union(df13) %>% 
  dplyr::union(df14)

# Convert lists to dataframe for easy data management
df_8day <- data.frame('year' = c(2005:2018), 'variance' = unlist(var_annual, use.names = FALSE), 'max' = unlist(max_max_annual, use.names = FALSE), 'mean' = unlist(mean_mean_annual, use.names = FALSE))
write.csv(df_8day,"/Users/rachelcarlson/Documents/github/Africa_Parks_RS/ResearchQ1.csv")

# Conduct Mann-Kendall to test for trends
df_8day_ts <- ts(df_8day[3])
Q1_MannKendall <- MannKendall(df_8day_ts)
Q1_MannKendall

## Find differences from monthly climatology: Feb
# Create empty list to store difference values per year
setwd("/Users/rachelcarlson/Documents/github/Africa_Parks_RS/data/monthly_clim/Feb/")
climato_diff_Feb <- list()

# Populate empty list for 15 years (2004-2018)
fp_Feb = getwd()
for (i in 1:14){
  files_extract <- list.files(path = fp_Feb, pattern = ".nc")
  # Create 15-layer raster stack per year (one layer per year, for May). Projection is already aligned with mask: EPSG 4326.
  raster_stack <- raster::stack(files_extract)
  # Clip rasters within stack (and load/clip May climatology raster) to bathy mask
  raster_stack_bazaruto <- raster_stack %>% raster::crop(bathy_mask) %>% raster::mask(bathy_mask)
  Feb_climato <- raster("climatology/A20030322018059.L3m_MC_RRS_Rrs_667_4km.nc") %>% raster::crop(bathy_mask) %>% raster::mask(bathy_mask)
  # Create mask for no data due to clouds, other missing data
  raster_stack_bazaruto[raster_stack_bazaruto <= 0] <- NA
  Feb_climato[Feb_climato <=0] <- NA
  # Calculate difference in pixel value within mask for annual May versus May climatology. Find mean of pixel diffs per raster.
  Feb_raster_diff <- raster_stack_bazaruto[[i]] - Feb_climato
  # Find mean diff value for the year
  Feb_diff_mean <- cellStats(Feb_raster_diff, stat='mean')
  # Write diffs to my list
  climato_diff_Feb[i] <- Feb_diff_mean
}

## Find differences from monthly climatology: Mar
# Create empty list to store difference values per year
setwd("/Users/rachelcarlson/Documents/github/Africa_Parks_RS/data/monthly_clim/March/")
climato_diff_Mar <- list()

# Populate empty list for 15 years (2004-2018)
fp_Mar = getwd()
for (i in 1:14){
  files_extract <- list.files(path = fp_Mar, pattern = ".nc")
  # Create 15-layer raster stack per year (one layer per year, for May). Projection is already aligned with mask: EPSG 4326.
  raster_stack <- raster::stack(files_extract)
  # Clip rasters within stack (and load/clip May climatology raster) to bathy mask
  raster_stack_bazaruto <- raster_stack %>% raster::crop(bathy_mask) %>% raster::mask(bathy_mask)
  Mar_climato <- raster("climatology/A20030602018090.L3m_MC_RRS_Rrs_667_4km.nc") %>% raster::crop(bathy_mask) %>% raster::mask(bathy_mask)
  # Create mask for no data due to clouds, other missing data
  raster_stack_bazaruto[raster_stack_bazaruto <= 0] <- NA
  Mar_climato[Mar_climato <=0] <- NA
  # Calculate difference in pixel value within mask for annual May versus May climatology. Find mean of pixel diffs per raster.
  Mar_raster_diff <- raster_stack_bazaruto[[i]] - Mar_climato
  # Find mean diff value for the year
  Mar_diff_mean <- cellStats(Mar_raster_diff, stat='mean')
  # Write diffs to my list
  climato_diff_Mar[i] <- Mar_diff_mean
}

## Find differences from monthly climatology: Apr
# Create empty list to store difference values per year
setwd("/Users/rachelcarlson/Documents/github/Africa_Parks_RS/data/monthly_clim/April/")
climato_diff_Apr <- list()

# Populate empty list for 15 years (2004-2018)
fp_Apr = getwd()
for (i in 1:14){
  files_extract <- list.files(path = fp_Apr, pattern = ".nc")
  # Create 15-layer raster stack per year (one layer per year, for May). Projection is already aligned with mask: EPSG 4326.
  raster_stack <- raster::stack(files_extract)
  # Clip rasters within stack (and load/clip May climatology raster) to bathy mask
  raster_stack_bazaruto <- raster_stack %>% raster::crop(bathy_mask) %>% raster::mask(bathy_mask)
  Apr_climato <- raster("climatology/A20030912018120.L3m_MC_RRS_Rrs_667_4km.nc") %>% raster::crop(bathy_mask) %>% raster::mask(bathy_mask)
  # Create mask for no data due to clouds, other missing data
  raster_stack_bazaruto[raster_stack_bazaruto <= 0] <- NA
  Apr_climato[Apr_climato <=0] <- NA
  # Calculate difference in pixel value within mask for annual May versus May climatology. Find mean of pixel diffs per raster.
  Apr_raster_diff <- raster_stack_bazaruto[[i]] - Apr_climato
  # Find mean diff value for the year
  Apr_diff_mean <- cellStats(Apr_raster_diff, stat='mean')
  # Write diffs to my list
  climato_diff_Apr[i] <- Apr_diff_mean
}

## Find differences from monthly climatology: May
# Create empty list to store difference values per year
setwd("/Users/rachelcarlson/Documents/github/Africa_Parks_RS/data/monthly_clim/May/")
climato_diff_May <- list()

# Populate empty list for 15 years (2004-2018)
fp_May = getwd()
for (i in 1:14){
  files_extract <- list.files(path = fp_May, pattern = ".nc")
  # Create 15-layer raster stack per year (one layer per year, for May). Projection is already aligned with mask: EPSG 4326.
  raster_stack <- raster::stack(files_extract)
  # Clip rasters within stack (and load/clip May climatology raster) to bathy mask
  raster_stack_bazaruto <- raster_stack %>% raster::crop(bathy_mask) %>% raster::mask(bathy_mask)
  May_climato <- raster("climatology/A20031212018151.L3m_MC_RRS_Rrs_667_4km.nc") %>% raster::crop(bathy_mask) %>% raster::mask(bathy_mask)
  # Create mask for no data due to clouds, other missing data
  raster_stack_bazaruto[raster_stack_bazaruto <= 0] <- NA
  May_climato[May_climato <=0] <- NA
  # Calculate difference in pixel value within mask for annual May versus May climatology. Find mean of pixel diffs per raster.
  May_raster_diff <- raster_stack_bazaruto[[i]] - May_climato
  # Find mean diff value for the year
  May_diff_mean <- cellStats(May_raster_diff, stat='mean')
  # Write diffs to my list
  climato_diff_May[i] <- May_diff_mean
}

df_climato_diffs <- data.frame('year' = c(2005:2018), 
                               'Feb_diffs' = unlist(climato_diff_Feb, use.names = FALSE), 
                               'Mar_diffs' = unlist(climato_diff_Mar, use.names = FALSE),
                               'Apr_diffs' = unlist(climato_diff_Apr, use.names = FALSE),
                               'May_diffs' = unlist(climato_diff_May, use.names = FALSE))
write.csv(df_climato_diffs,"/Users/rachelcarlson/Documents/github/Africa_Parks_RS/ResearchQ2.csv")

# Plots
# Fig 1
fig1 <- plot(bathy)

# Fig 2
fp_667_gen <- "A20051212005151.L3m_MO_RRS_Rrs_667_4km.nc"
fig2 <- raster(fp_667_gen) %>% raster::crop(bathy_mask) %>% raster::mask(bathy_mask)
plot(fig2)
plot(bathy_poly['geometry'], add = TRUE)

# Plot one year (example of variance within one year)
setwd("/Users/rachelcarlson/Documents/github/Africa_Parks_RS/data/667_8day_2018")
f_plot <- file.path("data/667_8day_2018/")
RS667_files_plot <- list.files(path = f_plot, pattern = ".nc")

eightday_plot <- list()
for (i in 1:length(RS667_files_plot)){
  raster_bathy_plot <- raster(RS667_files_plot[i]) %>% raster::crop(bathy_mask) %>% raster::mask(bathy_mask)
  eightday_plot[i] <- raster_bathy_plot
}

Rs667_stack_plot <- stack(eightday_plot)
max_rast_plot <- cellStats(Rs667_stack_plot,stat = 'max')
max_rast_df_plot <- data.frame(time_period = 1:46, maximum_turb_plot = max_rast_plot)
foo_8day <- data.frame("time" = max_rast_df_plot$time_period, "turb" = max_rast_df_plot$maximum_turb_plot)
foo_8day_ts <- ts(foo_8day[2])
plot_8day <- 
  ggplot(max_rast_df_plot, aes(time_period*8, maximum_turb_plot)) +
  geom_line(colour="blue") + 
  geom_point(colour="blue", size=2, fill = "blue") +
  theme_bw()
formatted_plot_8day <- plot_8day + 
  ylab("Mean ZOI 8-day RRS 667 (s^-1)") + 
  xlab("Day of the Year") + 
  ggtitle("Mean ZOI 8-day RRS 667, 2018") +
  scale_x_continuous(breaks = c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330, 360)) +
  scale_y_continuous(breaks = c(0.010, 0.0125, 0.015, 0.075))
kendall_8day <- MannKendall(foo_8day_ts)
kendall_8day

df_climato_diffs_ts <- ts(df_climato_diffs[2:5])
plot(df_climato_diffs_ts)
kendall_diffs <- MannKendall(df_climato_diffs_ts)
kendall_diffs

# More plot and random tests
Q2.1_df <- read.csv("/Users/rachelcarlson/Documents/Y1Q2_classes/ESS_241/project2/ResearchQ2.1.csv")
Q2.1_df$month <- ordered(Q2.1_df$month,
                         levels = c("Feb", "Mar", "Apr", "May"))
res.aov <- aov(diffs ~ month, data = Q2.1_df)
summary(res.aov)
TukeyHSD(res.aov)
