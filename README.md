# Africa_Parks_RS
## Turbidity variability and differences from monthly climatology for Bazaruto

Project experiments with MODIS-Aqua ocean color data to estimate turbidity in Bazaruto Archipelago National Park (south Mozambique) during a period of land-use change, 2005 to 2018. Methods are:

1. Extract raster files from NASA Ocean Color website for Rrs-667, 8-day temporal resolution, 2005 to 2018.
2. Create mask for study region based on <100 m bathymetry (GEBCO)
3. Clip rasters to study region and assess 1) mean pixel value for 8-day images, 2) variance in 8-day mean values over the course of each year. This indicates whether sedimentation plumes are become more erratic.
Plot data (most plots are created in Python, package "Altair")
