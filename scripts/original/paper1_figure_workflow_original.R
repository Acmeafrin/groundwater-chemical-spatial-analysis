
#Package

  library(sf)
  library(tmap)
  library(gstat) # Use gstat's idw routine
  
  
  
  
  
  library(sp)    # Used for the spsample function
  library(spatstat) 
  library(raster)    # Used to clip out thiessen polygons
  library(maps)
  library(mapproj)

  library(ggplot2)
  library(reshape2)
  library(nparcomp)
  library(corrplot)
  library(Hmisc)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(vegan)
  library(cowplot)
  library(party)
  library(caret)
  library(rpart)
  library(e1071)
  library(rpart.plot)
  library(randomForest)
  library(rfUtilities)
  library(ltm)
  library(grid)
  library(lmtest)
  library(gridExtra)
  library(agricolae)
  library(indicspecies)
  library(tidyverse)
  library(showtext)
  library(RColorBrewer)
  library(MASS)
  library(rcompanion)#tukey normalization
  library(ggpubr)
  library(rstatix)
  library(datasets)
  library(multcompView)
  library(ggmap)
  library(raster)
  library(ggforce)


  library(raster)
  
  library(rspat)
  library(leaflet)
  library(magrittr)
  library(remotes)
  library(mapview)
  library(maps)
  library(mapdata)
  library(sp)
  library(giscoR)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(crosstalk)
  library(plotly)

 
''#=========Well distribution map===========
x = read.csv("Groundwater(17-22).csv")
str(x)
{
  x$No. = as.numeric(x$No.)
  x$Year = as.numeric(x$Year)
  x$latitude = as.numeric(x$latitude)
  x$longitude = as.numeric(x$longitude)
  x$GQI = as.numeric(x$GQI)
  x$General_Bacteria = as.numeric(x$General_Bacteria)
}
str(x)
water = subset(x, x$Age == "Adult")
water = subset(water, water$시도=="충청남도")
summary(water$GQI)
table(water$GQI.rank)

summary(water$HQ.total)
water = subset(water, water$Year == "2017"|water$Year == "2018"| water$Year == "2019"| water$Year == "2020"| water$Year == "2021"| water$Year == "2022")

library(ggplot2)
library(sf)

# Read in your shapefile for the administrative boundaries
boundary_data <- st_read("LSMD_ADM_SECT_UMD_44.shp")
# Check the CRS for the boundary data
crs_boundary <- st_crs(boundary_data)

# Check the CRS for the water data (assuming it's an sf object; if not, it needs to be converted)
crs_water <- st_crs(water)

# Ensure 'water' dataframe has 'Year' as a factor
water$Year <- as.factor(water$Year)
# Replace 'longitude' and 'latitude' with your actual column names if they are different
water_sf <- st_as_sf(water, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

# Check CRS for the boundary data
crs_boundary <- st_crs(boundary_data)

# Now transform the CRS of the water_sf to match the boundary data
water_sf <- st_transform(water_sf, crs_boundary)
# Create the plot

ggplot() +
  geom_sf(data = boundary_data, fill = "white", color = "black") +
  geom_sf(data = water_sf, aes(color = Year), size = 2) +
  scale_color_manual(values = c(
    "2017" = "blue",
    "2018" = "red",
    "2019" = "green",
    "2020" = "orange",
    "2021" = "purple",
    "2022" = "darkgrey"
  )) +
  theme_bw() +
  ylab("Latitude") +
  xlab("Longitude") +
  ggtitle("Well Distribution Throughout the Year") +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 20),
    legend.position = c(0.1, 0.1),  # Position of legend inside the plot area, near the bottom left
    legend.justification = "bottom",  # Anchor point for the legend position
    legend.background = element_rect(fill = "white", colour = "black")  # Optionally add a background
  ) +
  guides(color = guide_legend(title = "Year"))


#===============================================================================
#Pollutant calculation different script
# Load necessary libraries
library(tidyr)
library(ggplot2)
library(colorspace)


{
x = read.csv("exceedence_chunhcheongnamado.csv")
# Ensure the data has the expected structure
str(x)

# Reshape the data from wide to long format
x_long <- gather(x, key = "pollutant", value = "num", -Parameters)



# Make sure the pollutant column is a factor with the correct levels
x_long$pollutant <- factor(x_long$pollutant, levels = c("T.C.", "E.C.", "General_Bacteria", "F.C.", "NO3.N", "Turbidity", "As", "Fe", "Al", "F", "Mn", "pH", "Others"))

# Create the bar plot with the specified custom colors and labels on top
count = ggplot(x_long, aes(fill = pollutant, y = num, x = pollutant)) + 
  geom_bar(position = 'stack', stat = 'identity') +
  geom_text(aes(label = num), vjust = -0.38, size = 4.8) +  # Add text labels on top of bars
  theme(panel.background = element_rect(fill = "white", color = "black"),
        axis.title.x = element_text(size = 0),
        axis.text.x = element_text(size = 20, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 25),
        plot.title = element_text(size = 25),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 0),
        legend.position = "none") +  # Remove legend
  scale_fill_manual(values = c("red2", "dodgerblue3", "forestgreen", "darkblue", "orange", 
                               "firebrick4", "seagreen", "tomato3", "tan4", "mediumvioletred", 
                               "cornflowerblue", "indianred1", "darkgray")) +
  labs(x = '', y = 'Counts', fill = "") +
  scale_x_discrete(labels = c("Total coliform", expression(italic("Escherichia coli")), 
                              "General Bacteria", expression(paste("NO"[3], "-N")), 
                              "Fecal coliform", "Turbidity", "As", "Fe", "Al", "F", "Mn", "pH", "Others"))

# Display the plot
count



}
{
  x = read.csv("pollutant_info충남.csv")
  x = x[-c(1:4),]
  x = x[c(1:9),]
  
  x$pollutant <- factor(x$pollutant, levels = c("NO3-N","Turbidity","As","Fe","Al","F","Mn","pH","Others"))
  library(colorspace)
  my_palette <- qualitative_hcl(9, "Dynamic")
  
  count = ggplot(x, aes(fill=pollutant, y=num, x=pollutant, palette = "Set1")) + 
    geom_bar(position='stack', stat='identity')+
    theme(panel.background = element_rect(fill="white", color = "black"),
          axis.title.x = element_text(size=0),
          axis.text.x = element_text(size=20,angle=45,hjust=1), axis.text.y = element_text(size=20),
          axis.title.y = element_text(size=25),
          plot.title = element_text(size = 25),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 0),
          legend.position = "NA")+
    scale_fill_manual(values = c("red2","dodgerblue3","forestgreen","darkblue","orange","firebrick4","seagreen","tomato3",
                                       "tan4"))+
                                         labs(x='', y='Counts')+ labs(fill = "") +
    scale_x_discrete(labels = c(expression(paste("NO"[3],"-N")), "Turbidity", "As", "Fe", "Al", "F", "Mn", "pH", "Others"))
  count
}
#===============================================================================
#interpolation

  library(tmap)
  library(gstat) 
  library(sp)   
  library(spatstat)  
  library(sf)
  library(raster)    
  library(maps)
  library(mapproj)
  
library(ggplot2)
  library(scales)

  df_map_cn <- st_read("LSMD_ADM_SECT_UMD_44.shp")
  # Assuming map_cn is your sf object with the original CRS
  to_crs <- "EPSG:4326" # WGS 84
  df_map_cn_transformed <- st_transform(df_map_cn, crs = to_crs)
  head(df_map_cn_transformed)
  #df_map_cn <- spTransform(map_cn, to.crs)= didn't work, changed the format of the script
  
  sg = read.csv("Groundwater(17-22).csv",header = TRUE, na.strings = c("", "NA", "-"))
  head(sg)
 # Subsetting the data
  sg_adults <- subset(sg, sg$시도 == "충청남도" & sg$Age == "Adult")
  
  # Check the number of rows after subsetting
  nrow(sg_adults)
  # Assuming longitude and latitude are critical and you only want to remove rows where these are NA
  sg_clean <- sg_adults[!is.na(sg_adults$longitude) & !is.na(sg_adults$latitude), ]
  # Print the names of the dataframe
  print(names(sg_clean))
  # Make sure that the longitude and latitude columns are correctly converted to numeric
  sg_clean$longitude <- as.numeric(as.character(sg_clean$longitude))
  sg_clean$latitude <- as.numeric(as.character(sg_clean$latitude))
  # Checking if any NA values were introduced during conversion
  sum(is.na(sg_clean$longitude))
  sum(is.na(sg_clean$latitude))
  # Now, create the SpatialPointsDataFrame
 
  s_p <- SpatialPointsDataFrame(coords = sg_clean[, c("longitude", "latitude")], 
                                data = sg_clean[, !names(sg_clean) %in% c("longitude", "latitude")])
  
  # Make sure that the longitude and latitude columns are correctly converted to numeric
  s_p <- SpatialPointsDataFrame(coords = sg_clean[, c("longitude", "latitude")], data = sg_clean[, -c(1:2)])
  # Assuming df_map_cn_transformed's bbox is the desired bounding box for SpatialPointsDataFrame
  bbox_sp <- st_bbox(df_map_cn_transformed)
  s_p@bbox <- matrix(c(bbox_sp["xmin"], bbox_sp["ymin"], bbox_sp["xmax"], bbox_sp["ymax"]), nrow = 2)
  # Create a grid for interpolation
  grd <- as.data.frame(spsample(s_p, "regular", n=200000))
  names(grd) <- c("longitude", "latitude")
  coordinates(grd) <- ~longitude + latitude
  gridded(grd) <- TRUE
  fullgrid(grd) <- TRUE
  # Set CRS for grid to match s_p
  crs(grd) <- crs(s_p)
  # Perform IDW interpolation
  P.idw.NO3.N <- gstat::idw(formula = NO3.N ~ 1, locations = s_p, newdata = grd, idp = 2)
  # Create a raster from the IDW output
  r.NO3.N <- raster(P.idw.NO3.N)
  # Ensure that r.NO3.N is a RasterLayer object
  if (!inherits(r.NO3.N, "RasterLayer")) {
    r.NO3.N <- raster(r.NO3.N)
  }
  
  # Make sure you're using the mask function from the raster package
  r.NO3.N <- raster::raster(P.idw.NO3.N)
  
  # Convert the sf object to a Spatial object correctly
  df_map_cn_spatial <- as(df_map_cn_transformed, 'Spatial')
  
  # Check that the conversion was successful and df_map_cn_spatial is a Spatial object
  print(class(df_map_cn_spatial))
  
  # Ensure both r.NO3.N and df_map_cn_spatial have the same CRS
  crs(r.NO3.N) <- crs(df_map_cn_transformed)
  crs(df_map_cn_spatial) <- crs(df_map_cn_transformed) # This should be redundant if df_map_cn_transformed had a CRS
  
  # Now apply the mask
  r.m.NO3.N <- raster::mask(r.NO3.N, df_map_cn_spatial)
  
  # Set the CRS for r.NO3.N to match df_map_cn_transformed
  crs(r.NO3.N) <- crs(df_map_cn_transformed)
  
  df_map_cn_spatial <- as(df_map_cn_transformed, "Spatial")
  # Attempt to fix invalid geometries
  df_map_cn_transformed <- st_make_valid(df_map_cn_transformed)
  
  # Define the CRS for r.NO3.N to match df_map_cn_transformed (WGS 84)
  crs(r.NO3.N) <- CRS("+init=epsg:4326")
  # Set the CRS for r.NO3.N to WGS 84 using EPSG code directly
  crs(r.NO3.N) <- CRS("EPSG:4326")
  
  #Visualize interpolation
  
  library(tmap)
  
  # Assuming df_map_cn_transformed is your borders and r.m.NO3.N is your raster layer
  tm_shape(df_map_cn_transformed) +
    tm_borders(col = "black", lwd = 0.5) +  # Outer boundaries
    tm_shape(r.m.NO3.N) +
    tm_raster(n = 5, palette = colorRampPalette(c("#006400", "#9ACD32", "yellow2", "orange2", "red2"))(5),
              title = "NO₃-N",
              labels = c("0 - 5", "5 - 10", "10 - 15", "15 - 20", "20 <"),
              breaks = c(0, 5, 10, 15, 20, 80)) +
    tm_shape(df_map_cn_transformed) +
    tm_borders(col = "black", lwd = 0.5) +  # Inner boundaries
    tm_layout(asp = 1,
              legend.position = c("left", "bottom"),
              legend.title.size = 1.40, legend.title.fontface = "bold",
              legend.text.size = 0.9, legend.text.fontface = "bold",
              legend.frame = TRUE)
  
    #============
  library(tmap)
  
  # Unicode for a narrow no-break space
  nnbsp <- "\u202F"
  
  # Generate the map with a custom legend title using narrow non-breaking spaces for tighter spacing
  tmap_map <- tm_shape(df_map_cn_transformed) +
    tm_borders(col = "black", lwd = 0.5) +  # Outer boundaries
    tm_shape(r.m.NO3.N) +
    tm_raster(n = 5, palette = colorRampPalette(c("#006400", "#9ACD32", "yellow2", "orange2", "red2"))(5),
              title = paste0("NO", nnbsp, "₃", nnbsp, "-N"),  # Attempting tighter spacing
              labels = c("0 - 5", "5 - 10", "10 - 15", "15 - 20", "20 <"),
              breaks = c(0, 5, 10, 15, 20, 80)) +
    tm_shape(df_map_cn_transformed) +
    tm_borders(col = "black", lwd = 0.5) +  # Inner boundaries
    tm_layout(asp = 1,
              legend.position = c("left", "bottom"),
              legend.title.size = 1.40, legend.title.fontface = "bold",
              legend.text.size = 0.9, legend.text.fontface = "bold",
              legend.frame = TRUE)
  
  # Print the map
  print(tmap_map)
  
    #===========
   
   #Turbidity idw
  
  P.idw.Turbidity <- gstat::idw(formula = Turbidity ~ 1, locations = s_p, newdata = grd, idp = 2)
  # Create a raster from the IDW output
  r.Turbidity <- raster(P.idw.Turbidity)
  # Ensure that r.NO3.N is a RasterLayer object
  if (!inherits(r.Turbidity, "RasterLayer")) {
    r.Turbidity <- raster(r.Turbidity)
  }
  
  # Make sure you're using the mask function from the raster package
  r.Turbidity <- raster::raster(P.idw.Turbidity)
  
  # Convert the sf object to a Spatial object correctly
  df_map_cn_spatial <- as(df_map_cn_transformed, 'Spatial')
  
  # Check that the conversion was successful and df_map_cn_spatial is a Spatial object
  print(class(df_map_cn_spatial))
  
  # Ensure both r.NO3.N and df_map_cn_spatial have the same CRS
  crs(r.Turbidity) <- crs(df_map_cn_transformed)
  crs(df_map_cn_spatial) <- crs(df_map_cn_transformed) # This should be redundant if df_map_cn_transformed had a CRS
  
  # Now apply the mask
  r.m.Turbidity<- raster::mask(r.Turbidity, df_map_cn_spatial)
  
  # Set the CRS for r.NO3.N to match df_map_cn_transformed
  crs(r.Turbidity) <- crs(df_map_cn_transformed)
  
  df_map_cn_spatial <- as(df_map_cn_transformed, "Spatial")
  # Attempt to fix invalid geometries
  df_map_cn_transformed <- st_make_valid(df_map_cn_transformed)
  
  # Define the CRS for r.NO3.N to match df_map_cn_transformed (WGS 84)
  crs(r.Turbidity) <- CRS("+init=epsg:4326")
  # Set the CRS for r.NO3.N to WGS 84 using EPSG code directly
  crs(r.Turbidity) <- CRS("EPSG:4326")
  
  #Visualize interpolation
  
  library(tmap)
  
  # Assuming df_map_cn_transformed is your borders and r.m.Turbidity is your raster layer
  tm_shape(df_map_cn_transformed) +
    tm_borders(col = "black", lwd = 0.5) +  # Outer boundaries
    tm_shape(r.m.Turbidity) +
    tm_raster(n = 5, palette = colorRampPalette(c("#006400", "#9ACD32", "yellow2", "orange2", "red2"))(5),
              title = "Turbidity",
              labels = c("0 - 0.5", "0.5 - 1", "1 - 10", "10 - 20", "> 20"),
              breaks = c(0, 0.5, 1, 10, 20, 160))+  # Adjusted breaks to match labels
    tm_shape(df_map_cn_transformed) +
    tm_borders(col = "black", lwd = 0.5) +  # Inner boundaries
    tm_layout(asp = 1,
              legend.position = c("left", "bottom"),
              legend.title.size = 1.40, legend.title.fontface = "bold",
              legend.text.size = 0.9, legend.text.fontface = "bold",
              legend.frame = TRUE)
  
   
   # As IDW (충청남도)
  P.idw.As <- gstat::idw(formula = As ~ 1, locations = s_p, newdata = grd, idp = 2)
  # Create a raster from the IDW output
  r.As <- raster(P.idw.As)
  # Ensure that r.NO3.N is a RasterLayer object
  if (!inherits(r.As, "RasterLayer")) {
    r.As <- raster(r.As)
  }
  
  # Make sure you're using the mask function from the raster package
  r.As <- raster::raster(P.idw.As)
  
  # Convert the sf object to a Spatial object correctly
  df_map_cn_spatial <- as(df_map_cn_transformed, 'Spatial')
  
  # Check that the conversion was successful and df_map_cn_spatial is a Spatial object
  print(class(df_map_cn_spatial))
  
  # Ensure both r.NO3.N and df_map_cn_spatial have the same CRS
  crs(r.As) <- crs(df_map_cn_transformed)
  crs(df_map_cn_spatial) <- crs(df_map_cn_transformed) # This should be redundant if df_map_cn_transformed had a CRS
  
  # Now apply the mask
  r.m.As <- raster::mask(r.As, df_map_cn_spatial)
  
  # Set the CRS for r.NO3.N to match df_map_cn_transformed
  crs(r.As) <- crs(df_map_cn_transformed)
  
  df_map_cn_spatial <- as(df_map_cn_transformed, "Spatial")
  # Attempt to fix invalid geometries
  df_map_cn_transformed <- st_make_valid(df_map_cn_transformed)
  
  # Define the CRS for r.NO3.N to match df_map_cn_transformed (WGS 84)
  crs(r.As) <- CRS("+init=epsg:4326")
  # Set the CRS for r.NO3.N to WGS 84 using EPSG code directly
  crs(r.As) <- CRS("EPSG:4326")
  
  #Visualize interpolation
  library(tmap)
  
  # Assuming df_map_cn_transformed is your borders and r.m.As is your raster layer
  tm_shape(df_map_cn_transformed) +
    tm_borders(col = "black", lwd = 0.5) +  # Outer boundaries
    tm_shape(r.m.As) +
    tm_raster(n = 5, palette = colorRampPalette(c("#006400", "#9ACD32", "yellow2", "orange2", "red2"))(5),
              title = "As",
              labels = c("0 - 0.003", "0.003 - 0.01", "0.01 - 0.05", "0.05 - 0.08", "> 0.08"),
              breaks = c(0, 0.003, 0.01, 0.05, 0.08, 0.117)) +
    tm_shape(df_map_cn_transformed) +
    tm_borders(col = "black", lwd = 0.5) +  # Inner boundaries
    tm_layout(asp = 1,
              legend.position = c("left", "bottom"),
              legend.title.size = 1.40, legend.title.fontface = "bold",
              legend.text.size = 0.9, legend.text.fontface = "bold",
              legend.frame = TRUE)
  

  #Fe IDW
  
  # As IDW (충청남도)
  P.idw.Fe <- gstat::idw(formula = Fe ~ 1, locations = s_p, newdata = grd, idp = 2)
  # Create a raster from the IDW output
  r.Fe <- raster(P.idw.Fe)
  # Ensure that r.NO3.N is a RasterLayer object
  if (!inherits(r.As, "RasterLayer")) {
    r.Fe <- raster(r.Fe)
  }
  
  # Make sure you're using the mask function from the raster package
  r.Fe <- raster::raster(P.idw.Fe)
  
  # Convert the sf object to a Spatial object correctly
  df_map_cn_spatial <- as(df_map_cn_transformed, 'Spatial')
  
  # Check that the conversion was successful and df_map_cn_spatial is a Spatial object
  print(class(df_map_cn_spatial))
  
  # Ensure both r.NO3.N and df_map_cn_spatial have the same CRS
  crs(r.Fe) <- crs(df_map_cn_transformed)
  crs(df_map_cn_spatial) <- crs(df_map_cn_transformed) # This should be redundant if df_map_cn_transformed had a CRS
  
  # Now apply the mask
  r.m.Fe <- raster::mask(r.Fe, df_map_cn_spatial)
  
  # Set the CRS for r.NO3.N to match df_map_cn_transformed
  crs(r.Fe) <- crs(df_map_cn_transformed)
  
  df_map_cn_spatial <- as(df_map_cn_transformed, "Spatial")
  # Attempt to fix invalid geometries
  df_map_cn_transformed <- st_make_valid(df_map_cn_transformed)
  
  # Define the CRS for r.NO3.N to match df_map_cn_transformed (WGS 84)
  crs(r.Fe) <- CRS("+init=epsg:4326")
  # Set the CRS for r.NO3.N to WGS 84 using EPSG code directly
  crs(r.Fe) <- CRS("EPSG:4326")
  
  #Visualize interpolation
  
  library(tmap)
  
  # Assuming df_map_cn_transformed is your borders and r.m.Fe is your raster layer
  tm_shape(df_map_cn_transformed) +
    tm_borders(col = "black", lwd = 0.5) +  # Outer boundaries
    tm_shape(r.m.Fe) +
    tm_raster(n = 5, palette = colorRampPalette(c("#006400", "#9ACD32", "yellow2", "orange2", "red2"))(5),
              title = "Fe",
              labels = c("0 - 0.01", "0.008 - 0.1", "0.1 - 0.3", "0.3 - 3", "> 3"),
              breaks = c(0, 0.01, 1, 2, 3, 22.89)) +
    tm_shape(df_map_cn_transformed) +
    tm_borders(col = "black", lwd = 0.5) +  # Inner boundaries
    tm_layout(asp = 1,
              legend.position = c("left", "bottom"),
              legend.title.size = 1.40, legend.title.fontface = "bold",
              legend.text.size = 0.9, legend.text.fontface = "bold",
              legend.frame = TRUE)
  
 
#=Global Moran 
{
  dat <- read.csv("Groundwater(17-22).csv")
  {
    dat$No. = as.numeric(dat$No.)
    dat$Year = as.numeric(dat$Year)
    dat$latitude = as.numeric(dat$latitude)
    dat$longitude = as.numeric(dat$longitude)
    dat$GQI = as.numeric(dat$GQI)
    dat$General_Bacteria = as.numeric(dat$General_Bacteria)
  }
  dat = subset(dat, dat$시도 == "충청남도")
  ad = subset(dat, dat$Age == "Adult")
  ch = subset(dat, dat$Age == "Child")
  
  #일단 0값들을 다 수정 => 공간분석을 위해선 0값 X
  ad[c(17:56)][ad[c(17:56)]==0]<-0.000001
  ch[c(17:56)][ch[c(17:56)]==0]<-0.000001
  str(ad)
  ad.net = ad[complete.cases(ad$longitude), ] #NA는 제거! (3개 행 제거)
  ch.net = ch[complete.cases(ch$longitude), ] #NA는 제거! (3개 행 제거)
  
  #데이터 표준화 (x축 변수= (수질인자 농도-평균)/표준편차) 즉 z-score 사용 -> 평균을 0으로 표준편차를 1로 만든다
  #Adult
  ad.net.s = scale(ad.net[c(17,19,20,22,23:56)])
  ad.net.s = as.data.frame(ad.net.s)
  ad.net = cbind(ad.net[c(1:11)], ad.net.s)
  summary(ad.net$General_Bacteria) #표준화가 잘 됐나 확인
  summary(ad.net$HQ.total) #표준화가 잘 됐나 확인
  #Child
  ch.net.s = scale(ch.net[c(17,19,20,22,23:56)])
  ch.net.s = as.data.frame(ch.net.s)
  ch.net = cbind(ch.net[c(1:11)], ch.net.s)
  summary(ch.net$General_Bacteria) #표준화가 잘 됐나 확인
  summary(ch.net$HQ.total) #표준화가 잘 됐나 확인
  
  #Good
  head(ad.net)
  head(ch.net)
  
  #이웃 관계 리스트 생성하기
  coords.ad <- cbind(ad.net$longitude, ad.net$latitude)
  coords.ch <- cbind(ch.net$longitude, ch.net$latitude)
  nb.ad <- knn2nb(knearneigh(coords.ad, k = 20)) # k-최근접 이웃 방식으로 이웃 관계 리스트 생성 => 고밀도 불균등 분포 데이터
  nb.ch <- knn2nb(knearneigh(coords.ch, k = 20)) # k-최근접 이웃 방식으로 이웃 관계 리스트 생성 => 고밀도 불균등 분포 데이터
  
  #공간 가중치 행렬 생성하기
  # dmat <- as.matrix (dist (cbind (ad.net$longitude, ad.net$latitude)))
  w.ad <- nb2listw (nb.ad, style = "minmax")
  w.ch <- nb2listw (nb.ch, style = "minmax")
  
#=================================================  
  #moran’s index 계산하기

  #NO3-N
  moran.test(ad.net$NO3.N, w.ad)
  mt <- moran.test(ad.net$NO3.N, w.ad)
  z_score <- (mt$estimate["Moran I statistic"] - mt$estimate["Expectation"]) / sqrt(mt$estimate["Variance"]) # z-score 계산식 적용하기
  print(z_score) # z-score 값 출력하기
  
  #NO3.N
  x = ad.net$NO3.N #수질인자 선정
  wx <- lag.listw(w.ad, x) # 공간적 지연값 
  xwx.lm <- lm(wx ~ x) # 회귀모형 
  infl.xwx <- influence.measures(xwx.lm) # 영향력 측정 
  is.inf <- which(apply(infl.xwx$is.inf, 1, any)) # 영향력이 큰 점들의 인덱스 
  labels <- as.character(ad.net$id) # 레이블
  
  #moran’s index 산점도 그래프 그리기
  NO3.N.moran = ggplot(data.frame(x = x, wx = wx), aes(x = x, y = wx)) + # 데이터 프레임과 매핑 설정 
    geom_point(aes(color = interaction(cut(x, c(-Inf, mean(x), Inf)), cut(wx, c(-Inf, mean(wx), Inf)))),size = 2.5) +
    scale_color_manual(values = c("yellow2", "dodgerblue3", "springgreen3", "red2")) + # 점의 색상을 수동으로 지정
    geom_smooth(method = "lm", se = FALSE) + # 회귀선 표시 
    geom_hline(yintercept = mean(wx), lty = 2) + # y=x 선 표시 
    geom_vline(xintercept = mean(x), lty = 2) + # y=x 선 표시 
    # geom_point(data = data.frame(x = x[is.inf], wx = wx[is.inf]), shape = 9) + # 영향력이 큰 점들 표시 
    # geom_text(data = data.frame(x = x[is.inf], wx = wx[is.inf], labels = labels[is.inf]), aes(label = labels), vjust = -0.5) + # 영향력이 큰 점들에 레이블 붙이기 
    labs(title = expression(paste("Moran’s index scatterplot (", NO[3]~"-"~N, ")")),
         x = expression(paste("Standardized ", NO[3]~"-"~N)),
         y = expression(paste("Spatially lagged ", NO[3]~"-"~N))) + # y축 레이블 설정 
    theme_bw() + # 테마 설정 
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 20),
          title = element_text(size=0),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = "NA")+
    xlim(-0.5,10) + ylim(-1,2.4)+
    annotate("text",x=7.5, y=2.1, label = "Moran's I = 0.172", size=10)
  
  NO3.N.moran
  #=================
  
  
  #General_Bacteria
  moran.test(ad.net$General_Bacteria, w.ad)
  mt <- moran.test(ad.net$General_Bacteria, w.ad)
  z_score <- (mt$estimate["Moran I statistic"] - mt$estimate["Expectation"]) / sqrt(mt$estimate["Variance"]) # z-score 계산식 적용하기
  print(z_score) # z-score 값 출력하기
  
  #NO3.N
  x = ad.net$General_Bacteria #수질인자 선정
  wx <- lag.listw(w.ad, x) # 공간적 지연값 
  xwx.lm <- lm(wx ~ x) # 회귀모형 
  infl.xwx <- influence.measures(xwx.lm) # 영향력 측정 
  is.inf <- which(apply(infl.xwx$is.inf, 1, any)) # 영향력이 큰 점들의 인덱스 
  labels <- as.character(ad.net$id) # 레이블
  
  #moran’s index 산점도 그래프 그리기
  General_Bacteria.moran = ggplot(data.frame(x = x, wx = wx), aes(x = x, y = wx)) + # 데이터 프레임과 매핑 설정 
    geom_point(aes(color = interaction(cut(x, c(-Inf, mean(x), Inf)), cut(wx, c(-Inf, mean(wx), Inf)))),size = 2.5) +
    scale_color_manual(values = c("#0969a2", "black", "lightgray", "red2")) + # 점의 색상을 수동으로 지정
    geom_smooth(method = "lm", se = FALSE) + # 회귀선 표시 
    geom_hline(yintercept = mean(wx), lty = 2) + # y=x 선 표시 
    geom_vline(xintercept = mean(x), lty = 2) + # y=x 선 표시 
    # geom_point(data = data.frame(x = x[is.inf], wx = wx[is.inf]), shape = 9) + # 영향력이 큰 점들 표시 
    # geom_text(data = data.frame(x = x[is.inf], wx = wx[is.inf], labels = labels[is.inf]), aes(label = labels), vjust = -0.5) + # 영향력이 큰 점들에 레이블 붙이기 
    labs(title = "Moran’s index scatterplot (General Bacteria)", # 제목 설정 
         x = "Standardized General Bacteria", # x축 레이블 설정 
         y = "Spatially lagged General Bacteria") + # y축 레이블 설정 
    theme_bw() + # 테마 설정 
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 20),
          title = element_text(size=0),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = "NA")+
    xlim(-0.5,10) + ylim(-1,2.4)+
    annotate("text",x=7.5, y=2.1, label = "Moran's I = 0.034", size=10)
  
  General_Bacteria.moran
  #Turbidity
  moran.test(ad.net$Turbidity, w.ad)
  mt <- moran.test(ad.net$Turbidity, w.ad)
  z_score <- (mt$estimate["Moran I statistic"] - mt$estimate["Expectation"]) / sqrt(mt$estimate["Variance"]) # z-score 계산식 적용하기
  print(z_score) # z-score 값 출력하기
  
  #Turbidity
  x = ad.net$Turbidity #수질인자 선정
  wx <- lag.listw(w.ad, x) # 공간적 지연값 
  xwx.lm <- lm(wx ~ x) # 회귀모형 
  infl.xwx <- influence.measures(xwx.lm) # 영향력 측정 
  is.inf <- which(apply(infl.xwx$is.inf, 1, any)) # 영향력이 큰 점들의 인덱스 
  labels <- as.character(ad.net$id) # 레이블
  
  #moran’s index 산점도 그래프 그리기
  Turbidity.moran = ggplot(data.frame(x = x, wx = wx), aes(x = x, y = wx)) + # 데이터 프레임과 매핑 설정 
    geom_point(aes(color = interaction(cut(x, c(-Inf, mean(x), Inf)), cut(wx, c(-Inf, mean(wx), Inf)))),size = 2.5) +
    scale_color_manual(values = c("yellow2", "dodgerblue3", "springgreen3", "red2")) + # 점의 색상을 수동으로 지정
    geom_smooth(method = "lm", se = FALSE) + # 회귀선 표시 
    geom_hline(yintercept = mean(wx), lty = 2) + # y=x 선 표시 
    geom_vline(xintercept = mean(x), lty = 2) + # y=x 선 표시 
    # geom_point(data = data.frame(x = x[is.inf], wx = wx[is.inf]), shape = 9) + # 영향력이 큰 점들 표시 
    # geom_text(data = data.frame(x = x[is.inf], wx = wx[is.inf], labels = labels[is.inf]), aes(label = labels), vjust = -0.5) + # 영향력이 큰 점들에 레이블 붙이기 
    labs(title = "Moran’s index scatterplot (Turbidity)", # 제목 설정 
         x = "Standardized Turbidity", # x축 레이블 설정 
         y = "Spatially lagged Turbidity") + # y축 레이블 설정 
    theme_bw() + # 테마 설정 
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 20),
          title = element_text(size=0),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = "NA")+
    xlim(-2,10) + ylim(-0.3,1)+
    annotate("text",x=7, y=0.9, label = "Moran's I = 0.002", size=10)
  
  Turbidity.moran
  #=================
  #As
  moran.test(ad.net$As, w.ad)
  mt <- moran.test(ad.net$As, w.ad)
  z_score <- (mt$estimate["Moran I statistic"] - mt$estimate["Expectation"]) / sqrt(mt$estimate["Variance"]) # z-score 계산식 적용하기
  print(z_score) # z-score 값 출력하기
  
  #As
  x = ad.net$As #수질인자 선정
  wx <- lag.listw(w.ad, x) # 공간적 지연값 
  xwx.lm <- lm(wx ~ x) # 회귀모형 
  infl.xwx <- influence.measures(xwx.lm) # 영향력 측정 
  is.inf <- which(apply(infl.xwx$is.inf, 1, any)) # 영향력이 큰 점들의 인덱스 
  labels <- as.character(ad.net$id) # 레이블
  
  #moran’s index 산점도 그래프 그리기
  As.moran = ggplot(data.frame(x = x, wx = wx), aes(x = x, y = wx)) + # 데이터 프레임과 매핑 설정 
    geom_point(aes(color = interaction(cut(x, c(-Inf, mean(x), Inf)), cut(wx, c(-Inf, mean(wx), Inf)))),size = 2.5) +
    scale_color_manual(values = c("yellow2", "dodgerblue3", "springgreen3", "red2")) + # 점의 색상을 수동으로 지정
    geom_smooth(method = "lm", se = FALSE) + # 회귀선 표시 
    geom_hline(yintercept = mean(wx), lty = 2) + # y=x 선 표시 
    geom_vline(xintercept = mean(x), lty = 2) + # y=x 선 표시 
    # geom_point(data = data.frame(x = x[is.inf], wx = wx[is.inf]), shape = 9) + # 영향력이 큰 점들 표시 
    # geom_text(data = data.frame(x = x[is.inf], wx = wx[is.inf], labels = labels[is.inf]), aes(label = labels), vjust = -0.5) + # 영향력이 큰 점들에 레이블 붙이기 
    labs(title = "Moran’s index scatterplot (As)", # 제목 설정 
         x = "Standardized As", # x축 레이블 설정 
         y = "Spatially lagged As") + # y축 레이블 설정 
    theme_bw() + # 테마 설정 
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 20),
          title = element_text(size=0),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = "NA")+
    xlim(-2,12) + ylim(-1,4)+ annotate("text",x=8.7, y=3.6, label = "Moran's I = 0.069", size=10)
  
  As.moran
  #=================
  #Fe
  moran.test(ad.net$Fe, w.ad)
  mt <- moran.test(ad.net$Fe, w.ad)
  z_score <- (mt$estimate["Moran I statistic"] - mt$estimate["Expectation"]) / sqrt(mt$estimate["Variance"]) # z-score 계산식 적용하기
  print(z_score) # z-score 값 출력하기
  
  #Fe
  x = ad.net$Fe #수질인자 선정
  wx <- lag.listw(w.ad, x) # 공간적 지연값 
  xwx.lm <- lm(wx ~ x) # 회귀모형 
  infl.xwx <- influence.measures(xwx.lm) # 영향력 측정 
  is.inf <- which(apply(infl.xwx$is.inf, 1, any)) # 영향력이 큰 점들의 인덱스 
  labels <- as.character(ad.net$id) # 레이블
  
  #moran’s index 산점도 그래프 그리기
  Fe.moran = ggplot(data.frame(x = x, wx = wx), aes(x = x, y = wx)) + # 데이터 프레임과 매핑 설정 
    geom_point(aes(color = interaction(cut(x, c(-Inf, mean(x), Inf)), cut(wx, c(-Inf, mean(wx), Inf)))),size = 2.5) +
    scale_color_manual(values = c("yellow2", "dodgerblue3", "springgreen3", "red2")) + # 점의 색상을 수동으로 지정
    geom_smooth(method = "lm", se = FALSE) + # 회귀선 표시 
    geom_hline(yintercept = mean(wx), lty = 2) + # y=x 선 표시 
    geom_vline(xintercept = mean(x), lty = 2) + # y=x 선 표시 
    # geom_point(data = data.frame(x = x[is.inf], wx = wx[is.inf]), shape = 9) + # 영향력이 큰 점들 표시 
    # geom_text(data = data.frame(x = x[is.inf], wx = wx[is.inf], labels = labels[is.inf]), aes(label = labels), vjust = -0.5) + # 영향력이 큰 점들에 레이블 붙이기 
    labs(title = "Moran’s index scatterplot (Fe)", # 제목 설정 
         x = "Standardized Fe", # x축 레이블 설정 
         y = "Spatially lagged Fe") + # y축 레이블 설정 
    theme_bw() + # 테마 설정 
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 20),
          title = element_text(size=0),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = "NA")+
    xlim(-1,4) + ylim(-0.3,0.8)+ annotate("text",x=2.7, y=0.7, label = "Moran's I = 0.004", size=10)
  
  Fe.moran
  #=================
  #Al
  moran.test(ad.net$Al, w.ad)
  mt <- moran.test(ad.net$Al, w.ad)
  z_score <- (mt$estimate["Moran I statistic"] - mt$estimate["Expectation"]) / sqrt(mt$estimate["Variance"]) # z-score 계산식 적용하기
  print(z_score) # z-score 값 출력하기
  
  #Al
  x = ad.net$Al #수질인자 선정
  wx <- lag.listw(w.ad, x) # 공간적 지연값 
  xwx.lm <- lm(wx ~ x) # 회귀모형 
  infl.xwx <- influence.measures(xwx.lm) # 영향력 측정 
  is.inf <- which(apply(infl.xwx$is.inf, 1, any)) # 영향력이 큰 점들의 인덱스 
  labels <- as.character(ad.net$id) # 레이블
  
  #moran’s index 산점도 그래프 그리기
  Al.moran = ggplot(data.frame(x = x, wx = wx), aes(x = x, y = wx)) + # 데이터 프레임과 매핑 설정 
    geom_point(aes(color = interaction(cut(x, c(-Inf, mean(x), Inf)), cut(wx, c(-Inf, mean(wx), Inf)))),size = 2.5) +
    scale_color_manual(values = c("yellow2", "dodgerblue3", "springgreen3", "red2")) + # 점의 색상을 수동으로 지정
    geom_smooth(method = "lm", se = FALSE) + # 회귀선 표시 
    geom_hline(yintercept = mean(wx), lty = 2) + # y=x 선 표시 
    geom_vline(xintercept = mean(x), lty = 2) + # y=x 선 표시 
    # geom_point(data = data.frame(x = x[is.inf], wx = wx[is.inf]), shape = 9) + # 영향력이 큰 점들 표시 
    # geom_text(data = data.frame(x = x[is.inf], wx = wx[is.inf], labels = labels[is.inf]), aes(label = labels), vjust = -0.5) + # 영향력이 큰 점들에 레이블 붙이기 
    labs(title = "Moran’s index scatterplot (Al)", # 제목 설정 
         x = "Standardized Al", # x축 레이블 설정 
         y = "Spatially lagged Al") + # y축 레이블 설정 
    theme_bw() + # 테마 설정 
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 20),
          title = element_text(size=0),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = "NA")+
    xlim(-2,15) + ylim(-0.5,2)+ annotate("text",x=10.5, y=1.85, label = "Moran's I = 0.074", size=10)
  
  Al.moran
  #==============
  #F
  moran.test(ad.net$F, w.ad) 
  mt <- moran.test(ad.net$F, w.ad) 
  z_score <- (mt$estimate["Moran I statistic"] - mt$estimate["Expectation"]) / sqrt(mt$estimate["Variance"]) # z-score 계산식 적용하기
  print(z_score) # z-score 값 출력하기
  
  #F
  x = ad.net$F #수질인자 선정
  wx <- lag.listw(w.ad, x) # 공간적 지연값 
  xwx.lm <- lm(wx ~ x) # 회귀모형 
  infl.xwx <- influence.measures(xwx.lm) # 영향력 측정 
  is.inf <- which(apply(infl.xwx$is.inf, 1, any)) # 영향력이 큰 점들의 인덱스 
  labels <- as.character(ad.net$id) # 레이블
  #moran’s index 산점도 그래프 그리기
  F.moran = ggplot(data.frame(x = x, wx = wx), aes(x = x, y = wx)) + # 데이터 프레임과 매핑 설정 
    geom_point(aes(color = interaction(cut(x, c(-Inf, mean(x), Inf)), cut(wx, c(-Inf, mean(wx), Inf)))),size = 2.5) +
    scale_color_manual(values = c("yellow2", "dodgerblue3", "springgreen3", "red2")) + # 점의 색상을 수동으로 지정
    geom_smooth(method = "lm", se = FALSE) + # 회귀선 표시 
    geom_hline(yintercept = mean(wx), lty = 2) + # y=x 선 표시 
    geom_vline(xintercept = mean(x), lty = 2) + # y=x 선 표시 
    # geom_point(data = data.frame(x = x[is.inf], wx = wx[is.inf]), shape = 9) + # 영향력이 큰 점들 표시 
    # geom_text(data = data.frame(x = x[is.inf], wx = wx[is.inf], labels = labels[is.inf]), aes(label = labels), vjust = -0.5) + # 영향력이 큰 점들에 레이블 붙이기 
    labs(title = "Moran’s index scatterplot (F)", # 제목 설정 
         x = "Standardized F", # x축 레이블 설정 
         y = "Spatially lagged F") + # y축 레이블 설정 
    theme_bw() + # 테마 설정 
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 20),
          title = element_text(size=0),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = "NA") +
    # xlim(-0.5,5) + ylim(-0.3,0.8)+ 
    annotate("text",x=12, y=3.2, label = "Moran's I = 0.171", size=10)
  F.moran
  #==============
  #Mn
  moran.test(ad.net$Mn, w.ad)
  mt <- moran.test(ad.net$Mn, w.ad) 
  z_score <- (mt$estimate["Moran I statistic"] - mt$estimate["Expectation"]) / sqrt(mt$estimate["Variance"]) # z-score 계산식 적용하기
  print(z_score) # z-score 값 출력하기
  
  #Mn
  x = ad.net$Mn #수질인자 선정
  wx <- lag.listw(w.ad, x) # 공간적 지연값 
  xwx.lm <- lm(wx ~ x) # 회귀모형 
  infl.xwx <- influence.measures(xwx.lm) # 영향력 측정 
  is.inf <- which(apply(infl.xwx$is.inf, 1, any)) # 영향력이 큰 점들의 인덱스 
  labels <- as.character(ad.net$id) # 레이블
  #moran’s index 산점도 그래프 그리기
  Mn.moran = ggplot(data.frame(x = x, wx = wx), aes(x = x, y = wx)) + # 데이터 프레임과 매핑 설정 
    geom_point(aes(color = interaction(cut(x, c(-Inf, mean(x), Inf)), cut(wx, c(-Inf, mean(wx), Inf)))),size = 2.5) +
    scale_color_manual(values = c("yellow2", "dodgerblue3", "springgreen3", "red2")) + # 점의 색상을 수동으로 지정
    geom_smooth(method = "lm", se = FALSE) + # 회귀선 표시 
    geom_hline(yintercept = mean(wx), lty = 2) + # y=x 선 표시 
    geom_vline(xintercept = mean(x), lty = 2) + # y=x 선 표시 
    # geom_point(data = data.frame(x = x[is.inf], wx = wx[is.inf]), shape = 9) + # 영향력이 큰 점들 표시 
    # geom_text(data = data.frame(x = x[is.inf], wx = wx[is.inf], labels = labels[is.inf]), aes(label = labels), vjust = -0.5) + # 영향력이 큰 점들에 레이블 붙이기 
    labs(title = "Moran’s index scatterplot (Mn)", # 제목 설정 
         x = "Standardized Mn", # x축 레이블 설정 
         y = "Spatially lagged Mn") + # y축 레이블 설정 
    theme_bw() + # 테마 설정 
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 20),
          title = element_text(size=0),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = "NA")+
    xlim(-2,15) + ylim(-1,3)+ annotate("text",x=10, y=2.7, label = "Moran's I = 0.027", size=10)
  Mn.moran
  #==============
  #pH
  moran.test(ad.net$pH, w.ad)
  mt <- moran.test(ad.net$pH, w.ad) 
  z_score <- (mt$estimate["Moran I statistic"] - mt$estimate["Expectation"]) / sqrt(mt$estimate["Variance"]) # z-score 계산식 적용하기
  print(z_score) # z-score 값 출력하기
  
  #pH
  x = ad.net$pH #수질인자 선정
  wx <- lag.listw(w.ad, x) # 공간적 지연값 
  xwx.lm <- lm(wx ~ x) # 회귀모형 
  infl.xwx <- influence.measures(xwx.lm) # 영향력 측정 
  is.inf <- which(apply(infl.xwx$is.inf, 1, any)) # 영향력이 큰 점들의 인덱스 
  labels <- as.character(ad.net$id) # 레이블
  #moran’s index 산점도 그래프 그리기
  pH.moran = ggplot(data.frame(x = x, wx = wx), aes(x = x, y = wx)) + # 데이터 프레임과 매핑 설정 
    geom_point(aes(color = interaction(cut(x, c(-Inf, mean(x), Inf)), cut(wx, c(-Inf, mean(wx), Inf)))),size = 2.5) +
    scale_color_manual(values = c("yellow2", "dodgerblue3", "springgreen3", "red2")) + # 점의 색상을 수동으로 지정
    geom_smooth(method = "lm", se = FALSE) + # 회귀선 표시 
    geom_hline(yintercept = mean(wx), lty = 2) + # y=x 선 표시 
    geom_vline(xintercept = mean(x), lty = 2) + # y=x 선 표시 
    # geom_point(data = data.frame(x = x[is.inf], wx = wx[is.inf]), shape = 9) + # 영향력이 큰 점들 표시 
    # geom_text(data = data.frame(x = x[is.inf], wx = wx[is.inf], labels = labels[is.inf]), aes(label = labels), vjust = -0.5) + # 영향력이 큰 점들에 레이블 붙이기 
    labs(title = "Moran’s index scatterplot (pH)", # 제목 설정 
         x = "Standardized pH", # x축 레이블 설정 
         y = "Spatially lagged pH") + # y축 레이블 설정 
    theme_bw() + # 테마 설정 
    theme(panel.grid= element_blank(),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 20),
          title = element_text(size=0),
          legend.text = element_text(size=20),
          legend.title=element_text(size=20),
          legend.position = "NA")+
    xlim(-4,5) + ylim(-2,3)+ annotate("text",x=2.8, y=2.7, label = "Moran's I = 0.348", size=10)
  pH.moran
}
#===============================================================================
#Local moran map - Visualization by administrative district (LISA calculation after averaging water quality factors by administrative district, k = 5), including Adult

    dat <- read.csv("for_local_moran.csv")
    library(spdep)
    
      dat$Year = as.numeric(dat$Year)
      
      dat$latitude = as.numeric(dat$latitude)
      dat$longitude = as.numeric(dat$longitude)
      ad = dat
    
    ad[c(10:17)][ad[c(10:17)]==0]<-0.000001
   
    str(ad)
    ad.net = ad[complete.cases(ad$longitude), ]
    
    #Adult
   
    # Assuming ad.net is a data frame
    ad.net.s <- ad.net[c(10:17)]
    ad.net.s = as.data.frame(ad.net.s)
    ad.net = cbind(ad.net[c(1:8)], ad.net.s)
    coords.ad <- cbind(ad.net$longitude, ad.net$latitude)
    summary(ad.net$pH) 
    head(ad.net)

    nb.ad <- knn2nb(knearneigh(coords.ad, k = 5))
    w.ad <- nb2listw (nb.ad, style = "minmax")
    
    lisa.pH <- localmoran(ad.net$pH, w.ad)
    lisa.pH = as.data.frame(lisa.pH)
    colnames(lisa.pH) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
    # High-High, Low-High, High-Low, Low-Low 구분
    lisa.pH$group <- NA # 새로운 열 생성
    lisa.pH$group[lisa.pH$Z.Ii > 0 & lisa.pH$Ii > 0] <- "High-High" # High-High 조건
    lisa.pH$group[lisa.pH$Z.Ii > 0 & lisa.pH$Ii < 0] <- "High-Low" # High-Low 조건
    lisa.pH$group[lisa.pH$Z.Ii < 0 & lisa.pH$Ii > 0] <- "Low-High" # Low-High 조건
    lisa.pH$group[lisa.pH$Z.Ii < 0 & lisa.pH$Ii < 0] <- "Low-Low" # Low-Low 조건
    head(lisa.pH)
    names(lisa.pH)[5] <- c("p.value")
    head(lisa.pH)
    
    lisa.NO3.N <- localmoran(ad.net$NO3.N, w.ad)
    lisa.NO3.N = as.data.frame(lisa.NO3.N)
    colnames(lisa.NO3.N) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
    # High-High, Low-High, High-Low, Low-Low 
    lisa.NO3.N$group <- NA
    lisa.NO3.N$group[lisa.NO3.N$Z.Ii > 0 & lisa.NO3.N$Ii > 0] <- "High-High" # High-High 
    lisa.NO3.N$group[lisa.NO3.N$Z.Ii > 0 & lisa.NO3.N$Ii < 0] <- "High-Low" # High-Low 
    lisa.NO3.N$group[lisa.NO3.N$Z.Ii < 0 & lisa.NO3.N$Ii > 0] <- "Low-High" # Low-High 
    lisa.NO3.N$group[lisa.NO3.N$Z.Ii < 0 & lisa.NO3.N$Ii < 0] <- "Low-Low" # Low-Low 
    head(lisa.NO3.N)
    names(lisa.NO3.N)[5] <- c("p.value")
    head(lisa.NO3.N)
    
    lisa.F <- localmoran(ad.net$F, w.ad)
    lisa.F = as.data.frame(lisa.F)
    colnames(lisa.F) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
    # High-High, Low-High, High-Low, Low-Low 
    lisa.F$group <- NA 
    lisa.F$group[lisa.F$Z.Ii > 0 & lisa.F$Ii > 0] <- "High-High" # High-High 
    lisa.F$group[lisa.F$Z.Ii > 0 & lisa.F$Ii < 0] <- "High-Low" # Low-Low 
    lisa.F$group[lisa.F$Z.Ii < 0 & lisa.F$Ii > 0] <- "Low-High" # High-Low 
    lisa.F$group[lisa.F$Z.Ii < 0 & lisa.F$Ii < 0] <- "Low-Low" # Low-High 
    head(lisa.F)
    names(lisa.F)[5] <- c("p.value")
    head(lisa.F)
    
    lisa.Al <- localmoran(ad.net$Al, w.ad)
    lisa.Al = as.data.frame(lisa.Al)
    colnames(lisa.Al) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
    # High-High, Low-High, High-Low, Low-Low 구분
    lisa.Al$group <- NA # 새로운 열 생성
    lisa.Al$group[lisa.Al$Z.Ii > 0 & lisa.Al$Ii > 0] <- "High-High" # High-High 조건
    lisa.Al$group[lisa.Al$Z.Ii > 0 & lisa.Al$Ii < 0] <- "High-Low" # Low-Low 조건
    lisa.Al$group[lisa.Al$Z.Ii < 0 & lisa.Al$Ii > 0] <- "Low-High" # High-Low 조건
    lisa.Al$group[lisa.Al$Z.Ii < 0 & lisa.Al$Ii < 0] <- "Low-Low" # Low-High 조건
    head(lisa.Al)
    names(lisa.Al)[5] <- c("p.value")
    head(lisa.Al)
    
    lisa.As <- localmoran(ad.net$As, w.ad)
    lisa.As = as.data.frame(lisa.As)
    colnames(lisa.As) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
    # High-High, Low-High, High-Low, Low-Low 구분
    lisa.As$group <- NA # 새로운 열 생성
    lisa.As$group[lisa.As$Z.Ii > 0 & lisa.As$Ii > 0] <- "High-High" # High-High 조건
    lisa.As$group[lisa.As$Z.Ii > 0 & lisa.As$Ii < 0] <- "High-Low" # Low-Low 조건
    lisa.As$group[lisa.As$Z.Ii < 0 & lisa.As$Ii > 0] <- "Low-High" # High-Low 조건
    lisa.As$group[lisa.As$Z.Ii < 0 & lisa.As$Ii < 0] <- "Low-Low" # Low-High 조건
    head(lisa.As)
    names(lisa.As)[5] <- c("p.vAsue")
    head(lisa.As)
    
    lisa.Mn <- localmoran(ad.net$Mn, w.ad)
    lisa.Mn = as.data.frame(lisa.Mn)
    colnames(lisa.Mn) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
    # High-High, Low-High, High-Low, Low-Low 구분
    lisa.Mn$group <- NA # 새로운 열 생성
    lisa.Mn$group[lisa.Mn$Z.Ii > 0 & lisa.Mn$Ii > 0] <- "High-High" # High-High 조건
    lisa.Mn$group[lisa.Mn$Z.Ii > 0 & lisa.Mn$Ii < 0] <- "High-Low" # Low-Low 조건
    lisa.Mn$group[lisa.Mn$Z.Ii < 0 & lisa.Mn$Ii > 0] <- "Low-High" # High-Low 조건
    lisa.Mn$group[lisa.Mn$Z.Ii < 0 & lisa.Mn$Ii < 0] <- "Low-Low" # Low-High 조건
    head(lisa.Mn)
    names(lisa.Mn)[5] <- c("p.value")
    head(lisa.Mn)
    
    lisa.Fe <- localmoran(ad.net$Fe, w.ad)
    lisa.Fe = as.data.frame(lisa.Fe)
    colnames(lisa.Fe) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
    # High-High, Low-High, High-Low, Low-Low 구분
    lisa.Fe$group <- NA # 새로운 열 생성
    lisa.Fe$group[lisa.Fe$Z.Ii > 0 & lisa.Fe$Ii > 0] <- "High-High" # High-High 조건
    lisa.Fe$group[lisa.Fe$Z.Ii > 0 & lisa.Fe$Ii < 0] <- "High-Low" # Low-Low 조건
    lisa.Fe$group[lisa.Fe$Z.Ii < 0 & lisa.Fe$Ii > 0] <- "Low-High" # High-Low 조건
    lisa.Fe$group[lisa.Fe$Z.Ii < 0 & lisa.Fe$Ii < 0] <- "Low-Low" # Low-High 조건
    head(lisa.Fe)
    names(lisa.Fe)[5] <- c("p.value")
    head(lisa.Fe)
    
    lisa.Turbidity <- localmoran(ad.net$Turbidity, w.ad)
    lisa.Turbidity = as.data.frame(lisa.Turbidity)
    colnames(lisa.Turbidity) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "Pr")
    # High-High, Low-High, High-Low, Low-Low 구분
    lisa.Turbidity$group <- NA # 새로운 열 생성
    lisa.Turbidity$group[lisa.Turbidity$Z.Ii > 0 & lisa.Turbidity$Ii > 0] <- "High-High" # High-High 조건
    lisa.Turbidity$group[lisa.Turbidity$Z.Ii > 0 & lisa.Turbidity$Ii < 0] <- "High-Low" # Low-Low 조건
    lisa.Turbidity$group[lisa.Turbidity$Z.Ii < 0 & lisa.Turbidity$Ii > 0] <- "Low-High" # High-Low 조건
    lisa.Turbidity$group[lisa.Turbidity$Z.Ii < 0 & lisa.Turbidity$Ii < 0] <- "Low-Low" # Low-High 조건
    head(lisa.Turbidity)
    names(lisa.Turbidity)[5] <- c("p.value")
    head(lisa.Turbidity)
    #=======================================
    ad.cn = cbind(ad.net[c(1:8)],
                  lisa.pH[5],lisa.pH[1],lisa.pH[4],lisa.pH[6],
                  lisa.NO3.N[5],lisa.NO3.N[1],lisa.NO3.N[4],lisa.NO3.N[6],
                  lisa.F[5],lisa.F[1],lisa.F[4],lisa.F[6],
                  lisa.Al[5],lisa.Al[1],lisa.Al[4],lisa.Al[6],
                  lisa.As[5],lisa.As[1],lisa.As[4],lisa.As[6],
                  
                  lisa.Mn[5],lisa.Mn[1],lisa.Mn[4],lisa.Mn[6],
                  lisa.Fe[5],lisa.Fe[1],lisa.Fe[4],lisa.Fe[6],
                  lisa.Turbidity[5],lisa.Turbidity[1],lisa.Turbidity[4],lisa.Turbidity[6])
    names(ad.cn)[9] <- c("pH.lisa.p")
    names(ad.cn)[10] <- c("pH.lisa.i")
    names(ad.cn)[11] <- c("pH.lisa.z")
    names(ad.cn)[12] <- c("pH.lisa.r")
    names(ad.cn)[13] <- c("NO3.N.lisa.p")
    names(ad.cn)[14] <- c("NO3.N.lisa.i")
    names(ad.cn)[15] <- c("NO3.N.lisa.z")
    names(ad.cn)[16] <- c("NO3.N.lisa.r")
    names(ad.cn)[17] <- c("F.lisa.p")
    names(ad.cn)[18] <- c("F.lisa.i")
    names(ad.cn)[19] <- c("F.lisa.z")
    names(ad.cn)[20] <- c("F.lisa.r")
    names(ad.cn)[21] <- c("Al.lisa.p")
    names(ad.cn)[22] <- c("Al.lisa.i")
    names(ad.cn)[23] <- c("Al.lisa.z")
    names(ad.cn)[24] <- c("Al.lisa.r")
    names(ad.cn)[25] <- c("As.lisa.p")
    names(ad.cn)[26] <- c("As.lisa.i")
    names(ad.cn)[27] <- c("As.lisa.z")
    names(ad.cn)[28] <- c("As.lisa.r")
    names(ad.cn)[29] <- c("Mn.lisa.p")
    names(ad.cn)[30] <- c("Mn.lisa.i")
    names(ad.cn)[31] <- c("Mn.lisa.z")
    names(ad.cn)[32] <- c("Mn.lisa.r")
    names(ad.cn)[33] <- c("Fe.lisa.p")
    names(ad.cn)[34] <- c("Fe.lisa.i")
    names(ad.cn)[35] <- c("Fe.lisa.z")
    names(ad.cn)[36] <- c("Fe.lisa.r")
    names(ad.cn)[37] <- c("Turbidity.lisa.p")
    names(ad.cn)[38] <- c("Turbidity.lisa.i")
    names(ad.cn)[39] <- c("Turbidity.lisa.z")
    names(ad.cn)[40] <- c("Turbidity.lisa.r")
  
  write.csv(ad.cn, "lisa.csv", na="-", fileEncoding = "CP949")
  
  
  # trials
  
  map_cn = st_read("LSMD_ADM_SECT_UMD_44.shp")                
  head(map_cn)
  class(map_cn)
  map_cn$EMD_NM <- iconv(map_cn$EMD_NM,
                         from = 'CP949',
                         to = 'UTF-8',
                         sub = NA,
                         mark = TRUE,
                         toRaw = FALSE)
  # Define the target CRS as an EPSG code
  to_crs <- "EPSG:4326"  # WGS 84
  
  # Transform the CRS using st_transform
  df_map_cn <- st_transform(map_cn, crs = to_crs)
  
  # fortify함수는 shp파일을 R의 데이터프레임으로 바꿔준다(좌표 변환 후 fortify하기) 
  df_map_cn = fortify(df_map_cn)   
  head(df_map_cn)
 
  # ggplot(data=df_map_cn, aes(x=long, y=lat, group=group))+
  #   geom_polygon(fill='white', color='black')
  
  
  
  df_map_cn1 = fortify(map_cn)
  head(map_cn)
  map_cn_info <- st_drop_geometry(map_cn)
  # Adding an ID column to df_map_cn starting from 0
  df_map_cn$id <- 0:(nrow(df_map_cn) - 1)
  
  head(map_cn_info)
  
  # 0부터 시작하는 연속적인 인덱스 값 만들기
  map_cn_info[, "id"] = (1:nrow(map_cn_info)) - 1
  # EMD_NM
  names(map_cn_info)[2] <- c("읍면동")
  head(map_cn_info)
  
  # df_map_cn$id 
  class(df_map_cn$id)
  
  class(map_cn_info$id)
  df_map_cn$id = as.numeric(df_map_cn$id)
  #==============================================
  sg_chung = read.csv("need.csv")
  
  head(sg_chung)
  str(sg_chung)
  dim(sg_chung)
  
  head(map_cn_info)
  
  d <-left_join(map_cn_info,sg_chung, by='읍면동')
  
  head(d)
  
  #left_join() 
  head(df_map_cn)
  head(d)
  c<-left_join(df_map_cn,d, by='id')  # left_join(x,y, by='')Data comes out differently depending on the x,y position order.
  #h<-merge(d,df_map_cn, by='id')
  head(c)
  # Check again if the geometry column is present
  head(d$geometry)#doesn't have geometry column
  head(c$geometry)#has geometry column
  
  #pH
  
  c <- st_as_sf(c,sf_column_name = "geometry")
  bbox <- st_bbox(c)
  # Calculate a padding value based on the range of your data
  pad_x <- (bbox$xmax - bbox$xmin) * 0.05
  pad_y <- (bbox$ymax - bbox$ymin) * 0.05
  

  # Plotting the map with space around the edges and an explicit border
  # Plotting the map with space around the edges and an explicit border
  gg <- ggplot(data = c) +
    geom_sf(aes(fill = as.factor(pH.lisa.p)), color = "black", size = 0.1) +  # Borders for each region
    scale_fill_manual(values = c("green4", "grey", "black"), na.value = "white",
                      labels = c("Significant", "Not Significant", "No Samples")) +
    theme_bw() +
    ylab("latitude") +
    xlab("longitude") +
    theme_minimal() +
    labs(fill = 'pH Level') +
    theme(panel.grid = element_blank(),
      axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 20, face = "bold"),
          legend.position = c(0.15, 0.18),
          legend.background = element_rect(fill = "white", colour = "black"),
          plot.margin = unit(c(10, 10, 10, 10), "points"),
          axis.ticks = element_line(color = "black", size = 0.5),
          axis.ticks.length = unit(1.3, "mm"), # Outward ticks
          axis.ticks.margin = unit(2, "mm")) +  # Spacing from axis text to ticks
    coord_sf(xlim = c(bbox["xmin"] - pad_x, bbox["xmax"] + pad_x), 
             ylim = c(bbox["ymin"] - pad_y, bbox["ymax"] + pad_y), expand = FALSE) +  # Set limits with padding
    guides(fill = guide_legend(title = "pH"))  # Set the title of the legend
  
  # Overlay the bounding box as a border
  gg <- gg + geom_rect(aes(xmin=bbox["xmin"] - pad_x, ymin=bbox["ymin"] - pad_y, xmax=bbox["xmax"] + pad_x, ymax=bbox["ymax"] + pad_y),
                       inherit.aes = FALSE, fill=NA, color="black", size=1)
  
  # Print the plot with the border and space around
  print(gg)
  
  
  # High-High는 local moran’s index와 z-score가 모두 양수
  # Low-Low는 local moran’s index와 z-score가 모두 음수
  # High-Low는 local moran’s index는 음수이고 z-score는 양수
  # Low-High는 local moran’s index는 양수이고 z-score는 음수
  
  # Create a new category for 'No samples'
  # Replace <CONDITION_FOR_NO_SAMPLES> with the actual condition to determine 'No samples'
  # Replace NA values in 'pH.lisa.r' with the label "No samples"
  c$pH.lisa.r <- ifelse(is.na(c$pH.lisa.r), "No samples", as.character(c$pH.lisa.r))
  
  # Convert 'pH.lisa.r' back to a factor with all levels you want to show in the legend
  c$pH.lisa.r <- factor(c$pH.lisa.r, levels = c("High-High", "Low-Low", "Not Significant", "No samples"))
  
  ggplot(data = c) + 
    geom_sf(aes(fill = pH.lisa.r), color = 'black') +
    theme_bw() +
    ylab("latitude") +
    xlab("longitude") +
    theme(
      panel.grid = element_blank(),
      axis.title = element_text(size = 15),
      axis.text = element_text(size = 15),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 20, face = "bold"), # Set legend title to bold
      legend.position = c(0.17,0.25),
      legend.background = element_rect(fill = "white", colour = "black") # Add a box around the legend
    ) +
    guides(
      fill = guide_legend(title = "pH") # Set the title of the legend
    ) +
    scale_fill_manual(
      values = c("High-High" = "red2", "Low-Low" = "blue", "Not Significant" = "#c0c0c0", "No samples" = "#909090"),
      labels = c("High-High", "Low-Low", "Not Significant", "No samples")
    )
  
  
  #================================
  #NO3.N
 
  gg <- ggplot(data = c) +
    geom_sf(aes(fill = as.factor(NO3.N.lisa.p)), color = "black", size = 0.1) +  # Borders for each region
    scale_fill_manual(values = c("green4", "grey", "black"), na.value = "white",
                      labels = c("Significant", "Not Significant", "No Samples")) +
    theme_bw() +
    ylab("latitude") +
    xlab("longitude") +
    theme_minimal() +
    labs(fill = 'NO3.N Level') +
    theme(
      panel.grid = element_blank(),
      axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 20, face = "bold"),
          legend.position = c(0.15, 0.18),
          legend.background = element_rect(fill = "white", colour = "black"),
          plot.margin = unit(c(10, 10, 10, 10), "points"),
          axis.ticks = element_line(color = "black", size = 0.5),
          axis.ticks.length = unit(1.3, "mm"), # Outward ticks
          axis.ticks.margin = unit(2, "mm")) +  # Spacing from axis text to ticks
    coord_sf(xlim = c(bbox["xmin"] - pad_x, bbox["xmax"] + pad_x), 
             ylim = c(bbox["ymin"] - pad_y, bbox["ymax"] + pad_y), expand = FALSE) +  # Set limits with padding
    guides(
      fill = guide_legend(
        title = expression(NO[3]*"-N")))
  # Overlay the bounding box as a border
  gg <- gg + geom_rect(aes(xmin=bbox["xmin"] - pad_x, ymin=bbox["ymin"] - pad_y, xmax=bbox["xmax"] + pad_x, ymax=bbox["ymax"] + pad_y),
                       inherit.aes = FALSE, fill=NA, color="black", size=1)
  
  # Print the plot with the border and space around
  print(gg)
  
  
  
  
  # High-High는 local moran’s index와 z-score가 모두 양수
  # Low-Low는 local moran’s index와 z-score가 모두 음수
  # High-Low는 local moran’s index는 음수이고 z-score는 양수
  # Low-High는 local moran’s index는 양수이고 z-score는 음수
  # Replace NA values in 'pH.lisa.r' with the label "No samples"
  c$NO3.N.lisa.r <- ifelse(is.na(c$NO3.N.lisa.r), "No samples", as.character(c$NO3.N.lisa.r))
  # Convert 'pH.lisa.r' back to a factor with all levels you want to show in the legend
  c$NO3.N.lisa.r <- factor(c$NO3.N.lisa.r, levels = c("High-High", "Low-Low", "Not Significant", "No samples"))
  
  ggplot(data = c) + 
    geom_sf(aes(fill = NO3.N.lisa.r), color = 'black') +
    theme_bw() +
    ylab("latitude") +
    xlab("longitude") +
    theme(
      panel.grid = element_blank(),
      axis.title = element_text(size = 15),
      axis.text = element_text(size = 15),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 20, face = "bold"), # Set legend title to bold
      legend.position = c(0.17,0.25),
      legend.background = element_rect(fill = "white", colour = "black") # Add a box around the legend
    ) +
    guides(
      fill = guide_legend(
        title = expression(NO[3]*"-N") # Using expression() for mathematical notation
      )
    ) +
    scale_fill_manual(
      values = c("High-High" = "red2", "Low-Low" = "blue", "Not Significant" = "#c0c0c0", "No samples" = "#909090"),
      labels = c("High-High", "Low-Low", "Not Significant", "No samples")
    )
  #================================
  #F
  gg <- ggplot(data = c) +
    geom_sf(aes(fill = as.factor(F.lisa.p)), color = "black", size = 0.1) +  # Borders for each region
    scale_fill_manual(values = c("green4", "grey", "black"), na.value = "white",
                      labels = c("Significant", "Not Significant", "No Samples")) +
    theme_bw() +
    ylab("latitude") +
    xlab("longitude") +
    theme_minimal() +
    labs(fill = 'F Level') +
    theme( panel.grid = element_blank(),
      axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 20, face = "bold"),
          legend.position = c(0.15, 0.18),
          legend.background = element_rect(fill = "white", colour = "black"),
          plot.margin = unit(c(10, 10, 10, 10), "points"),
          axis.ticks = element_line(color = "black", size = 0.5),
          axis.ticks.length = unit(1.3, "mm"), # Outward ticks
          axis.ticks.margin = unit(2, "mm")) +  # Spacing from axis text to ticks
    coord_sf(xlim = c(bbox["xmin"] - pad_x, bbox["xmax"] + pad_x), 
             ylim = c(bbox["ymin"] - pad_y, bbox["ymax"] + pad_y), expand = FALSE) +  # Set limits with padding
    guides(fill = guide_legend(title = "F"))  # Set the title of the legend
  
  # Overlay the bounding box as a border
  gg <- gg + geom_rect(aes(xmin=bbox["xmin"] - pad_x, ymin=bbox["ymin"] - pad_y, xmax=bbox["xmax"] + pad_x, ymax=bbox["ymax"] + pad_y),
                       inherit.aes = FALSE, fill=NA, color="black", size=1)
  
  # Print the plot with the border and space around
  print(gg)
  
  # High-High는 local moran’s index와 z-score가 모두 양수
  # Low-Low는 local moran’s index와 z-score가 모두 음수
  # High-Low는 local moran’s index는 음수이고 z-score는 양수
  # Low-High는 local moran’s index는 양수이고 z-score는 음수
  
  # Replace NA values in 'pH.lisa.r' with the label "No samples"
  c$F.lisa.r <- ifelse(is.na(c$F.lisa.r), "No samples", as.character(c$F.lisa.r))
  # Convert 'pH.lisa.r' back to a factor with all levels you want to show in the legend
  c$F.lisa.r <- factor(c$F.lisa.r, levels = c("High-High", "Low-Low", "Not Significant", "No samples"))
  
  ggplot(data = c) + 
    geom_sf(aes(fill = F.lisa.r), color = 'black') +
    theme_bw() +
    ylab("latitude") +
    xlab("longitude") +
    theme(
      panel.grid = element_blank(),
      axis.title = element_text(size = 15),
      axis.text = element_text(size = 15),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 20, face = "bold"), # Set legend title to bold
      legend.position = c(0.17,0.25),
      legend.background = element_rect(fill = "white", colour = "black") # Add a box around the legend
    ) +
    guides(
      fill = guide_legend(title = "F") # Set the title of the legend
    ) +
    scale_fill_manual(
      values = c("High-High" = "red2", "Low-Low" = "blue", "Not Significant" = "#c0c0c0", "No samples" = "#909090"),
      labels = c("High-High", "Low-Low", "Not Significant", "No samples")
    )
  
  #================================
  #Al
  gg <- ggplot(data = c) +
    geom_sf(aes(fill = as.factor(Al.lisa.p)), color = "black", size = 0.1) +  # Borders for each region
    scale_fill_manual(values = c("green4", "grey", "black"), na.value = "white",
                      labels = c("Significant", "Not Significant", "No Samples")) +
    theme_bw() +
    ylab("latitude") +
    xlab("longitude") +
    theme_minimal() +
    labs(fill = 'Al Level') +
    theme( panel.grid = element_blank(),
           axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 20, face = "bold"),
          legend.position = c(0.15, 0.18),
          legend.background = element_rect(fill = "white", colour = "black"),
          plot.margin = unit(c(10, 10, 10, 10), "points"),
          axis.ticks = element_line(color = "black", size = 0.5),
          axis.ticks.length = unit(1.3, "mm"), # Outward ticks
          axis.ticks.margin = unit(2, "mm")) +  # Spacing from axis text to ticks
    coord_sf(xlim = c(bbox["xmin"] - pad_x, bbox["xmax"] + pad_x), 
             ylim = c(bbox["ymin"] - pad_y, bbox["ymax"] + pad_y), expand = FALSE) +  # Set limits with padding
    guides(fill = guide_legend(title = "Al"))  # Set the title of the legend
  
  # Overlay the bounding box as a border
  gg <- gg + geom_rect(aes(xmin=bbox["xmin"] - pad_x, ymin=bbox["ymin"] - pad_y, xmax=bbox["xmax"] + pad_x, ymax=bbox["ymax"] + pad_y),
                       inherit.aes = FALSE, fill=NA, color="black", size=1)
  
  # Print the plot with the border and space around
  print(gg)
   # High-High는 local moran’s index와 z-score가 모두 양수
  # Low-Low는 local moran’s index와 z-score가 모두 음수
  # High-Low는 local moran’s index는 음수이고 z-score는 양수
  # Low-High는 local moran’s index는 양수이고 z-score는 음수
  
  # Replace NA values in 'pH.lisa.r' with the label "No samples"
  c$Al.lisa.r <- ifelse(is.na(c$Al.lisa.r), "No samples", as.character(c$Al.lisa.r))
  # Convert 'pH.lisa.r' back to a factor with all levels you want to show in the legend
  c$Al.lisa.r <- factor(c$Al.lisa.r, levels = c("High-High", "Low-Low", "Not Significant", "No samples"))
  
  ggplot(data = c) + 
    geom_sf(aes(fill = Al.lisa.r), color = 'black') +
    theme_bw() +
    ylab("latitude") +
    xlab("longitude") +
    theme(
      panel.grid = element_blank(),
      axis.title = element_text(size = 15),
      axis.text = element_text(size = 15),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 20, face = "bold"), # Set legend title to bold
      legend.position = c(0.17,0.25),
      legend.background = element_rect(fill = "white", colour = "black") # Add a box around the legend
    ) +
    guides(
      fill = guide_legend(title = "") # Set the title of the legend
    ) +
    scale_fill_manual(
      values = c( "Low-Low" = "blue", "Not Significant" = "#c0c0c0", "No samples" = "#909090"),
      labels = c("Low-Low", "Not Significant", "No samples")
    )
  
  #As
  gg <- ggplot(data = c) +
    geom_sf(aes(fill = as.factor(As.lisa.p)), color = "black", size = 0.1) +  # Borders for each region
    scale_fill_manual(values = c("green4", "grey", "black"), na.value = "white",
                      labels = c("Significant", "Not Significant", "No Samples")) +
    theme_bw() +
    ylab("latitude") +
    xlab("longitude") +
    theme_minimal() +
    labs(fill = 'As Level') +
    theme( panel.grid = element_blank(),
           axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 20, face = "bold"),
          legend.position = c(0.15, 0.18),
          legend.background = element_rect(fill = "white", colour = "black"),
          plot.margin = unit(c(10, 10, 10, 10), "points"),
          axis.ticks = element_line(color = "black", size = 0.5),
          axis.ticks.length = unit(1.3, "mm"), # Outward ticks
          axis.ticks.margin = unit(2, "mm")) +  # Spacing from axis text to ticks
    coord_sf(xlim = c(bbox["xmin"] - pad_x, bbox["xmax"] + pad_x), 
             ylim = c(bbox["ymin"] - pad_y, bbox["ymax"] + pad_y), expand = FALSE) +  # Set limits with padding
    guides(fill = guide_legend(title = "As"))  # Set the title of the legend
  
  # Overlay the bounding box as a border
  gg <- gg + geom_rect(aes(xmin=bbox["xmin"] - pad_x, ymin=bbox["ymin"] - pad_y, xmax=bbox["xmax"] + pad_x, ymax=bbox["ymax"] + pad_y),
                       inherit.aes = FALSE, fill=NA, color="black", size=1)
  
  # Print the plot with the border and space around
  print(gg)
  
  # High-High는 local moran’s index와 z-score가 모두 양수
  # Low-Low는 local moran’s index와 z-score가 모두 음수
  # High-Low는 local moran’s index는 음수이고 z-score는 양수
  # Low-High는 local moran’s index는 양수이고 z-score는 음수
  
  # Replace NA values in 'pH.lisa.r' with the label "No samples"
  c$As.lisa.r <- ifelse(is.na(c$As.lisa.r), "No samples", as.character(c$As.lisa.r))
  # Convert 'pH.lisa.r' back to a factor with all levels you want to show in the legend
  c$As.lisa.r <- factor(c$As.lisa.r, levels = c("High-High", "Low-Low", "Not Significant", "No samples"))
  
  ggplot(data = c) + 
    geom_sf(aes(fill = As.lisa.r), color = 'black') +
    theme_bw() +
    ylab("latitude") +
    xlab("longitude") +
    theme(
      panel.grid = element_blank(),
      axis.title = element_text(size = 15),
      axis.text = element_text(size = 15),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 20, face = "bold"), # Set legend title to bold
      legend.position = c(0.17,0.25),
      legend.background = element_rect(fill = "white", colour = "black") # Add a box around the legend
    ) +
    guides(
      fill = guide_legend(title = "As") # Set the title of the legend
    ) +
    scale_fill_manual(
      values = c("High-High" = "red2", "Low-Low" = "blue", "Not Significant" = "#c0c0c0", "No samples" = "#909090"),
      labels = c("High-High", "Low-Low", "Not Significant", "No samples")
    )
  
  
  #Mn
  gg <- ggplot(data = c) +
    geom_sf(aes(fill = as.factor(Mn.lisa.p)), color = "black", size = 0.1) +  # Borders for each region
    scale_fill_manual(values = c("green4", "grey", "black"), na.value = "white",
                      labels = c("Significant", "Not Significant", "No Samples")) +
    theme_bw() +
    ylab("latitude") +
    xlab("longitude") +
    theme_minimal() +
    labs(fill = 'Mn Level') +
    theme( panel.grid = element_blank(),
      axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 20, face = "bold"),
          legend.position = c(0.15, 0.18),
          legend.background = element_rect(fill = "white", colour = "black"),
          plot.margin = unit(c(10, 10, 10, 10), "points"),
          axis.ticks = element_line(color = "black", size = 0.5),
          axis.ticks.length = unit(1.3, "mm"), # Outward ticks
          axis.ticks.margin = unit(2, "mm")) +  # Spacing from axis text to ticks
    coord_sf(xlim = c(bbox["xmin"] - pad_x, bbox["xmax"] + pad_x), 
             ylim = c(bbox["ymin"] - pad_y, bbox["ymax"] + pad_y), expand = FALSE) +  # Set limits with padding
    guides(fill = guide_legend(title = "Mn"))  # Set the title of the legend
  
  # Overlay the bounding box as a border
  gg <- gg + geom_rect(aes(xmin=bbox["xmin"] - pad_x, ymin=bbox["ymin"] - pad_y, xmax=bbox["xmax"] + pad_x, ymax=bbox["ymax"] + pad_y),
                       inherit.aes = FALSE, fill=NA, color="black", size=1)
  
  # Print the plot with the border and space around
  print(gg)
  # High-High는 local moran’s index와 z-score가 모두 양수
  # Low-Low는 local moran’s index와 z-score가 모두 음수
  # High-Low는 local moran’s index는 음수이고 z-score는 양수
  # Low-High는 local moran’s index는 양수이고 z-score는 음수
  # Replace NA values in 'pH.lisa.r' with the label "No samples"
  c$Mn.lisa.r <- ifelse(is.na(c$Mn.lisa.r), "No samples", as.character(c$Mn.lisa.r))
  # Convert 'pH.lisa.r' back to a factor with all levels you want to show in the legend
  c$Mn.lisa.r <- factor(c$Mn.lisa.r, levels = c("High-High", "Low-Low", "Not Significant", "No samples"))
  
  ggplot(data = c) + 
    geom_sf(aes(fill = Mn.lisa.r), color = 'black') +
    theme_bw() +
    ylab("latitude") +
    xlab("longitude") +
    theme(
      panel.grid = element_blank(),
      axis.title = element_text(size = 15),
      axis.text = element_text(size = 15),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 20, face = "bold"), # Set legend title to bold
      legend.position = c(0.17,0.25),
      legend.background = element_rect(fill = "white", colour = "black") # Add a box around the legend
    ) +
    guides(
      fill = guide_legend(title = "Mn") # Set the title of the legend
    ) +
    scale_fill_manual(
      values = c("High-High" = "red2", "Low-Low" = "blue", "Not Significant" = "#c0c0c0", "No samples" = "#909090"),
      labels = c("High-High", "Low-Low", "Not Significant", "No samples")
    )
  
  
  
  #Fe
  gg <- ggplot(data = c) +
    geom_sf(aes(fill = as.factor(Fe.lisa.p)), color = "black", size = 0.1) +  # Borders for each region
    scale_fill_manual(values = c("green4", "grey", "black"), na.value = "white",
                      labels = c("Significant", "Not Significant", "No Samples")) +
    theme_bw() +
    ylab("latitude") +
    xlab("longitude") +
    theme_minimal() +
    labs(fill = 'Fe Level') +
    theme( panel.grid = element_blank(),
           axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 20, face = "bold"),
          legend.position = c(0.15, 0.18),
          legend.background = element_rect(fill = "white", colour = "black"),
          plot.margin = unit(c(10, 10, 10, 10), "points"),
          axis.ticks = element_line(color = "black", size = 0.5),
          axis.ticks.length = unit(1.3, "mm"), # Outward ticks
          axis.ticks.margin = unit(2, "mm")) +  # Spacing from axis text to ticks
    coord_sf(xlim = c(bbox["xmin"] - pad_x, bbox["xmax"] + pad_x), 
             ylim = c(bbox["ymin"] - pad_y, bbox["ymax"] + pad_y), expand = FALSE) +  # Set limits with padding
    guides(fill = guide_legend(title = "Fe"))  # Set the title of the legend
  
  # Overlay the bounding box as a border
  gg <- gg + geom_rect(aes(xmin=bbox["xmin"] - pad_x, ymin=bbox["ymin"] - pad_y, xmax=bbox["xmax"] + pad_x, ymax=bbox["ymax"] + pad_y),
                       inherit.aes = FALSE, fill=NA, color="black", size=1)
  
  
  # Print the plot with the border and space around
  print(gg)
  # High-High는 local moran’s index와 z-score가 모두 양수
  # Low-Low는 local moran’s index와 z-score가 모두 음수
  # High-Low는 local moran’s index는 음수이고 z-score는 양수
  # Low-High는 local moran’s index는 양수이고 z-score는 음수
  # Replace NA values in 'pH.lisa.r' with the label "No samples"
  c$Fe.lisa.r <- ifelse(is.na(c$Fe.lisa.r), "No samples", as.character(c$Fe.lisa.r))
  # Convert 'pH.lisa.r' back to a factor with all levels you want to show in the legend
  c$Fe.lisa.r <- factor(c$Fe.lisa.r, levels = c("High-High", "Low-Low", "Not Significant", "No samples"))
  
  ggplot(data = c) + 
    geom_sf(aes(fill = Fe.lisa.r), color = 'black') +
    theme_bw() +
    ylab("latitude") +
    xlab("longitude") +
    theme(
      panel.grid = element_blank(),
      axis.title = element_text(size = 15),
      axis.text = element_text(size = 15),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 20, face = "bold"), # Set legend title to bold
      legend.position = c(0.17,0.25),
      legend.background = element_rect(fill = "white", colour = "black") # Add a box around the legend
    ) +
    guides(
      fill = guide_legend(title = "Fe") # Set the title of the legend
    ) +
    scale_fill_manual(
      values = c("High-High" = "red2", "Low-Low" = "blue", "Not Significant" = "#c0c0c0", "No samples" = "#909090"),
      labels = c("High-High", "Low-Low", "Not Significant", "No samples")
    )
  
  
  
  #Turbidity
  gg <- ggplot(data = c) +
    geom_sf(aes(fill = as.factor(Turbidity.lisa.p)), color = "black", size = 0.1) +  # Borders for each region
    scale_fill_manual(values = c("green4", "grey", "black"), na.value = "white",
                      labels = c("Significant", "Not Significant", "No Samples")) +
    theme_bw() +
    ylab("latitude") +
    xlab("longitude") +
    theme_minimal() +
    labs(fill = 'Turbidity Level') +
    theme( panel.grid = element_blank(),
           axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 20, face = "bold"),
          legend.position = c(0.15, 0.18),
          legend.background = element_rect(fill = "white", colour = "black"),
          plot.margin = unit(c(10, 10, 10, 10), "points"),
          axis.ticks = element_line(color = "black", size = 0.5),
          axis.ticks.length = unit(1.3, "mm"), # Outward ticks
          axis.ticks.margin = unit(2, "mm")) +  # Spacing from axis text to ticks
    coord_sf(xlim = c(bbox["xmin"] - pad_x, bbox["xmax"] + pad_x), 
             ylim = c(bbox["ymin"] - pad_y, bbox["ymax"] + pad_y), expand = FALSE) +  # Set limits with padding
    guides(fill = guide_legend(title = "Turbidity"))  # Set the title of the legend
  
  # Overlay the bounding box as a border
  gg <- gg + geom_rect(aes(xmin=bbox["xmin"] - pad_x, ymin=bbox["ymin"] - pad_y, xmax=bbox["xmax"] + pad_x, ymax=bbox["ymax"] + pad_y),
                       inherit.aes = FALSE, fill=NA, color="black", size=1)
  
  
  # Print the plot with the border and space around
  print(gg)
  # High-High는 local moran’s index와 z-score가 모두 양수
  # Low-Low는 local moran’s index와 z-score가 모두 음수
  # High-Low는 local moran’s index는 음수이고 z-score는 양수
  # Low-High는 local moran’s index는 양수이고 z-score는 음수
  # Replace NA values in 'pH.lisa.r' with the label "No samples"
  c$Turbidity.lisa.r <- ifelse(is.na(c$Turbidity.lisa.r), "No samples", as.character(c$Turbidity.lisa.r))
  # Convert 'pH.lisa.r' back to a factor with all levels you want to show in the legend
  c$Turbidity.lisa.r <- factor(c$Turbidity.lisa.r, levels = c("High-High", "Low-Low", "Not Significant", "No samples"))
  
  ggplot(data = c) + 
    geom_sf(aes(fill = Turbidity.lisa.r), color = 'black') +
    theme_bw() +
    ylab("latitude") +
    xlab("longitude") +
    theme(
      panel.grid = element_blank(),
      axis.title = element_text(size = 15),
      axis.text = element_text(size = 15),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 20, face = "bold"), # Set legend title to bold
      legend.position = c(0.17,0.25),
      legend.background = element_rect(fill = "white", colour = "black") # Add a box around the legend
    ) +
    guides(
      fill = guide_legend(title = "Turbidity") # Set the title of the legend
    ) +
    scale_fill_manual(
      values = c("High-High" = "red2", "Low-Low" = "blue", "Not Significant" = "#c0c0c0", "No samples" = "#909090"),
      labels = c("High-High", "Low-Low", "Not Significant", "No samples")
    )























