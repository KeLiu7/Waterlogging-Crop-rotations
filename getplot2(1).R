# Set work path
{
  usrname <- Sys.info()[7]

  if (Sys.info()[1] == "Windows") {
    dir_root <- "Z:/"
    dir_work <- "Z:/huhu/project/p9/"
  } else if (Sys.info()[1] == "Linux") {
    dir_root <- stringr::str_glue("/u/{Sys.info()[7]}/")
    dir_work <- stringr::str_glue("/u/{Sys.info()[7]}/huhu/project/p9/")
  }
  
  options(width = 150)
  
  libs <- c("data.table", "stringr", "magrittr")
  sapply(libs, require, character.on = T)
}

# Load config file
config <- data.table::fread(stringr::str_glue("{dir_root}database/geography/map of Australia/Australia_wheatbelt_lonlat.csv")) %>% 
  .[, State := stringr::str_squish(stringr::str_split_i(location, ",", -2L))] %>% 
  .[! State %in% c("Australian Capital Territory", "Queensland", "Tasmania")] %>% 
  .[, location := NULL]
head(config)

require(ggplot2)
map <- sf::st_read(stringr::str_glue("{dir_root}database/geography/map of Australia/aagis_asgs16v1_g5a.shp"))

# 1 Distribution map of rainfall and KS, meteorological stations and soil stations in wheat cropping belt from 2000 to 2024 --------------------------
# Load the function to extract the site and latitude and longitude information of Australia wheatbelt.
path_mets <- dir(path = stringr::str_glue("{dir_root}database/met/Australia wheatbelt/WG_APSIM_ACC2"), full.names = T) %>%
  .[grep("SSP245", .)]
head(path_mets)

path_met <- path_mets[1]
met_lonlat <- purrr::map_dfr(.x = path_mets, .f = lonlat_extract_of_wheatbelt)
tail(met_lonlat)
# data.table::fwrite(met_lonlat, stringr::str_glue("{dir_root}database/met/Australia wheatbelt/site_lon_lat.csv"))
met_lonlat <- data.table::fread(stringr::str_glue("{dir_root}/database/met/Australia wheatbelt/site_lon_lat.csv"))
head(met_lonlat)

# 1.2 Extract the nearest weather station
data <- data.table::fread(stringr::str_glue("{dir_work}1.data/wheatbelt_lonlat.csv")) %>%
  .[, yield := NULL]
head(data)

df1_site <- data[1]
data_path <- purrr::map_dfr(.x = split.data.frame(x = data, f = ~site), .f = nearlest_lonlat_extract_of_wheatbelt, met_lonlat = met_lonlat)
head(data_path)

# 1.3 Annual average cumulative rainfall
map <- sf::st_read(stringr::str_glue("{dir_root}database/geography/map of Australia/aagis_asgs16v1_g5a.shp"))

## Read in latitude and longitude information of wheatbelt (568 grids)
data <- data.table::fread(stringr::str_glue("{dir_root}database/geography/map of Australia/Australia_wheatbelt_lonlat.csv"))
head(data)

year_start <- 1961
year_end <- 2023
rain <- purrr::map_dfr(.x = split.data.frame(x = data, f = ~site), .f = mean_rain_wheatbelt, year_start, year_end)
rain$rain <- round(rain$rain, 2)
head(rain)
range(rain$rain)
# data.table::fwrite(rain, stringr::str_glue("{dir_work}wheatbelt{year_start}-{year_end}年平均累积降雨量.csv"))

# 1.4 plot
rain <- data.table::fread(stringr::str_glue("{dir_work}wheatbelt{year_start}-{year_end}_annual average cumulative rainfall.csv")) %>%
  .[rain >= 100 & rain <= 200, rain1 := "100-200"] %>%
  .[rain > 200 & rain <= 300, rain1 := "201-300"] %>%
  .[rain > 300 & rain <= 400, rain1 := "301-400"] %>%
  .[rain > 400 & rain <= 600, rain1 := "401-600"] %>%
  .[rain > 600 & rain <= 800, rain1 := "601-800"] %>% 
  .[rain > 800 & rain <= 1000, rain1 := "801-1000"] %>% 
  .[rain > 1000 & rain <= 1200, rain1 := "1001-1200"]
head(rain)
range(rain$rain)
unique(rain$rain1)
data_plot <- sp_sf(data = rain, longitude = "long", latitude = "lat", index = "rain1", name = "Rain (mm)")
data_plot
data_plot$`Rain (mm)` <- factor(data_plot$`Rain (mm)`, levels = c("100-200", "201-300", "301-400", "401-600", "601-800", "801-1000", "1001-1200"))

require(ggplot2)
# cairo_pdf(file = stringr::str_glue("{dir_work}wheatbelt{year_start}-{year_end}Annual average cumulative rainfall_SILO1.pdf"), width = 12, height = 10, family = "Calibri")
ggplot() +
  geom_sf(data = data_plot, mapping = aes(fill = `Rain (mm)`), color = "grey", size = 4.7, shape = 22) +
  geom_sf(data = map, fill = "transparent", linewidth = 1) +
  # linear scale
  ggspatial::annotation_scale(location = "bl", text_cex = 2, pad_x = unit(2.3, "cm"), pad_y = unit(5, "cm")) +
  # compass
  ggspatial::annotation_north_arrow(location = "tr", pad_x = unit(3.2, "cm"), pad_y = unit(2.6, "cm"), which_north = "false", style = ggspatial::north_arrow_orienteering(text_size = 25, line_width = 2)) +
  scale_fill_manual(values = viridis::viridis(n = 7, alpha = 1, begin = 0, end = 1, direction = 1, option = "A") %>% rev()) +
  guides(fill = guide_legend(override.aes = list(size = 10), ncol = 3)) +
  theme_void() +
  theme(plot.title = element_text(size = 30), plot.subtitle = element_text(size = 30), legend.title = element_text(size = 25), legend.text = element_text(size = 20), legend.position = c(0.4, 0.11), legend.direction = "horizontal", legend.title.position = "top", plot.margin = margin(t = .1, r = .1, b = .1, l = .1, unit = "cm"), legend.key.width = unit(1.5, "cm"))

dev.off()

# 1.5 Extracting the information of the nearest meteorological station and soil station
# Extract the latest meteorological and soil station information of wheatbelt and make APSIM met file.
data <- data.table::fread(stringr::str_glue("{dir_root}database/geography/map of Australia/Australia_wheatbelt_lonlat.csv"))
head(data)

met_station_info <- data.table::fread(stringr::str_glue("{dir_root}database/met/Australia/All/Historical_met/station_information.csv"))
head(met_station_info)

soil_station_info <- data.table::fread(stringr::str_glue("{dir_root}database/geography/Australia Soil/Apsoil/Apsoil.csv")) %>% 
  .[, .(station, longitude, latitude)] %>% 
  unique() %>% 
  .[! (longitude > 160 | longitude < 100)]
head(soil_station_info)

## Extract meteorological and soil file information from wheatbelt, Australia
res <- purrr::map_dfr(.x = split.data.frame(x = data, f = ~site), .f = nearlest_met_soil_station_extract_of_wheatbelt, met_station_info, soil_station_info, dir_work, dir_root)
head(res)
# data.table::fwrite(res, stringr::str_glue("{dir_root}database/澳大利亚wheatbelt站点_site_met_soil_station.csv"))

## 1.6 KS
map <- sf::st_read(stringr::str_glue("{dir_root}database/geography/map of Australia/aagis_asgs16v1_g5a.shp"))

### SLGA KS
#### 0-30cm KS
config <- data.table::fread(stringr::str_glue("{dir_root}database/geography/map of Australia/Australia_wheatbelt_lonlat.csv"))
head(config)

xmls <- dir(path = stringr::str_glue("{dir_work}1.data/soil/"), pattern = "xml$", full.names = T)

xml <- xmls[120]
res <- purrr::map_dfr(.x = xmls, .f = function(xml, config){
  single <- read.delim2(file = xml, header = F) %>%
    data.table::as.data.table() %>%
    .[]
  row_KS <- grep(pattern = "KS", x = single$V1) %>% .[1]
res <- config[site %in% stringr::str_split_i(basename(xml), "\\.", 1)] %>%
  .[, c("0-30cm", "location") := list(mean(as.numeric(stringr::str_remove(stringr::str_squish(single$V1[c(row_KS + 1, row_KS + 2)]), ","))), NULL)]
}, config)
# fwrite(res, stringr::str_glue("{dir_work}res/wheatbelt_SLGA_KS.csv"))

res <- purrr::map_dfr(.x = xmls, .f = function(xml, config){
  single <- read.delim2(file = xml, header = F) %>%
    data.table::as.data.table() %>%
    .[]
  row_KS <- grep(pattern = "KS", x = single$V1) %>% .[1]
  row_BDmeta <- grep(pattern = "BDMetadata", x = single$V1)
  row_thick <- grep(pattern = "Thickness", x = single$V1) %>% .[1]
  
  thick <- as.numeric(stringr::str_remove(stringr::str_squish(single$V1[c((row_thick + 1) : (row_thick + (row_BDmeta - row_KS - 2)))]), ","))
  KS <- as.numeric(stringr::str_remove(stringr::str_squish(single$V1[c((row_KS + 1) : (row_BDmeta -2))]), ","))
  
  res <- data.table::data.table(Thickness = thick, KS = KS)
  res$site <-  config[site %in% stringr::str_split_i(basename(xml), "\\.", 1)]$site
  res$lon <-  config[site %in% stringr::str_split_i(basename(xml), "\\.", 1)]$lon
  res$lat <-  config[site %in% stringr::str_split_i(basename(xml), "\\.", 1)]$lat
  res$location <-  config[site %in% stringr::str_split_i(basename(xml), "\\.", 1)]$location
  res[, `Depth (cm)` := cumsum(Thickness) / 10, by = .(site, lon, lat, location)]
  
  data.table::setcolorder(res, c("site", "lon", "lat", "location", "Thickness", "Depth (cm)", "KS"))
  
  return(res)
}, config)
# fwrite(res, stringr::str_glue("{dir_work}res/wheatbelt_SLGA_KS_all.csv"))

KS_plot <- data.table::fread(stringr::str_glue("{dir_work}res/wheatbelt_SLGA_KS.csv")) %>% 
  sp2sf(longitude = "lon", latitude = "lat", index = "0-30cm", name = "KS (mm/day)")
KS_plot
# cairo_pdf(file = stringr::str_glue("{dir_work}wheatbelt_KS_SLGA_distribution.pdf"), width = 12, height = 10, family = "Calibri", onefile = T)
ggplot() +
  geom_sf(data = KS_plot, mapping = aes(fill = `KS (mm/day)`), color = "grey", size = 4.7, shape = 22) +
  geom_sf(data = map, fill = "transparent", linewidth = 1) +
  # linear scale
  ggspatial::annotation_scale(location = "bl", text_cex = 2.5, pad_x = unit(2.3, "cm"), pad_y = unit(4.5, "cm")) +
  # compass
  ggspatial::annotation_north_arrow(location = "tr", pad_x = unit(3.2, "cm"), pad_y = unit(2.6, "cm"), which_north = "false", style = ggspatial::north_arrow_orienteering(text_size = 20, line_width = 1.5)) +
  scale_fill_gradientn(limit = range, colours = viridis::viridis(n = 10, alpha = 1, begin = 0, end = 1, direction = 1, option = "A") %>% rev()) +
  labs(fill = "KS 0-30cm (mm/day)")+
  theme_void() +
  theme(plot.title = element_text(size = 30), plot.subtitle = element_text(size = 30), legend.title = element_text(size = 30), legend.text = element_text(size = 25), legend.position = c(0.32, 0.11), legend.direction = "horizontal", legend.title.position = "top", plot.margin = margin(t = .1, r = .1, b = .1, l = .1, unit = "cm"), legend.key.width = unit(2.5, "cm"), legend.key.height = unit(0.3, "cm"))

dev.off()

## 1.7 met station and soil site
### met station
met_station <- data.table::fread(stringr::str_glue("{dir_root}database/met/Australia/Australia_wheatbelt/Historical_met/wheatbelt_station_info.csv")) %>%  
  sp_sf(longitude = "lon", latitude = "lat", index = "station_num", name = "site")
met_station$group <- "SILO met station (5836)"
head(met_station)

### soil site
soil_station <- data.table::fread(stringr::str_glue("{dir_root}database/geography/Australia Soil/APsoil/Apsoil.csv"), select = c("site", "longitude", "latitude")) %>% 
  unique() %>% 
  .[! (longitude > 160 | longitude < 100)] %>% 
  sp_sf(longitude = "longitude", latitude = "latitude", index = "site")
soil_station$group <- "APsoil station (900)"
head(soil_station)

### wheatbelt_distribution
wheat_belt <- data.table::fread(stringr::str_glue("{dir_root}database/geography/map of Australia/Australia_wheatbelt_lonlat.csv")) %>% 
  sp_sf(longitude = "long", latitude = "lat", index = "site")
wheat_belt

data_plot <- rbind(met_station, soil_station)
head(data_plot)
data_plot$group <- factor(data_plot$group, levels = c("SILO met station (5836)", "APsoil station (900)"))

require(ggplot2)
# cairo_pdf(file = stringr::str_glue("{dir_work}wheatbelt_met station_and_soil site_distribution.pdf"), width = 12, height = 10, family = "Calibri", onefile = T)
ggplot() +
  geom_sf(data = wheat_belt, fill = rgb(239, 192, 2, maxColorValue = "255"), color = "black", shape = 22, size = 4.7)+
  geom_sf(data = data_plot, mapping = aes(color = group), size = 1, alpha = .5) +
  geom_sf(data = map, fill = "transparent", linewidth = 0.5) +
  # linear scale
  ggspatial::annotation_scale(location = "bl", text_cex = 2.5, pad_x = unit(2.3, "cm"), pad_y = unit(4.9, "cm")) +
  # compass
  ggspatial::annotation_north_arrow(location = "tr", pad_x = unit(3.2, "cm"), pad_y = unit(2.6, "cm"), which_north = "false", style = ggspatial::north_arrow_orienteering(text_size = 20, line_width = 1.5)) +
  scale_color_manual(values = c("#5376a5", "#ca3f3f"))+
  labs(color = "Station type")+
  guides(color = guide_legend(override.aes = list(size = 10), nrow = 2)) +
  theme_void() +
  theme(plot.title = element_text(size = 30), plot.subtitle = element_text(size = 30), legend.title = element_text(size = 30), legend.text = element_text(size = 25), legend.position = c(0.293, 0.11), legend.direction = "horizontal", legend.title.position = "top", plot.margin = margin(t = .1, r = .1, b = .1, l = .1, unit = "cm"), legend.key.width = unit(2.5, "cm"), legend.key.height = unit(0.3, "cm"))

dev.off()

# 1.8 Extract the grid information of each state
config <- data.table::fread(stringr::str_glue("{dir_root}database/geography/map of Australia/Australia_wheatbelt_lonlat.csv"))
head(config)

x <- config[1]
res <- purrr::map_dfr(.x = 1:nrow(config), .f = function(x, config){
  single <- config[x]
  location <- getLocation(location = c(single$long, single$lat), formatted = T)
  res <- data.frame(site = single$site, lon = single$long, lat = single$lat, location = location)
}, config)
data.table::fwrite(data.table::as.data.table(res), stringr::str_glue("{dir_root}database/geography/map of Australia/Australia_wheatbelt_lonlat.csv"))

# 2 Map of CWI-------------------------------------------------------------------------------------
require(ggplot2)
data <- data.table::fread(stringr::str_glue("{dir_work}res/adjust/wheatbeltCWSDI_VCI_KS_CWI.csv"), select = c("site", "long", "lat", "year", "term", "cwi"))  %>% 
  .[, .(cwi = mean(cwi)), by = .(site, long, lat, term)] %>% 
  # .[cwi < 0, WLD := "NA"] %>% 
  .[cwi >= 0 & cwi < 0.3, WLD := "Normal"] %>% 
  .[cwi >= 0.3 & cwi < 0.6, WLD := "Low"] %>% 
  .[cwi >= 0.6 & cwi < 0.9, WLD := "Medium"] %>% 
  .[cwi >= 0.9 & cwi < 1.2, WLD := "High"] %>% 
  .[cwi >= 1.2, WLD := "Extreme high"] %>% 
  na.omit()
range(data$cwi)

data[, .(.N), by = .(term, WLD)]

data_plot <- sp2sf(data = data, longitude = "long", latitude = "lat", index = "WLD")
data_plot$term <- data$term
data_plot$WLD <- factor(data_plot$WLD, levels = c("Normal", "Low", "Medium", "High", "Extreme high"), labels = c("Normal", "Mild waterlogging", "Moderate waterlogging", "Severe waterlogging", "Extremely Severe waterlogging"))
head(data_plot)

map <- sf::st_read(stringr::str_glue("{dir_root}database/geography/map of Australia/aagis_asgs16v1_g5a.shp"))

# cairo_pdf(file = stringr::str_glue("{dir_work}res/wheatbelt2000-2022_CWI_waterlogging_risk.pdf"), width = 34, height = 20, family = "Calibri", onefile = T)
ggplot() +
  facet_wrap(~ term, nrow = 2) +
  geom_sf(data = data_plot, mapping = aes(fill = WLD), color = "grey", size = 4, shape = 22) +
  geom_sf(data = map, fill = "transparent", linewidth = 1) +
  # linear scale
  ggspatial::annotation_scale(location = "bl", text_cex = 4, pad_x = unit(2.3, "cm"), pad_y = unit(4.5, "cm")) +
  # compass
  ggspatial::annotation_north_arrow(location = "tr", pad_x = unit(3.2, "cm"), pad_y = unit(2.6, "cm"), which_north = "false", style = ggspatial::north_arrow_orienteering(text_size = 40, line_width = 2)) +
  scale_fill_manual(values = viridis::viridis(n = 5, alpha = 1, begin = 0, end = 1, direction = 1, option = "A") %>% rev()) +
  guides(fill = guide_legend(override.aes = list(size = 15))) +
  labs(color = "Waterlogging classification")+
  theme_void() +
  theme(plot.title = element_text(size = 30), plot.subtitle = element_text(size = 30), legend.title = element_text(size = 50), legend.text = element_text(size = 45), legend.position = c(0.8, 0.3),  plot.margin = margin(t = .1, r = .1, b = .1, l = .1, unit = "cm"), legend.key.width = unit(4, "cm"), strip.text = element_text(size = 55))

dev.off()

### Average histogram of different waterlogging types by state
config <- data.table::fread(stringr::str_glue("{dir_root}database/geography/map of Australia/Australia_wheatbelt_lonlat.csv")) %>% 
  .[, State := stringr::str_squish(stringr::str_split_i(location, ",", -2L))] %>% 
  .[! State %in% c("Australian Capital Territory", "Queensland", "Tasmania")]
head(config)
unique(config$State)
config[, .N, by = .(State)]

data <- data.table::fread(stringr::str_glue("{dir_work}res/adjust/wheatbeltCWSDI_VCI_KS_CWI.csv"), select = c("site", "long", "lat", "year", "term", "cwi"))  %>% 
  .[, .(cwi = mean(cwi)), by = .(site, long, lat, term)] %>% 
  # .[cwi < 0, WLD := "NA"] %>% 
  .[cwi >= 0 & cwi < 0.3, WLD := "Normal"] %>% 
  .[cwi >= 0.3 & cwi < 0.6, WLD := "Low"] %>% 
  .[cwi >= 0.6 & cwi < 0.9, WLD := "Medium"] %>% 
  .[cwi >= 0.9 & cwi < 1.2, WLD := "High"] %>% 
  .[cwi >= 1.2, WLD := "Extreme high"] %>% 
  na.omit()
head(data)

res <- data[config[, ! "location"], on = .(site, long == lon, lat)]
head(res)

plot_data <- res[term %in% "EGP", .N, by = .(State, WLD)]
plot_data$WLD <- factor(plot_data$WLD, levels = c("Normal", "Low", "Medium", "High", "Extreme high"), labels = c("Normal", "Mild", "Moderate", "Severe", "Extremely severe"))
plot_data

require(ggplot2)
# cairo_pdf(file = stringr::str_glue("{dir_work}res/P1_wheatbelt2000-2022 CWI_IRSIC_KS_Waterlogging Risk Map_Annual Average_Column by State.pdf"), width = 25, height = 12, family = "Calibri", onefile = T)
ggplot(data = plot_data, mapping = aes(x = WLD, y = N, fill = WLD))+
  facet_grid(. ~ State)+
  geom_col(color = "black", linewidth = 1, width = .8)+
  scale_fill_manual(values = viridis::viridis(n = 7, alpha = 1, begin = 0, end = 1, direction = 1, option = "A") %>% .[-c(1:2)] %>% rev())+
  guides(fill = "none")+
  labs(x = "Type of Waterlogging", y = "Number")+
  theme_test()+
  theme(
    axis.ticks = ggplot2::element_line(linewidth = 1.5),
    axis.ticks.length = unit(0.3, "cm"),
    panel.border = ggplot2::element_rect(linewidth = 1.5),
    strip.background = ggplot2::element_rect(fill = "#F5F5F5", linewidth = 1.5, colour = "black"),
    plot.margin = margin(t = 1, r = 3, b = 1, l = 1, unit = "cm"),
    axis.title = element_text(size = 50),
    axis.text = element_text(size = 40, color = "black"),
    axis.text.x = element_text(angle = 45, vjust = .5),
    strip.text = element_text(size = 45),
    legend.title = element_text(size = 45),
    legend.text = element_text(size = 40),
    legend.position = c(0.85, 0.85)
  )

dev.off()

## KS frenquency
data_plot <- data.table::fread(stringr::str_glue("{dir_work}res/adjust/wheatbeltCWSDI_VCI_KS_CWI.csv"), select = c("site", "long", "lat", "year", "term", "cwi"))  %>% 
  # .[cwi < 0, WLD := "NA"] %>% 
  .[cwi >= 0 & cwi < 0.3, WLD := "Normal"] %>% 
  .[cwi >= 0.3 & cwi < 0.6, WLD := "Low"] %>% 
  .[cwi >= 0.6 & cwi < 0.9, WLD := "Medium"] %>% 
  .[cwi >= 0.9 & cwi < 1.2, WLD := "High"] %>% 
  .[cwi >= 1.2, WLD := "Extreme high"] %>% 
  na.omit() %>% 
  .[, .(Years = .N), by = .(site, long, lat, term, WLD)]
head(data_plot)
# data.table::fwrite(data_plot, stringr::str_glue("{dir_work}res/res_wheatbelt_KS_WLD.csv"))

data_plot_all <- data.table::fread(stringr::str_glue("{dir_work}res/res_wheatbelt_KS_WLD.csv"))
head(data_plot_all)

data_plot_all[, .N, by = .(term, WLD)]

uniq <- unique(data_plot[, .(term, WLD)])

require(ggplot2)
# cairo_pdf(file = stringr::str_glue("{dir_work}res/Wheatbelt2000-2022 CWI_IRSIC_KS_Waterlogging Risk Map_Frequency.pdf"), width = 12, height = 8, family = "Calibri", onefile = T)
  # i <- 1
  for (i in 1:nrow(uniq)) {
    data_plot <- data_plot_all[term %in% uniq$term[i] & WLD %in% uniq$WLD[i]]
    data_plot1 <- sp2sf(data = data_plot, longitude = "long", latitude = "lat", index = "Years")
    data_plot1$term <- data_plot$term
    data_plot1$WLD <- data_plot$WLD
    data_plot1$WLD <- factor(data_plot1$WLD, levels = c("Normal", "Low", "Medium", "High", "Extreme high"), labels = c("Normal", "Mild waterlogging", "Moderate waterlogging", "Severe waterlogging", "Extremely Severe waterlogging"))
    data_plot1$Years <- data_plot1$Years / 23 * 100
    # range(data_plot1$Years)
    # head(data_plot1)
    
    p <- ggplot() +
      geom_sf(data = data_plot1, mapping = aes(color = Years), size = 3.3, shape = 15) +
      geom_sf(data = map, fill = "transparent", linewidth = 1) +
      # linear scale
      ggspatial::annotation_scale(location = "bl", text_cex = 2, pad_x = unit(2.3, "cm"), pad_y = unit(4.7, "cm")) +
      # compass
      ggspatial::annotation_north_arrow(location = "tr", pad_x = unit(3.2, "cm"), pad_y = unit(2.6, "cm"), which_north = "false", style = ggspatial::north_arrow_orienteering(text_size = 25, line_width = 2)) +
      scale_color_gradientn(limit = c(0, 100), colours = viridis::viridis(n = 10, alpha = 1, begin = 0, end = 1, direction = 1, option = "A") %>% rev()) +
      labs(color = stringr::str_glue("Frequency (%)"), title = stringr::str_glue("{uniq$term[i]} {uniq$WLD[i]}"))+
      theme_void() +
      theme(plot.title = element_text(size = 30), plot.subtitle = element_text(size = 30), legend.title = element_text(size = 40), legend.text = element_text(size = 30), legend.position = c(0.3, 0.13), legend.direction = "horizontal", legend.title.position = "top", plot.margin = margin(t = .1, r = .1, b = .1, l = .1, unit = "cm"), legend.key.width = unit(2, "cm"), legend.key.height = unit(0.8, "cm"), strip.text = element_text(size = 50))

  print(p)
}
dev.off()

### Average annual frequency chart of different waterlogging types by state
config <- data.table::fread(stringr::str_glue("{dir_root}database/geography/map of Australia/Australia_wheatbelt_lonlat.csv")) %>% 
  .[, State := stringr::str_squish(stringr::str_split_i(location, ",", -2L))] %>% 
  .[! State %in% c("Australian Capital Territory", "Queensland", "Tasmania")]
head(config)
unique(config$State)
config[, .N, by = .(State)]

data <- data.table::fread(stringr::str_glue("{dir_work}res/adjust/wheatbeltCWSDI_VCI_KS_CWI.csv"), select = c("site", "long", "lat", "year", "term", "cwi"))  %>% 
  # .[cwi < 0, WLD := "NA"] %>% 
  .[cwi >= 0 & cwi < 0.3, WLD := "Normal"] %>% 
  .[cwi >= 0.3 & cwi < 0.6, WLD := "Low"] %>% 
  .[cwi >= 0.6 & cwi < 0.9, WLD := "Medium"] %>% 
  .[cwi >= 0.9 & cwi < 1.2, WLD := "High"] %>% 
  .[cwi >= 1.2, WLD := "Extreme high"] %>% 
  na.omit() 
data

res <- data[config[, ! "location"], on = .(site, long == lon, lat)]
head(res)

plot_data <- res[term %in% "EGP", .(`Mean CWI` = mean(cwi)), by = .(State, year)]
plot_data

require(ggplot2)
# cairo_pdf(file = stringr::str_glue("{dir_work}res/P1_wheatbelt2000-2022 CWI_IRSIC_KS_Waterlogging Risk Chart_Frequency_Line Chart by State.pdf"), width = 25, height = 9, family = "Calibri", onefile = T)
ggplot(data = plot_data, mapping = aes(x = year, y = `Mean CWI`))+
  facet_grid(. ~ State)+
  geom_line(color = "black", linewidth = 1)+
  geom_point(color = "black", shape = 21, size = 3, fill = "grey")+
  geom_hline(data = data.table(WLD = c("Mild", "Moderate", "Severe"), CWI = c(0.3, 0.6, 0.9)), mapping = aes(yintercept = CWI, color = WLD), linewidth = 1)+
  scale_color_manual(values = viridis::viridis(n = 7, alpha = 1, begin = 0, end = 1, direction = 1, option = "A") %>% .[-c(1:2)] %>% rev() %>% .[2:4])+
  guides(color = "none")+
  labs(x = "Year", y = "CWI")+
  theme_test()+
  theme(
    axis.ticks = ggplot2::element_line(linewidth = 1.5),
    axis.ticks.length = unit(0.3, "cm"),
    panel.border = ggplot2::element_rect(linewidth = 1.5),
    strip.background = ggplot2::element_rect(fill = "#F5F5F5", linewidth = 1.5, colour = "black"),
    plot.margin = margin(t = 1, r = 3, b = 1, l = 1, unit = "cm"),
    axis.title = element_text(size = 50),
    axis.text = element_text(size = 40, color = "black"),
    axis.text.x = element_text(angle = 45, vjust = .5),
    strip.text = element_text(size = 45),
    legend.title = element_text(size = 45),
    legend.text = element_text(size = 40),
    legend.position = c(0.85, 0.85)
  )

dev.off()

# 3 plot of result--------------------------------------------------------------------------s
data_all <- data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation_origin.csv"), select = c("site", "Zone", "SimulationName", "Date", "CO2_ton", "OrganicC_30cm", "WheatYield", "CanolaYield", "BarleyYield", "ChickpeaYield", "OatsYield", "CO2_N2O_wheat", "CO2_N2O_canola", "CO2_N2O_Barley", "CO2_N2O_chickpea", "CO2_N2O_Oats", "sumNO3Lost_kg_ha_wheat", "sumNO3Lost_kg_ha_barley", "sumNO3Lost_kg_ha_canola", "sumNO3Lost_kg_ha_chickpea", "sumNO3Lost_kg_ha_Oats", "denitn_kg_ha_wheat", "denitn_kg_ha_barley", "denitn_kg_ha_canola", "denitn_kg_ha_chickpea", "denitn_kg_ha_oats", "no3n_in_leach_wheat", "no3n_in_leach_barley", "no3n_in_leach_canola", "no3n_in_leach_chickpea", "no3n_in_leach_Oats", "no3n_in_runoff_wheat", "no3n_in_runoff_barley", "no3n_in_runoff_canola", "no3n_in_runoff_chickpea", "no3n_in_runoff_Oats"))
head(data_all)

# yield,CO2_ton, OrganicC_30cm, CO2_N2O, sumNO3Lost_kg_ha, denitn, no3n_in_leach, and no3n_in_runoff
purrr::walk(.x = unique(data_all$site), .f = function(Site, data_all){
  res_origin <- data_all[site %in% Site]

  res_yield <- data.table::copy(res_origin) %>% 
      melt.data.table(id.vars = c("site", "Zone", "SimulationName", "Date", "CO2_ton", "OrganicC_30cm"), measure.vars = c("WheatYield", "CanolaYield", "BarleyYield", "ChickpeaYield", "OatsYield"), variable.name = "Var", value.name = "Yield")

  res_CO2_N2O <- data.table::copy(res_origin) %>% 
      melt.data.table(id.vars = c("site", "Zone", "SimulationName", "Date"), measure.vars = c("CO2_N2O_wheat", "CO2_N2O_canola", "CO2_N2O_Barley", "CO2_N2O_chickpea", "CO2_N2O_Oats"), variable.name = "Var", value.name = "CO2_N2O") %>% 
      .[, Var := paste0(stringr::str_split_i(Var, "_", 3) %>% stringr::str_to_title(), "Yield")]

  res_sumNO3Lost_kg_ha <- data.table::copy(res_origin) %>% 
      melt.data.table(id.vars = c("site", "Zone", "SimulationName", "Date"), measure.vars = c("sumNO3Lost_kg_ha_wheat", "sumNO3Lost_kg_ha_barley", "sumNO3Lost_kg_ha_canola", "sumNO3Lost_kg_ha_chickpea", "sumNO3Lost_kg_ha_Oats"), variable.name = "Var", value.name = "sumNO3Lost_kg_ha") %>% 
      .[, Var := paste0(stringr::str_split_i(Var, "_", 4) %>% stringr::str_to_title(), "Yield")]

  res_denitn <- data.table::copy(res_origin) %>% 
      melt.data.table(id.vars = c("site", "Zone", "SimulationName", "Date"), measure.vars = c("denitn_kg_ha_wheat", "denitn_kg_ha_barley", "denitn_kg_ha_canola", "denitn_kg_ha_chickpea", "denitn_kg_ha_oats"), variable.name = "Var", value.name = "denitn_kg_ha") %>% 
      .[, Var := paste0(stringr::str_split_i(Var, "_", 4) %>% stringr::str_to_title(), "Yield")]

  res_no3n_in_leach <- data.table::copy(res_origin) %>% 
      melt.data.table(id.vars = c("site", "Zone", "SimulationName", "Date"), measure.vars = c("no3n_in_leach_wheat", "no3n_in_leach_barley", "no3n_in_leach_canola", "no3n_in_leach_chickpea", "no3n_in_leach_Oats"), variable.name = "Var", value.name = "no3n_in_leach") %>% 
      .[, Var := paste0(stringr::str_split_i(Var, "_", 4) %>% stringr::str_to_title(), "Yield")]

  res_no3n_in_runoff <- data.table::copy(res_origin) %>% 
      melt.data.table(id.vars = c("site", "Zone", "SimulationName", "Date"), measure.vars = c("no3n_in_runoff_wheat", "no3n_in_runoff_barley", "no3n_in_runoff_canola", "no3n_in_runoff_chickpea", "no3n_in_runoff_Oats"), variable.name = "Var", value.name = "no3n_in_runoff") %>% 
      .[, Var := paste0(stringr::str_split_i(Var, "_", 4) %>% stringr::str_to_title(), "Yield")]

  res <- res_yield[res_CO2_N2O, on = .(site, Zone, SimulationName, Date, Var)] %>% 
    .[res_sumNO3Lost_kg_ha, on = .(site, Zone, SimulationName, Date, Var)] %>%
    .[res_denitn, on = .(site, Zone, SimulationName, Date, Var)] %>%
    .[res_no3n_in_leach, on = .(site, Zone, SimulationName, Date, Var)] %>%
    .[res_no3n_in_runoff, on = .(site, Zone, SimulationName, Date, Var)] %>%
    .[! Yield %in% 0] %>% 
    data.table::setorder(Zone, SimulationName, Date, Var) 

  data.table::fwrite(res, stringr::str_glue("{dir_work}res/WL_Ratation.csv"), append = T)
}, data_all, .progress = T)
rm(data_all); gc()

## Extract the data of waterlogged grid and write it down
data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation.csv")) %>% 
  .[site %in% sites_5$site] %>% 
  data.table::fwrite(., stringr::str_glue("{dir_work}res/WL_Ratation_5.csv"))

## Extracting grids with yield loss of more than 5%
data <- data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation_yield-NO3Lost-GHG-GHGI_rain90.csv"), select = c("site", "lon", "lat", "State", "Zone", "SimulationName", "mean_yield")) %>% 
  data.table::dcast.data.table(formula = site + lon + lat + State + SimulationName ~ Zone, value.var = "mean_yield") %>% 
  .[, Yield_loss := (Field - Field_WL) / Field * 100] %>% 
  .[, .(mean_yield_loss = mean(Yield_loss)), by = .(site, lon , lat)] %>% 
  .[mean_yield_loss > 5]
data
data.table::fwrite(data, stringr::str_glue("{dir_work}res/WL_Ratation_yield_threshold_5_grid.csv"))

# Optimum rotation mode and yield difference
data <- config[data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation_5.csv"), select = c("site", "Zone", "SimulationName", "Yield")), on = .(site)]
head(data)
uniq_all <- unique(data[, .(site, State, Zone, SimulationName)])

## yield
row <- 1
data_all <- purrr::map_dfr(.x = 1:nrow(uniq_all), .f = function(row, data, uniq_all){
  single <- data[uniq_all[row], on = .(site, State, Zone, SimulationName)]
  floor <- nrow(single) %/% 4
  duo <- nrow(single) %% 4
  
  if (duo == 0) {
    single$rotation <- c(rep(1:floor, 4) %>% sort())
  } else {
    single$rotation <- c(rep(1:floor, 4) %>% sort(), (1:4)[1:duo])
  }
  
  res <- single[, .(mean_yield = mean(Yield)), by = .(site, lon, lat, State, Zone, SimulationName, rotation)]
  return(res)
}, data, uniq_all)
data_all

# Extract the rotation mode with the largest (best) yield under waterlogging.
yield_data <- data_all[, .(Yield = mean(mean_yield)), by = .(site, lon, lat, Zone, SimulationName)] %>% 
  data.table::dcast.data.table(formula = site + lon + lat + SimulationName ~ Zone, value.var = "Yield") %>% 
  .[, Yield_loss := (Field - Field_WL) / Field * 100] %>% 
  .[, .SD[which.max(Field_WL), .(max_Field_WL = Field_WL, SimulationName, Yield_loss)], by = .(site, lon, lat)]
yield_data
yield_data[, .N, by = .(SimulationName)]

### the optimal rotation_distribution
data_plot <- data.table::copy(yield_data) %>% 
  sp2sf(longitude = "lon", latitude = "lat", index = "SimulationName", name = "Optimal rotation")  
data_plot

# cairo_pdf(file = stringr::str_glue("{dir_work}res/Yield Verification under Different Rotation and Waterlogging from 1961 to 2020 _rain90_ Optimal Rotation Mode.pdf"), width = 12, height = 10, family = "Calibri", onefile = T)
ggplot() +
  geom_sf(data = data_plot, mapping = aes(fill = `Optimal rotation`), color = "grey", size = 4.7, shape = 22) +
  geom_sf(data = map, fill = "transparent", linewidth = 1) +
  # linear scale
  ggspatial::annotation_scale(location = "bl", text_cex = 2, pad_x = unit(2.3, "cm"), pad_y = unit(5.6, "cm")) +
  # compass
  ggspatial::annotation_north_arrow(location = "tr", pad_x = unit(3.2, "cm"), pad_y = unit(2.6, "cm"), which_north = "false", style = ggspatial::north_arrow_orienteering(text_size = 40, line_width = 2)) +
  scale_fill_manual(values = viridis::viridis(n = 12, alpha = 1, begin = 0, end = 1, direction = 1, option = "A") %>% .[c(4, 6, 8, 10, 12)]) +
  guides(fill = guide_legend(ncol = 2)) +
  theme_void() +
  theme(plot.title = element_text(size = 30), plot.subtitle = element_text(size = 30), legend.title = element_text(size = 25), legend.text = element_text(size = 18), legend.position = c(0.335, 0.11), legend.direction = "horizontal", legend.title.position = "top", plot.margin = margin(t = .1, r = .1, b = .1, l = .1, unit = "cm"), legend.key.width = unit(2, "cm"), strip.text = element_text(size = 55))

dev.off()

#### the optimal rotation yield loss
data_plot <- sp2sf(data = yield_data, longitude = "lon", latitude = "lat", index = "Yield_loss")
data_plot

# cairo_pdf(file = stringr::str_glue("{dir_work}res/Yield Verification under Different Rotation and Waterlogging from 1961 to 2020 _rain90_ Yield Loss Percentage.pdf"), width = 12, height = 10, family = "Calibri", onefile = T)
ggplot() +
  geom_sf(data = data_plot, mapping = aes(fill = Yield_loss), color = "grey", size = 4.7, shape = 22) +
  geom_sf(data = map, fill = "transparent", linewidth = 1) +
  # linear scale
  ggspatial::annotation_scale(location = "bl", text_cex = 2, pad_x = unit(2.3, "cm"), pad_y = unit(5.6, "cm")) +
  # compass
  ggspatial::annotation_north_arrow(location = "tr", pad_x = unit(3.2, "cm"), pad_y = unit(2.6, "cm"), which_north = "false", style = ggspatial::north_arrow_orienteering(text_size = 40, line_width = 2)) +
  scale_fill_gradientn(colours = viridis::viridis(n = 10, alpha = 1, begin = 0, end = 1, direction = 1, option = "A") %>% rev() %>% .[1:8]) +
  labs(fill = "Yield loss (%)")+
  theme_void() +
  theme(plot.title = element_text(size = 30), plot.subtitle = element_text(size = 30), legend.title = element_text(size = 35), legend.text = element_text(size = 30), legend.position = c(0.25, 0.1), legend.direction = "horizontal", legend.title.position = "top", plot.margin = margin(t = .1, r = .1, b = .1, l = .1, unit = "cm"), legend.key.width = unit(2, "cm"), strip.text = element_text(size = 55))

dev.off()

#### plot
data_plot <- data_all[, .(Yield = mean(mean_yield)), by = .(site, lon, lat, State, Zone, SimulationName)]
data_plot$SimulationName <- factor(data_plot$SimulationName, levels = unique(data_plot$SimulationName), labels = stringr::str_split(unique(data_plot$SimulationName), "_")  %>% purrr::map_vec(.f = function(x) {stringr::str_sub(x, start = 1L, end = 1L) %>% paste0(collapse = "")}))

# cairo_pdf(file = stringr::str_glue("{dir_work}res/Box diagram of yield by state under different rotation and waterlogging from 1961 to 2020.pdf"), width = 18, height = 19, family = "Calibri", onefile = T)
ggplot()+
  facet_wrap(. ~ State, scales = "free_x", ncol = 2)+
  geom_jitter(data = data_plot[Zone %in% "Field"], mapping = aes(y = Yield, x = reorder(SimulationName, Yield, decreasing = T)), color = "grey30", alpha = .5, size = 3, width = .4)+
  geom_boxplot(data = data_plot[Zone %in% "Field"], mapping = aes(y = Yield, x = reorder(SimulationName, Yield, decreasing = T), fill = reorder(SimulationName, Yield)), width = .7, color = "grey40", linewidth = .7, position = position_dodge2(padding = .2), outliers = F, alpha = .3)+
  geom_jitter(data = data_plot[Zone %in% "Field_WL"], mapping = aes(y = Yield, x = reorder(SimulationName, Yield, decreasing = T), color = reorder(SimulationName, Yield)), alpha = .5, size = 3, width = .3)+
  geom_boxplot(data = data_plot[Zone %in% "Field_WL"], mapping = aes(y = Yield, x = reorder(SimulationName, Yield, decreasing = T), fill = reorder(SimulationName, Yield)), width = .3, linewidth = .7, position = position_dodge2(padding = .2), outliers = F, alpha = .5)+
  scale_y_continuous(expand = c(0.01, 0), limits = c(0, 6500))+
  scale_fill_manual(values = viridis::viridis(n = 18, alpha = 1, begin = 0, end = 1, direction = 1, option = "A") %>% .[5:18] %>% rev())+
  scale_color_manual(values = viridis::viridis(n = 18, alpha = 1, begin = 0, end = 1, direction = 1, option = "A") %>% .[5:18] %>% rev())+
  guides(fill = "none", color = "none")+
  labs(y = "Yield (kg/ha)", x = "Crop Rotations")+
  theme_test()+
  theme(
    axis.ticks = ggplot2::element_line(linewidth = 1.5),
    axis.ticks.length = unit(0.3, "cm"),
    panel.border = ggplot2::element_rect(linewidth = 1.5),
    strip.background = ggplot2::element_rect(fill = "#F5F5F5", linewidth = 1.5, colour = "black"),
    plot.margin = margin(t = 1, r = 3, b = 1, l = 3, unit = "cm"),
    axis.title = element_text(size = 45),
    axis.text = element_text(size = 40, color = "black"), # 40
    axis.text.x = element_text(angle = 90, vjust = .5),
    strip.text = element_text(size = 40),
    legend.title = element_text(size = 55),
    legend.text = element_text(size = 45)
  )

dev.off()

# Calculate the state average CO2 flux change year by year.
res_all <- config[, .(site, lon, lat, State)] %>% 
  .[data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation_5.csv"), select = c("site", "Zone", "SimulationName", "Date", "CO2_ton", "CO2_N2O")) %>% .[Zone %in% "Field_WL"], on = .(site)] %>%
  .[, Year := data.table::year(Date)] %>%  
  .[, .(CO2_ton = mean(CO2_ton) / 1000, CO2_N2O = mean(CO2_N2O) / 1000), by = .(State, SimulationName, Year)] %>% 
  data.table::setorder(State, SimulationName, Year)
res_all

uniq_all <- unique(res_all[, .(State, SimulationName)])
row <- 1
res <- purrr::map_dfr(.x = 1:nrow(uniq_all), .f = function(row, res_all, uniq_all){
  uniq <- uniq_all[row]
  single <- res_all[uniq, on = .(State, SimulationName)]
  single$CO2_ton <- purrr::map_vec(.x = 1:nrow(single), .f = function(row, single){single$CO2_ton[row] - single$CO2_ton[1]}, single)
  
  return(single)
}, res_all, uniq_all)
res

## line plot
row <- 1
res <- purrr::map_dfr(.x = 1:nrow(uniq_all), .f = function(row, res, uniq_all){
  uniq <- uniq_all[row]
  single <- res[uniq, on = .(State, SimulationName)]
  single$CO2_ton <- cumsum(single$CO2_ton)
  single$CO2_N2O <- cumsum(single$CO2_N2O)
  
  return(single)
}, res, uniq_all)

res
# data_plot <- data.table::melt.data.table(data = res, id.vars = c("State", "SimulationName", "Year"), variable.name = "Type", value.name = "CO2")
data_plot <- data.table::copy(res) %>% 
  .[, CO2 := CO2_ton + CO2_N2O]
head(data_plot)

# cairo_pdf(file = stringr::str_glue("{dir_work}res/Line chart of CO2 flux by state.pdf"), width = 16, height = 12, family = "Calibri", onefile = T)
ggplot(data = data_plot, mapping = aes(x = Year, y = CO2, color = SimulationName))+
  facet_wrap(State ~ SimulationName)+
  geom_line(linewidth = 1)+
  geom_hline(yintercept = 0, linetype = "longdash", linewidth = .7)+
  scale_color_manual(values = viridis::viridis(n = 18, alpha = 1, begin = 0, end = 1, direction = 1, option = "A") %>% .[2:15])+
  labs(y = "Δ Soil fluxes (tCO2-eq ha-1)")+
  guides(color = "none")+
  theme_test()+
  theme(axis.title = element_text(size = 30), 
        axis.text = element_text(size = 25, color = "black"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 17),
        legend.position = c(0.85, 0.85),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        strip.text = element_text(size = 20),
        axis.ticks = ggplot2::element_line(linewidth = 1),
        axis.ticks.length = unit(0.3, "cm"),
        panel.border = ggplot2::element_rect(linewidth = 1),
        strip.background = ggplot2::element_rect(fill = "#F5F5F5", linewidth = 1, colour = "black"),
        plot.margin = margin(t = 1, r = 3, b = 1, l = 1, unit = "cm"))

dev.off()

# Calculate denitrification and organic carbon
data_plot <- config[data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation_5.csv")) %>% .[Zone %in% "Field_WL"], on = .(site)] %>% 
  .[, Year := data.table::year(Date)] %>%  
  .[, .(denitn = mean(denitn_kg_ha)), by = .(State, SimulationName, Year)] %>% 
  data.table::setorder(State, SimulationName, Year)
data_plot

# cairo_pdf(file = stringr::str_glue("{dir_work}res/Denitrification line chart by state.pdf"), width = 16, height = 12, family = "Calibri", onefile = T)
ggplot(data = data_plot, mapping = aes(x = Year, y = denitn, color = SimulationName))+
  facet_wrap(State ~ SimulationName)+ #, scales = "free_y"
  geom_line(linewidth = 1)+
  geom_hline(yintercept = 10, linetype = "longdash", linewidth = .7)+
  guides(color = "none")+
  scale_color_manual(values = viridis::viridis(n = 18, alpha = 1, begin = 0, end = 1, direction = 1, option = "A") %>% .[2:15])+
  labs(y = "Denitn (kg/ha)")+
  theme_test()+
  theme(axis.title = element_text(size = 30), 
        axis.text = element_text(size = 25, color = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        strip.text = element_text(size = 20),
        axis.ticks = ggplot2::element_line(linewidth = 1),
        axis.ticks.length = unit(0.3, "cm"),
        panel.border = ggplot2::element_rect(linewidth = 1),
        strip.background = ggplot2::element_rect(fill = "#F5F5F5", linewidth = 1, colour = "black"),
        plot.margin = margin(t = 1, r = 3, b = 1, l = 1, unit = "cm"))

dev.off()

## SOC
data_plot <- config[data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation_5.csv")) %>% .[Zone %in% "Field_WL"], on = .(site)] %>%
  .[, Year := data.table::year(Date)] %>%  
  .[, .(OrganicC = mean(OrganicC_30cm)), by = .(State, SimulationName, Year)] %>% 
  data.table::setorder(State, SimulationName, Year)
data_plot

# cairo_pdf(file = stringr::str_glue("{dir_work}res/Organic carbon line chart by state.pdf"), width = 16, height = 12, family = "Calibri", onefile = T)
ggplot(data = data_plot, mapping = aes(x = Year, y = OrganicC, color = SimulationName))+
  facet_wrap(State ~ SimulationName)+ # , scales = "free_y"
  geom_line(linewidth = 1)+
  # geom_hline(yintercept = 0, linetype = "longdash", linewidth = .7)+
  guides(color = "none")+
  scale_color_manual(values = viridis::viridis(n = 18, alpha = 1, begin = 0, end = 1, direction = 1, option = "A") %>% .[2:15])+
  labs(y = "SOC:0-30cm (kg/ha)")+
  theme_test()+
  theme(axis.title = element_text(size = 30), 
        axis.text = element_text(size = 25, color = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        strip.text = element_text(size = 20),
        axis.ticks = ggplot2::element_line(linewidth = 1),
        axis.ticks.length = unit(0.3, "cm"),
        panel.border = ggplot2::element_rect(linewidth = 1),
        strip.background = ggplot2::element_rect(fill = "#F5F5F5", linewidth = 1, colour = "black"),
        plot.margin = margin(t = 1, r = 3, b = 1, l = 1, unit = "cm"))

dev.off()

## sumNO3Lost_kg_ha
data_plot <- config[data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation_5.csv")) %>% .[Zone %in% "Field_WL"], on = .(site)] %>%
  .[, Year := data.table::year(Date)] %>%  
  .[, .(sumNO3Lost = mean(sumNO3Lost_kg_ha)), by = .(State, SimulationName, Year)] %>% 
  data.table::setorder(State, SimulationName, Year)
data_plot

# cairo_pdf(file = stringr::str_glue("{dir_work}res/N-loss broken line by state.pdf"), width = 16, height = 12, family = "Calibri", onefile = T)
ggplot(data = data_plot, mapping = aes(x = Year, y = sumNO3Lost, color = SimulationName))+
  facet_wrap(State ~ SimulationName)+ # , scales = "free_y"
  geom_line(linewidth = 1)+
  # geom_hline(yintercept = 0, linetype = "longdash", linewidth = .7)+
  guides(color = "none")+
  geom_hline(yintercept = 10, linetype = "longdash", linewidth = .7)+
  scale_color_manual(values = viridis::viridis(n = 18, alpha = 1, begin = 0, end = 1, direction = 1, option = "A") %>% .[2:15])+
  labs(y = "sumNO3Lost (kg/ha)")+
  theme_test()+
  theme(axis.title = element_text(size = 30), 
        axis.text = element_text(size = 25, color = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        strip.text = element_text(size = 20),
        axis.ticks = ggplot2::element_line(linewidth = 1),
        axis.ticks.length = unit(0.3, "cm"),
        panel.border = ggplot2::element_rect(linewidth = 1),
        strip.background = ggplot2::element_rect(fill = "#F5F5F5", linewidth = 1, colour = "black"),
        plot.margin = margin(t = 1, r = 3, b = 1, l = 1, unit = "cm"))

dev.off()

# profit
data_all <- config[, .(site, State)] %>% 
  .[data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation_5.csv"), select = c("site", "Zone", "SimulationName", "Date", "Var", "Yield")) %>% .[Zone %in% "Field_WL"], on = .(site)] %>%
  .[, Year := data.table::year(Date)] %>% 
  .[, c("Zone", "Date") := NULL] %>% 
  .[, Price_mean := fcase(Var %in% "WheatYield", 347, Var %in% "CanolaYield", 640, Var %in% "BarleyYield", 281, Var %in% "ChickpeaYield", 513, Var %in% "OatsYield", 301)] %>% 
  .[, Price_sd := fcase(Var %in% "WheatYield", 61, Var %in% "CanolaYield", 127, Var %in% "BarleyYield", 41, Var %in% "ChickpeaYield", 183, Var %in% "OatsYield", 103)] %>% 
  .[, Cost_mean := fcase(Var %in% "WheatYield", 601, Var %in% "CanolaYield", 613, Var %in% "BarleyYield", 546, Var %in% "ChickpeaYield", 524, Var %in% "OatsYield", 391)] %>% 
  .[, Cost_sd := fcase(Var %in% "WheatYield", 148, Var %in% "CanolaYield", 84, Var %in% "BarleyYield", 137, Var %in% "ChickpeaYield", 77, Var %in% "OatsYield", 95)] %>% 
  .[, c("profit", "profit_max", "profit_min") := list(Yield / 1000 * Price_mean - Cost_mean, (Yield / 1000) * (Price_mean + Price_sd) - (Cost_mean - Cost_sd), (Yield / 1000) * (Price_mean - Price_sd) - (Cost_mean + Cost_sd))]  

head(data_all)
uniq_all <- unique(data_all[, .(site, State, SimulationName)])

row <- 1
res <- purrr::map_dfr(.x = 1:nrow(uniq_all), .f = function(row, data_all, uniq_all){
  single <- data_all[uniq_all[row], on = .(site, State, SimulationName)]
  floor <- nrow(single) %/% 4
  duo <- nrow(single) %% 4
  
  if (duo == 0) {
    single$rotation <- c(rep(1:floor, 4) %>% sort())
  } else {
    single$rotation <- c(rep(1:floor, 4) %>% sort(), (1:4)[1:duo])
  }
  
  res <- single[, .(profit = mean(profit), profit_max = mean(profit_max), profit_min = mean(profit_min)), by = .(site, State, SimulationName, rotation)]
  return(res)
}, data_all, uniq_all)

res

data_plot_all <- data.table::melt.data.table(data = res, id.vars = c("site", "State", "SimulationName", "rotation"), variable.name = "Profit", value.name = "Price")
head(data_plot_all)
data_5_all <- data_plot_all[, .(Median = quantile(x = Price, 0.5)), by = .(State, SimulationName)]

theme_huhu <- theme_test()+
  theme(axis.title = element_text(size = 40), 
        axis.text = element_text(size = 35, color = "black"),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20),
        legend.position = c(0.8, 0.76),
        # axis.text.x = element_text(angle = 90, vjust = 0.5),
        strip.text = element_text(size = 35),
        axis.ticks = ggplot2::element_line(linewidth = 1),
        axis.ticks.length = unit(0.3, "cm"),
        panel.border = ggplot2::element_rect(linewidth = 1),
        strip.background = ggplot2::element_rect(fill = "#F5F5F5", linewidth = 1, colour = "black"),
        plot.margin = margin(t = 1, r = 3, b = 1, l = 1, unit = "cm"))

### 1 Western Australia
data_plot_WA <- data_plot_all[State %in% "Western Australia"]
data_plot_WA$SimulationName <- factor(data_plot_WA$SimulationName, levels = unique(data_plot_WA$SimulationName), labels = stringr::str_split(unique(data_plot_WA$SimulationName), "_")  %>% purrr::map_vec(.f = function(x) {stringr::str_sub(x, start = 1L, end = 1L) %>% paste0(collapse = "")}))
data_5_WA <- data_plot_WA[, .(Median = quantile(x = Price, 0.5)), by = .(SimulationName)] %>% 
  .[, State := "Western Australia"]

### 2 South Australia
data_plot_SA <- data_plot_all[State %in% "South Australia"]
data_plot_SA$SimulationName <- factor(data_plot_SA$SimulationName, levels = unique(data_plot_SA$SimulationName), labels = stringr::str_split(unique(data_plot_SA$SimulationName), "_")  %>% purrr::map_vec(.f = function(x) {stringr::str_sub(x, start = 1L, end = 1L) %>% paste0(collapse = "")}))
data_5_SA <- data_plot_SA[, .(Median = quantile(x = Price, 0.5)), by = .(SimulationName)] %>% 
  .[, State := "South Australia"]

### 3 Victoria
data_plot_VIC <- data_plot_all[State %in% "Victoria"]
data_plot_VIC$SimulationName <- factor(data_plot_VIC$SimulationName, levels = unique(data_plot_VIC$SimulationName), labels = stringr::str_split(unique(data_plot_VIC$SimulationName), "_")  %>% purrr::map_vec(.f = function(x) {stringr::str_sub(x, start = 1L, end = 1L) %>% paste0(collapse = "")}))
data_5_VIC <- data_plot_VIC[, .(Median = quantile(x = Price, 0.5)), by = .(SimulationName)] %>% 
  .[, State := "Victoria"]

### 4 New South Wales
data_plot_NSW <- data_plot_all[State %in% "New South Wales"]
data_plot_NSW$SimulationName <- factor(data_plot_NSW$SimulationName, levels = unique(data_plot_NSW$SimulationName), labels = stringr::str_split(unique(data_plot_NSW$SimulationName), "_")  %>% purrr::map_vec(.f = function(x) {stringr::str_sub(x, start = 1L, end = 1L) %>% paste0(collapse = "")}))
data_5_NSW <- data_plot_NSW[, .(Median = quantile(x = Price, 0.5)), by = .(SimulationName)] %>% 
  .[, State := "New South Wales"]

data_plot <- rbind(data_plot_WA, data_plot_SA, data_plot_VIC, data_plot_NSW)
data_5 <- rbind(data_5_WA, data_5_SA, data_5_VIC, data_5_NSW)

# cairo_pdf(file = stringr::str_glue("{dir_work}res/Profit density map by state.pdf"), width = 36.5, height = 8, family = "Calibri", onefile = T)
data_d <- data_plot[State %in% "New South Wales"]
data_d$SimulationName <- factor(data_d$SimulationName, levels = c("FWOW", "OCWC", "WCWO", "WBWC", "WWWW"))
data_l <- data_5[State %in% "New South Wales"]
data_l$SimulationName <- factor(data_l$SimulationName, levels = c("FWOW", "OCWC", "WCWO", "WBWC", "WWWW"))
p_NSW <- ggplot(data = data_d, mapping = aes(x = Price))+
  facet_wrap(. ~ State)+
  geom_density(mapping = aes(color = SimulationName), linewidth = 1.5)+
  geom_vline(data = data_l, aes(xintercept = Median, color = SimulationName), linetype = "dashed", linewidth = 1.2) +
  scale_color_manual(values = viridis::viridis(n = 25, alpha = 1, begin = 0, end = 1, direction = 1, option = "A") %>% .[c(5, 12, 16, 20, 23)])+
  scale_y_continuous(expand = c(0.00001, 0.00001), limits = c(0, 0.001))+
  scale_x_continuous(limits = range(data_plot_all$Price))+
  labs(x = "Profit ($/ha/year)", color = "Crop rotations")+
  theme_huhu

data_d <- data_plot[State %in% "South Australia"]
data_d$SimulationName <- factor(data_d$SimulationName, levels = c("FWOW", "WFWW", "WCWC", "WOWW", "WWWW"))
data_l <- data_5[State %in% "South Australia"]
data_l$SimulationName <- factor(data_l$SimulationName, levels = c("FWOW", "WFWW", "WCWC", "WOWW", "WWWW"))
p_SA <- ggplot(data = data_d, mapping = aes(x = Price))+
  facet_wrap(. ~ State)+
  geom_density(mapping = aes(color = SimulationName), linewidth = 1.5)+
  geom_vline(data = data_l, aes(xintercept = Median, color = SimulationName), linetype = "dashed", linewidth = 1.2) +
  scale_color_manual(values = viridis::viridis(n = 25, alpha = 1, begin = 0, end = 1, direction = 1, option = "A") %>% .[c(5, 12, 16, 18, 23)])+
  scale_y_continuous(expand = c(0.00001, 0.00001), limits = c(0, 0.001))+
  scale_x_continuous(limits = range(data_plot_all$Price))+
  labs(x = "Profit ($/ha/year)", color = "Crop rotations")+
  theme_huhu

data_d <- data_plot[State %in% "Victoria"]
data_d$SimulationName <- factor(data_d$SimulationName, levels = c("OFCW", "BFCW", "WFCW", "CWCW", "WWWW"))
data_l <- data_5[State %in% "Victoria"]
data_l$SimulationName <- factor(data_l$SimulationName, levels = c("OFCW", "BFCW", "WFCW", "CWCW", "WWWW"))
p_VIC <- ggplot(data = data_d, mapping = aes(x = Price))+
  facet_wrap(. ~ State)+
  geom_density(mapping = aes(color = SimulationName), linewidth = 1.5)+
  geom_vline(data = data_l, aes(xintercept = Median, color = SimulationName), linetype = "dashed", linewidth = 1.2) +
  scale_color_manual(values = viridis::viridis(n = 25, alpha = 1, begin = 0, end = 1, direction = 1, option = "A") %>% .[c(5, 12, 16, 20, 23)])+
  scale_y_continuous(expand = c(0.00001, 0.00001), limits = c(0, 0.001))+
  scale_x_continuous(limits = range(data_plot_all$Price))+
  labs(x = "Profit ($/ha/year)", color = "Crop rotations")+
  theme_huhu

data_d <- data_plot[State %in% "Western Australia"]
data_d$SimulationName <- factor(data_d$SimulationName, levels = c("FWOW", "WWBC", "WBCW", "WCWC", "WWWW"))
data_l <- data_5[State %in% "Western Australia"]
data_l$SimulationName <- factor(data_l$SimulationName, levels = c("FWOW", "WWBC", "WBCW", "WCWC", "WWWW"))
p_WA <- ggplot(data = data_d, mapping = aes(x = Price))+
  facet_wrap(. ~ State)+
  geom_density(mapping = aes(color = SimulationName), linewidth = 1.5)+
  geom_vline(data = data_l, aes(xintercept = Median, color = SimulationName), linetype = "dashed", linewidth = 1.2) +
  scale_color_manual(values = viridis::viridis(n = 25, alpha = 1, begin = 0, end = 1, direction = 1, option = "A") %>% .[c(5, 12, 16, 20, 23)])+
  scale_y_continuous(expand = c(0.00001, 0.00001), limits = c(0, 0.001))+
  scale_x_continuous(limits = range(data_plot_all$Price))+
  labs(x = "Profit ($/ha/year)", color = "Crop rotations")+
  theme_huhu

require(patchwork)
p <- (p_NSW | p_SA | p_VIC | p_WA) + patchwork::plot_layout(nrow = 1)
print(p)

dev.off()

# Calculate the output, CO2 flux, GHGI, N loss, N productivity and energy footprint, and draw a radar chart.
energy_data <- openxlsx::read.xlsx(stringr::str_glue("{dir_work}1.data/res/Energy footprint.xlsx"), sheet = 1) %>% 
  data.table::setDT()
energy_data

data_all <- config[data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation_5.csv"), select = c("site", "Zone", "SimulationName", "Yield")) %>% .[Zone %in% "Field"], on = .(site)]
head(data_all)
uniq_all <- unique(data_all[, .(site, State, SimulationName)])

## No waterlogging-yield (average after crop rotation) andN productivity (yield /N)
res <- purrr::map_dfr(.x = 1:nrow(uniq_all), .f = function(row, data_all, uniq_all){
  single <- data_all[uniq_all[row], on = .(site, State, SimulationName)]
  floor <- nrow(single) %/% 4
  duo <- nrow(single) %% 4
  
  if (duo == 0) {
    single$rotation <- c(rep(1:floor, 4) %>% sort())
  } else {
    single$rotation <- c(rep(1:floor, 4) %>% sort(), (1:4)[1:duo])
  }
  
  res <- single[, .(mean_yield = mean(Yield)), by = .(site, State, SimulationName, rotation)]
  return(res)
}, data_all, uniq_all)
res_yield <- res[, .(mean_yield = mean(mean_yield)), by = .(State, SimulationName)]
res_yield

data_all <- config[data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation_5.csv"), select = c("site", "Zone", "SimulationName", "Yield")) %>% .[Zone %in% "Field_WL"], on = .(site)]
head(data_all)
uniq_all <- unique(data_all[, .(site, State, SimulationName)])

## Waterlogging-yield (average after crop rotation) andN productivity (yield /N)
res <- purrr::map_dfr(.x = 1:nrow(uniq_all), .f = function(row, data_all, uniq_all){
  single <- data_all[uniq_all[row], on = .(site, State, SimulationName)]
  floor <- nrow(single) %/% 4
  duo <- nrow(single) %% 4
  vars <- stringr::str_split(unique(single$SimulationName), "_") %>% unlist()
  if (duo == 0) {
    single$rotation <- rep(vars, floor)
  } else {
    single$rotation <- c(rep(vars, floor), vars[1:duo])
  }
  
  res <- single %>%
    .[, .(mean_yield = mean(Yield)), by = .(site, State, SimulationName, rotation)]
  return(res)
}, data_all, uniq_all)
res_yield_NUE <- res[, .(mean_yield = mean(mean_yield)), by = .(State, SimulationName)] %>% 
  .[, NUE := mean_yield / 100] %>% 
  data.table::setnames("mean_yield", "mean_WL_yield")
res_yield_NUE

yield_cv <- res[, .(Yield = mean(mean_yield)), by = .(State, site, SimulationName)] %>% 
  .[, (round((sd(Yield) / mean(Yield)) * 100, 2)), by = .(State, SimulationName)] %>% 
  data.table::setnames("V1", "CV (%)")
yield_cv

## Energy footprint
res <- purrr::map_dfr(.x = 1:nrow(uniq_all), .f = function(row, data_all, uniq_all){
  single <- data_all[uniq_all[row], on = .(site, State, SimulationName)]
  
  floor <- nrow(single) %/% 4
  duo <- nrow(single) %% 4
  vars <- stringr::str_split(unique(single$SimulationName), "_") %>% unlist()
  if (duo == 0) {
    single$rotation <- rep(vars, floor)
  } else {
    single$rotation <- c(rep(vars, floor), vars[1:duo])
  }
  
  single[, energy := fcase(rotation == "Fababean", energy_data[Item == "Seed_bean"]$`Crop.Comprehensive.Energy.(MJ)`, 
                           rotation == "Wheat", energy_data[Item == "Seed_wheat"]$`Crop.Comprehensive.Energy.(MJ)`, 
                           rotation == "Oats", energy_data[Item == "Seed_oat"]$`Crop.Comprehensive.Energy.(MJ)`, 
                           rotation == "Barley", energy_data[Item == "Seed_barley"]$`Crop.Comprehensive.Energy.(MJ)`, 
                           rotation == "Canola", energy_data[Item == "Seed_canola"]$`Crop.Comprehensive.Energy.(MJ)`)]
  
  mean_yield <- mean(single$Yield)
  single <- single[Yield >=  (mean_yield * 0.5) & Yield <=  (mean_yield * 1.5)]
  
  res <- single[, energy_foot := energy / Yield * 1000] %>% 
    .[, .(mean_energy_foot = mean(energy_foot)), by = .(site, State, SimulationName, rotation)]
  return(res)
}, data_all, uniq_all)
res_energy <- res[, .(mean_energy_foot = mean(mean_energy_foot)), by = .(State, SimulationName)]
res_energy

## CO2 flux
res_all <- config[, .(site, lon, lat, State)] %>% 
  .[data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation_5.csv"), select = c("site", "Zone", "SimulationName", "Date", "CO2_ton", "CO2_N2O")) %>% .[Zone %in% "Field_WL"], on = .(site)] %>%
  .[, Year := data.table::year(Date)] %>%  
  .[, .(CO2_ton = mean(CO2_ton) / 1000, CO2_N2O = mean(CO2_N2O) / 1000), by = .(State, SimulationName, Year)] %>% 
  data.table::setorder(State, SimulationName, Year)
res_all

uniq_all <- unique(res_all[, .(State, SimulationName)])
row <- 1
res <- purrr::map_dfr(.x = 1:nrow(uniq_all), .f = function(row, res_all, uniq_all){
  uniq <- uniq_all[row]
  single <- res_all[uniq, on = .(State, SimulationName)]
  single$CO2_ton <- purrr::map_vec(.x = 1:nrow(single), .f = function(row, single){single$CO2_ton[row] - single$CO2_ton[1]}, single)
  
  return(single)
}, res_all, uniq_all)
res

row <- 1
res <- purrr::map_dfr(.x = 1:nrow(uniq_all), .f = function(row, res, uniq_all){
  uniq <- uniq_all[row]
  single <- res[uniq, on = .(State, SimulationName)]
  single$CO2_ton <- cumsum(single$CO2_ton)
  single$CO2_N2O <- cumsum(single$CO2_N2O)
  
  return(single)
}, res, uniq_all)

res_CO2 <- data.table::copy(res) %>% 
  .[, CO2 := CO2_ton + CO2_N2O] %>% 
  .[, .(CO2 = tail(CO2, 1)), by = .(State, SimulationName)]
res_CO2

## GHG
data_all <- config[, .(site, lon, lat, State)] %>% 
  .[data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation_5.csv"), select = c("site", "Zone", "SimulationName", "Date", "CO2_ton", "CO2_N2O")) %>% .[Zone %in% "Field_WL"], on = .(site)] %>%
  .[, Year := data.table::year(Date)] %>%  
  .[, .(CO2_ton = mean(CO2_ton), CO2_N2O = mean(CO2_N2O)), by = .(State, SimulationName, Year)]
head(data_all)

res_GHG <- data_all %>% 
  .[, .(delta_CO2_ton = CO2_ton[.N] - CO2_ton[1], CO2_N2O = mean(CO2_N2O)), by = .(State, SimulationName)] %>% 
  .[, mean_GHG := CO2_N2O - delta_CO2_ton * 44 / 12] %>% 
  .[, .(State, SimulationName, mean_GHG)]
res_GHG

## N lost
res_NO3Lost <- config[data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation_5.csv"), select = c("site", "Zone", "SimulationName", "sumNO3Lost_kg_ha")) %>% .[Zone %in% "Field_WL"], on = .(site)] %>%
  .[, .(sumNO3Lost = mean(sumNO3Lost_kg_ha)), by = .(State, SimulationName)]

## waterlogging resilience 
data_all_w <- config[data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation_5.csv"), select = c("site", "Zone", "SimulationName", "Yield")), on = .(site)]
head(data_all_w)
uniq_all <- unique(data_all_w[, .(site, State, Zone, SimulationName)])

row <- 1
res <- purrr::map_dfr(.x = 1:nrow(uniq_all), .f = function(row, data_all_w, uniq_all){
  single <- data_all_w[uniq_all[row], on = .(site, State, Zone, SimulationName)]
  floor <- nrow(single) %/% 4
  duo <- nrow(single) %% 4
  
  if (duo == 0) {
    single$rotation <- c(rep(1:floor, 4) %>% sort())
  } else {
    single$rotation <- c(rep(1:floor, 4) %>% sort(), (1:4)[1:duo])
  }
  
  res <- single[, .(mean_yield = mean(Yield)), by = .(site, State, Zone, SimulationName, rotation)]
  return(res)
}, data_all_w, uniq_all)

res_wr <- res[, .(mean_yield = mean(mean_yield)), by = .(State, Zone, SimulationName)] %>% 
  data.table::dcast.data.table(formula = State + SimulationName ~ Zone, value.var = "mean_yield") %>% 
  .[, `Waterlogging resilience %` := (Field - Field_WL) / Field * 100] %>% 
  .[, c("Field", "Field_WL") := NULL]
res_wr

## profit
data_all <- config[, .(site, State)] %>% 
  .[data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation_5.csv"), select = c("site", "Zone", "SimulationName", "Date", "Var", "Yield")) %>% .[Zone %in% "Field_WL"], on = .(site)] %>%
  .[, Year := data.table::year(Date)] %>% 
  .[, c("Zone", "Date") := NULL] %>% 
  .[, Price_mean := fcase(Var %in% "WheatYield", 347, Var %in% "CanolaYield", 640, Var %in% "BarleyYield", 281, Var %in% "ChickpeaYield", 513, Var %in% "OatsYield", 301)] %>% 
  .[, Price_sd := fcase(Var %in% "WheatYield", 61, Var %in% "CanolaYield", 127, Var %in% "BarleyYield", 41, Var %in% "ChickpeaYield", 183, Var %in% "OatsYield", 103)] %>% 
  .[, Cost_mean := fcase(Var %in% "WheatYield", 601, Var %in% "CanolaYield", 613, Var %in% "BarleyYield", 546, Var %in% "ChickpeaYield", 524, Var %in% "OatsYield", 391)] %>% 
  .[, Cost_sd := fcase(Var %in% "WheatYield", 148, Var %in% "CanolaYield", 84, Var %in% "BarleyYield", 137, Var %in% "ChickpeaYield", 77, Var %in% "OatsYield", 95)] %>% 
  .[, c("profit", "profit_max", "profit_min") := list(Yield / 1000 * Price_mean - Cost_mean, (Yield / 1000) * (Price_mean + Price_sd) - (Cost_mean - Cost_sd), (Yield / 1000) * (Price_mean - Price_sd) - (Cost_mean + Cost_sd))]  

head(data_all)
uniq_all <- unique(data_all[, .(site, State, SimulationName)])

row <- 1
res <- purrr::map_dfr(.x = 1:nrow(uniq_all), .f = function(row, data_all, uniq_all){
  single <- data_all[uniq_all[row], on = .(site, State, SimulationName)]
  floor <- nrow(single) %/% 4
  duo <- nrow(single) %% 4
  
  if (duo == 0) {
    single$rotation <- c(rep(1:floor, 4) %>% sort())
  } else {
    single$rotation <- c(rep(1:floor, 4) %>% sort(), (1:4)[1:duo])
  }
  
  res <- single[, .(profit = mean(profit), profit_max = mean(profit_max), profit_min = mean(profit_min)), by = .(site, State, SimulationName, rotation)]
  return(res)
}, data_all, uniq_all)

res

data_plot_all <- data.table::melt.data.table(data = res, id.vars = c("site", "State", "SimulationName", "rotation"), variable.name = "Profit", value.name = "Price")
head(data_plot_all)
res_profit <- data_plot_all[, .(Median = quantile(x = Price, 0.5)), by = .(State, SimulationName)] %>% 
  data.table::setnames("Median", "WL_Profit")

## Do not open waterlogging profit
data_all <- config[, .(site, State)] %>% 
  .[data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation_5.csv"), select = c("site", "Zone", "SimulationName", "Date", "Var", "Yield")) %>% .[Zone %in% "Field"], on = .(site)] %>%
  .[, Year := data.table::year(Date)] %>% 
  .[, c("Zone", "Date") := NULL] %>% 
  .[, Price_mean := fcase(Var %in% "WheatYield", 347, Var %in% "CanolaYield", 640, Var %in% "BarleyYield", 281, Var %in% "ChickpeaYield", 513, Var %in% "OatsYield", 301)] %>% 
  .[, Price_sd := fcase(Var %in% "WheatYield", 61, Var %in% "CanolaYield", 127, Var %in% "BarleyYield", 41, Var %in% "ChickpeaYield", 183, Var %in% "OatsYield", 103)] %>% 
  .[, Cost_mean := fcase(Var %in% "WheatYield", 601, Var %in% "CanolaYield", 613, Var %in% "BarleyYield", 546, Var %in% "ChickpeaYield", 524, Var %in% "OatsYield", 391)] %>% 
  .[, Cost_sd := fcase(Var %in% "WheatYield", 148, Var %in% "CanolaYield", 84, Var %in% "BarleyYield", 137, Var %in% "ChickpeaYield", 77, Var %in% "OatsYield", 95)] %>% 
  .[, c("profit", "profit_max", "profit_min") := list(Yield / 1000 * Price_mean - Cost_mean, (Yield / 1000) * (Price_mean + Price_sd) - (Cost_mean - Cost_sd), (Yield / 1000) * (Price_mean - Price_sd) - (Cost_mean + Cost_sd))] 

head(data_all)
uniq_all <- unique(data_all[, .(site, State, SimulationName)])

row <- 1
res <- purrr::map_dfr(.x = 1:nrow(uniq_all), .f = function(row, data_all, uniq_all){
  single <- data_all[uniq_all[row], on = .(site, State, SimulationName)]
  floor <- nrow(single) %/% 4
  duo <- nrow(single) %% 4
  
  if (duo == 0) {
    single$rotation <- c(rep(1:floor, 4) %>% sort())
  } else {
    single$rotation <- c(rep(1:floor, 4) %>% sort(), (1:4)[1:duo])
  }
  
  res <- single[, .(profit = mean(profit), profit_max = mean(profit_max), profit_min = mean(profit_min)), by = .(site, State, SimulationName, rotation)]
  return(res)
}, data_all, uniq_all)

res

data_plot_all <- data.table::melt.data.table(data = res, id.vars = c("site", "State", "SimulationName", "rotation"), variable.name = "Profit", value.name = "Price")
head(data_plot_all)
res_Field_profit <- data_plot_all[, .(Median = quantile(x = Price, 0.5)), by = .(State, SimulationName)] %>% 
  data.table::setnames("Median", "Field_Profit")

## Combine the results and calculate GHGI.
res <- res_yield_NUE[res_CO2, on = .(State, SimulationName)] %>% 
  .[res_yield, on = .(State, SimulationName)] %>% 
  .[yield_cv, on = .(State, SimulationName)] %>%
  .[res_energy, on = .(State, SimulationName)] %>%
  .[res_NO3Lost, on = .(State, SimulationName)] %>% 
  .[res_GHG, on = .(State, SimulationName)] %>% 
  .[res_wr, on = .(State, SimulationName)] %>% 
  .[res_profit, on = .(State, SimulationName)] %>% 
  .[res_Field_profit, on = .(State, SimulationName)] %>% 
  .[, GHGI := mean_GHG / mean_yield]
res
# openxlsx::write.xlsx(res, stringr::str_glue("{dir_work}res/WL_Ratation_Radar chart data.xlsx"))

# devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)
require(ggradar)

data <- openxlsx::read.xlsx(stringr::str_glue("{dir_work}res/WL_Ratation_Radar chart data.xlsx")) %>% 
  data.table::setDT() %>% 
  .[, .(State, SimulationName, Yield, Profit, NUE, Yield.CV, Energy.footprint, GHGI, WL.penalty)] %>% 
  data.table::setnames(c("Energy.footprint", "Yield.CV", "WL.penalty"), c("EF", "YS", "WL resilience"))
head(data)
unique(data$State)

# cairo_pdf(file = stringr::str_glue("{dir_work}res/Radar_origin.pdf"), width = 18, height = 18, family = "Calibri", onefile = T)
unique(data$State)
### Western Australia
i <- 1
for (i in 1:length(unique(data$State))) {
  df <- data[State %in% unique(data$State)[i]]
  df_s <- cbind(df[, 2], apply(df[, ! 1:2], 2, scale, center = F))
  df_s$SimulationName <- factor(df_s$SimulationName, levels = unique(df_s$SimulationName), labels = stringr::str_split(unique(df_s$SimulationName), "_")  %>% purrr::map_vec(.f = function(x) {stringr::str_sub(x, start = 1L, end = 1L) %>% paste0(collapse = "")}))
  
  if (i == 1) {
    p <- ggradar(plot.data = df_s,
                 base.size = 60,
                 axis.label.size = 18,
                 grid.label.size = 15,
                 group.point.size = 6,
                 group.line.width = 1.1,
                 # group.colours = viridis::viridis(n = 50, alpha = 1, begin = 0, end = 1, direction = 1, option = "A") %>% .[c(10, 30, 20, 40, 47)],
                 group.colours = c(rgb(255, 101, 1, maxColorValue = 255), rgb(1, 176, 81, maxColorValue = 255), rgb(113, 49, 160, maxColorValue = 255), rgb(0, 102, 255, maxColorValue = 255), rgb(0, 30, 94, maxColorValue = 255)),
                 fill = T,
                 fill.alpha = .1,
                 legend.text.size = 45,
                 legend.position = "bottom",
                 plot.title = unique(data$State)[i],
                 background.circle.colour = NA,
                 gridline.min.colour = "grey45",
                 gridline.mid.colour="grey45",
                 gridline.max.colour="grey45")
  } else {
    p <- ggradar(plot.data = df_s,
                 base.size = 60,
                 axis.label.size = 18,
                 grid.label.size = 15,
                 group.point.size = 6,
                 group.line.width = 1.1,
                 # group.colours = viridis::viridis(n = 50, alpha = 1, begin = 0, end = 1, direction = 1, option = "A") %>% .[c(10, 30, 20, 40, 47)],
                 group.colours = c(rgb(255, 101, 1, maxColorValue = 255), rgb(113, 49, 160, maxColorValue = 255), rgb(1, 176, 81, maxColorValue = 255), rgb(0, 102, 255, maxColorValue = 255), rgb(0, 30, 94, maxColorValue = 255)),
                 fill = T,
                 fill.alpha = .1,
                 legend.text.size = 45,
                 legend.position = "bottom",
                 plot.title = unique(data$State)[i],
                 background.circle.colour = NA,
                 gridline.min.colour = "grey45",
                 gridline.mid.colour="grey45",
                 gridline.max.colour="grey45")
  }
  print(p)
}

dev.off()

### NUE/N lost/GHGI box plot
#### NUE
data_all <- config[data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation_5.csv"), select = c("site", "Zone", "SimulationName", "Yield")) %>% .[Zone %in% "Field_WL"], on = .(site)]
head(data_all)
uniq_all <- unique(data_all[, .(site, State, SimulationName)])

##### yield and NUE
row <- 1
res_NUE <- purrr::map_dfr(.x = 1:nrow(uniq_all), .f = function(row, data_all, uniq_all){
  single <- data_all[uniq_all[row], on = .(site, State, SimulationName)]
  floor <- nrow(single) %/% 4
  duo <- nrow(single) %% 4
  
  if (duo == 0) {
    single$rotation <- c(rep(1:floor, 4) %>% sort())
  } else {
    single$rotation <- c(rep(1:floor, 4) %>% sort(), (1:4)[1:duo])
  }
  
  res <- single[, .(mean_yield = mean(Yield)), by = .(site, State, SimulationName, rotation)]
  return(res)
}, data_all, uniq_all)

res_NUE$NUE <- res_NUE$mean_yield / 100
res_NUE$SimulationName <- factor(res_NUE$SimulationName, levels = unique(res_NUE$SimulationName), labels = stringr::str_split(unique(res_NUE$SimulationName), "_")  %>% purrr::map_vec(.f = function(x) {stringr::str_sub(x, start = 1L, end = 1L) %>% paste0(collapse = "")}))
head(res_NUE)

data_plot <- res_NUE[, .(NUE = mean(NUE)), by = .(site, State, SimulationName)]
data_plot
# cairo_pdf(file = stringr::str_glue("{dir_work}res/NUE box diagram by state.pdf"), width = 12, height = 16, family = "Calibri", onefile = T)
ggplot(data = data_plot, mapping = aes(x = reorder(SimulationName, NUE, decreasing = T), y = NUE, fill = SimulationName))+
  facet_wrap(State ~ ., scales = "free_x")+
  geom_jitter(size = 2, shape = 21, mapping = aes(fill = SimulationName))+
  geom_boxplot(outliers = F, alpha = .8, width = .5)+
  scale_fill_manual(values = viridis::viridis(n = 18, alpha = 1, begin = 0, end = 1, direction = 1, option = "A") %>% .[5:18])+
  guides(fill = "none")+
  labs(x = "Rotations")+
  theme_huhu+
  theme(axis.text.x = element_text(size = 27, angle = 90, vjust = .5, hjust = 1))

dev.off()

#### N lost
res_NO3Lost <- config[data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation_5.csv"), select = c("site", "Zone", "SimulationName", "sumNO3Lost_kg_ha")) %>% .[Zone %in% "Field_WL"], on = .(site)]

res_NO3Lost$SimulationName <- factor(res_NO3Lost$SimulationName, levels = unique(res_NO3Lost$SimulationName), labels = stringr::str_split(unique(res_NO3Lost$SimulationName), "_")  %>% purrr::map_vec(.f = function(x) {stringr::str_sub(x, start = 1L, end = 1L) %>% paste0(collapse = "")}))

data_plot <- res_NO3Lost[, .(sumNO3Lost_kg_ha = mean(sumNO3Lost_kg_ha)), by = .(site, State, SimulationName)]
data_plot
# cairo_pdf(file = stringr::str_glue("{dir_work}res/N lost box diagram by state.pdf"), width = 12, height = 16, family = "Calibri", onefile = T)
ggplot(data = data_plot, mapping = aes(x = reorder(SimulationName, sumNO3Lost_kg_ha, decreasing = T), y = sumNO3Lost_kg_ha, fill = SimulationName))+
  facet_wrap(State ~ ., scales = "free_x")+
  geom_jitter(size = 2, shape = 21, mapping = aes(fill = SimulationName))+
  geom_boxplot(outliers = F, alpha = .8, width = .5)+
  scale_fill_manual(values = viridis::viridis(n = 18, alpha = 1, begin = 0, end = 1, direction = 1, option = "A") %>% .[5:18])+
  guides(fill = "none")+
  labs(x = "Rotations", y = "N Lost (kg/ha)")+
  theme_huhu+
  theme(axis.text.x = element_text(size = 27, angle = 90, vjust = .5, hjust = 1))

dev.off()

#### GHGI
data_all <- config[, .(site, lon, lat, State)] %>% 
  .[data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation_5.csv"), select = c("site", "Zone", "SimulationName", "Date", "CO2_ton", "CO2_N2O")) %>% .[Zone %in% "Field_WL"], on = .(site)] %>%
  .[, Year := data.table::year(Date)] %>%  
  .[, .(CO2_ton = mean(CO2_ton), CO2_N2O = mean(CO2_N2O)), by = .(site, State, SimulationName, Year)]
head(data_all)

res_GHG <- data_all %>% 
  .[, .(delta_CO2_ton = CO2_ton[.N] - CO2_ton[1], CO2_N2O = mean(CO2_N2O)), by = .(site, State, SimulationName)] %>% 
  .[, mean_GHG := CO2_N2O - delta_CO2_ton * 44 / 12]
res_GHG

data_all <- config[data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation_5.csv"), select = c("site", "Zone", "SimulationName", "Yield")) %>% .[Zone %in% "Field_WL"], on = .(site)]
head(data_all)
uniq_all <- unique(data_all[, .(site, State, SimulationName)])

row <- 1
res_yield <- purrr::map_dfr(.x = 1:nrow(uniq_all), .f = function(row, data_all, uniq_all){
  single <- data_all[uniq_all[row], on = .(site, State, SimulationName)]
  floor <- nrow(single) %/% 4
  duo <- nrow(single) %% 4
  
  if (duo == 0) {
    single$rotation <- c(rep(1:floor, 4) %>% sort())
  } else {
    single$rotation <- c(rep(1:floor, 4) %>% sort(), (1:4)[1:duo])
  }
  
  res <- single[, .(mean_yield = mean(Yield)), by = .(site, State, SimulationName, rotation)]
  return(res)
}, data_all, uniq_all)

res_yield <- res_yield[, .(mean_yield = mean(mean_yield)), by = .(site, State, SimulationName)]
res_yield

res_GHGI <- res_yield[res_GHG, on = .(site, State, SimulationName)] %>% 
  .[, GHGI := mean_GHG / mean_yield] %>% 
  .[! GHGI > 1.5]
res_GHGI
res_GHGI[, .(mean_GHGI = mean(GHGI) %>% round(2)), by = .(State, SimulationName)]
res_GHGI$SimulationName <- factor(res_GHGI$SimulationName, levels = unique(res_GHGI$SimulationName), labels = stringr::str_split(unique(res_GHGI$SimulationName), "_")  %>% purrr::map_vec(.f = function(x) {stringr::str_sub(x, start = 1L, end = 1L) %>% paste0(collapse = "")}))
plot(res_GHGI[State %in% "South Australia"]$GHGI)

data_plot <- res_GHGI[, .(GHGI = mean(GHGI)), by = .(site, State, SimulationName)]
data_plot
# cairo_pdf(file = stringr::str_glue("{dir_work}res/GHGI box diagram by state.pdf"), width = 23.3, height = 8, family = "Calibri", onefile = T)
ggplot(data = data_plot, mapping = aes(x = reorder(SimulationName, GHGI, FUN = median, decreasing = T), y = GHGI))+
  facet_wrap(State ~ ., scales = "free_x", nrow = 1)+
  geom_jitter(size = 2, shape = 21, fill = rgb(253, 155, 107, maxColorValue = 255), alpha = .9, color = "white")+
  geom_boxplot(outliers = F, alpha = 1, width = .5, fill = rgb(253, 155, 107, maxColorValue = 255))+
  # scale_y_continuous(limits = c(-0.5, 1))+
  # scale_fill_manual(values = viridis::viridis(n = 18, alpha = 1, begin = 0, end = 1, direction = 1, option = "A") %>% .[5:18])+
  guides(fill = "none")+
  labs(x = "Crop rotations", y = "GHGI")+
  theme_huhu+
  theme(axis.text.x = element_text(size = 27, angle = 90, vjust = .5, hjust = 1))

dev.off()

# 8 waterlogging stress

res_origin <- data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation_FOX_5.csv"), select = c("site", "SimulationName", "Date", "WheatYield", "CanolaYield",  "BarleyYield", "ChickpeaYield", "OatsYield", "Wheat.Leaf.Photosynthesis.FOX", "Canola.Leaf.Photosynthesis.FOX", "Barley.Leaf.Photosynthesis.FOX", "Chickpea.Leaf.Photosynthesis.FOX", "Oats.Leaf.Photosynthesis.FOX", "daily_tt_wheat", "daily_tt_canola", "daily_tt_barley", "daily_tt_chickpea", "daily_tt_oats", "Wheat.Phenology.CurrentStageName", "Canola.Phenology.CurrentStageName", "Barley.Phenology.CurrentStageName", "Chickpea.Phenology.CurrentStageName", "Oats.Phenology.CurrentStageName"))
head(res_origin)

uniq_all <- unique(res_origin[, .(site, SimulationName)])
res <- purrr::map_dfr(.x = 1:nrow(uniq_all), .f = function(row, res_origin, uniq_all){
  single <- res_origin[uniq_all[row], on = .(site, SimulationName)] %>% 
    .[, year := data.table::year(Date)]
  correct_crop <- data.table(year = unique(single$year) %>% sort(), crop = stringr::str_split(unique(single$SimulationName), "_") %>% unlist()) %>% 
    .[, crop := ifelse(crop == "Fababean", "Chickpea", crop)]

  single_correct <- correct_crop[single, on = .(year)]
  
  res_year <- purrr::map_dfr(.x = unique(single$year) %>% sort(), .f = function(year_cur, single_correct){
    single_year <- single_correct[year %in% year_cur]
    crop <- unique(single_year$crop)
    res_data <- single_year[, c("year", "crop", "site", "SimulationName", "Date", paste0(crop, "Yield"), paste0("daily_tt_", stringr::str_to_lower(crop)), paste0(crop, ".Leaf.Photosynthesis.FOX"), paste0(crop, ".Phenology.CurrentStageName")), with = F] %>% 
      data.table::setnames(c(paste0(crop, "Yield"), paste0("daily_tt_", stringr::str_to_lower(crop)), paste0(crop, ".Leaf.Photosynthesis.FOX"), paste0(crop, ".Phenology.CurrentStageName")), c("Yield", "daily_tt", "fox", "stage"))

    if_sowing <- which(res_data$stage == "Sowing") %>% 
      length()
    if_harvest <- which(res_data$stage == "HarvestRipe") %>%
      length()

    if(if_sowing == 0){
      res <- data.table::data.table(year = NA, crop = NA, site = NA, SimulationName = NA, Date = as.Date("1961-1-1"), Yield = NA, daily_tt = NA, fox = NA, stage = NA)
    } else if(if_sowing == 1) {
      if(if_harvest == 0){
        res <- res_data[which(res_data$stage == "Sowing"):nrow(res_data)] %>% 
          .[, Date := as.Date(Date)]
      } else if(if_harvest == 1){
        res <- res_data[which(res_data$stage == "Sowing"):which(res_data$stage == "HarvestRipe")] %>% 
          .[, Date := as.Date(Date)]
      } else if(if_harvest == 2){
        res <- res_data[which(res_data$stage == "Sowing"):(which(res_data$stage == "HarvestRipe") %>% .[2])] %>% 
          .[, Date := as.Date(Date)]
      }
    }

    return(res)
  }, single_correct) %>% 
  na.omit()

  return(res_year)
}, res_origin, uniq_all)
data.table::fwrite(res, stringr::str_glue("{dir_work}res/WL_Ratation_FOX_5_extract.csv"))

data_all <- data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation_FOX_5_extract.csv"))
head(data_all)
uniq_all <- unique(data_all[, .(site, SimulationName)])

season_cut <- seq(-1050, 650, by = 100)
season_break<- seq(-1000, 650, by = 100)

res <- purrr::map_dfr(.x = 1:nrow(uniq_all), .f = function(row, data_all, uniq_all){
  single <- data_all[uniq_all[row], on = .(site, SimulationName)]

  res_year <- purrr::map_dfr(.x = unique(single$year), .f = function(Year, single){
    single_year <- single[year %in% Year] %>% 
      .[, tt := cumsum(daily_tt)] 

    if(unique(single_year$crop) == "Wheat"){
      if_flower <- which(single_year$stage == "Anthesis") %>%
        length()
      if(length(if_flower)){
        single_year[, tt := tt - single_year[140]$tt]
      } else {
        single_year[, tt := tt - single_year[which(single_year$stage == "Anthesis")]$tt]
      }
    } else if(unique(single_year$crop) %in% c("Chickpea", "Canola")){
      if_flower <- which(single_year$stage == "StartFlowering") %>%
        length()
      if(length(if_flower)){
        single_year[, tt := tt - single_year[140]$tt]
      } else {
        single_year[, tt := tt - single_year[which(single_year$stage == "StartFlowering")]$tt]
      }
    } else if(unique(single_year$crop) %in% c("Oats", "Barley")){
      if_flower <- which(single_year$stage == "Flowering") %>%
        length()
      if(length(if_flower)){
        single_year[, tt := tt - single_year[140]$tt]
      } else {
        single_year[, tt := tt - single_year[which(single_year$stage == "Flowering")]$tt]
      }
    }

    ttcut <- cut(single_year$tt, breaks = season_cut)
    res <- tapply(single_year$fox, ttcut, mean, na.rm = T) %>% 
      data.frame() %>% 
      data.table::data.table() %>% 
      .[, c("site", "year", "crop", "SimulationName", "tt", "yield") := list(single_year$site[1], single_year$year[1], single_year$crop[1], single_year$SimulationName[1], season_break, single_year[which.max(single_year$Yield)]$Yield)] %>% 
      data.table::setnames(".", "fox")
    
    return(res)
  }, single)

  res <- res_year %>% 
    data.table::dcast.data.table(formula = site + year + SimulationName ~ tt, value.var = c("fox", "yield")) %>% 
    .[, 1:21] %>% 
    .[, lapply(.SD, function(x)(mean(x, na.rm = T))), .SDcols = names(.)[-c(1:3)], by = .(site)]

  return(res)
}, data_all, uniq_all) %>% 
  na.omit()
res
data.table::fwrite(res, stringr::str_glue("{dir_work}res/WL_Ratation_FOX_5_data-means.csv"))

## K-means
data <- data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation_FOX_5_data-means.csv"))
data_kmeans <- data[, lapply(.SD, mean), .SDcols = names(data)[-1], by = .(site)]
data_kmeans

### Australia-wide K-means clustering test: 2:10
res_kmeans_test <- purrr::map_dfr(.x = 2:10, .f = function(k, data_kmeans){
  res_k <- kmeans(data_kmeans[, 2:18], centers = k)
  res <- data.table::data.table(cluster = k, `Percentage variance accounted for (%)` = round(res_k$betweenss / res_k$totss * 100, 2))
  return(res)
}, data_kmeans)
res_kmeans_test

# cairo_pdf(file = stringr::str_glue("{dir_work}res/Kmeans test of waterlogging pressure in Australia.pdf"), width = 8, height = 8, family = "Calibri", onefile = T)
ggplot(data = res_kmeans_test, mapping = aes(x = cluster, y = `Percentage variance accounted for (%)`))+
  geom_line(linewidth = 1, color = viridis::viridis(n = 10, alpha = 1, begin = 0, end = 1, direction = 1, option = "A") %>% .[5])+
  geom_point(size = 3, shape = 21, fill = viridis::viridis(n = 10, alpha = 1, begin = 0, end = 1, direction = 1, option = "A") %>% .[6], alpha = 1)+
  scale_y_continuous(breaks = seq(60, 100, 10))+
  labs(x = "No. of clusters")+
  theme_test()+
  theme(axis.title = element_text(size = 30), 
        axis.text = element_text(size = 25, color = "black"),
        strip.text = element_text(size = 20),
        axis.ticks = ggplot2::element_line(linewidth = 1),
        axis.ticks.length = unit(0.3, "cm"),
        panel.border = ggplot2::element_rect(linewidth = 1.2),
        strip.background = ggplot2::element_rect(fill = "#F5F5F5", linewidth = 1, colour = "black"),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"))

dev.off()


theme_kmeans <-   
  theme_test()+
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 20, color = "black"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 25),
        legend.position = c(0.22, 0.25),
        strip.text = element_text(size = 20),
        axis.ticks = ggplot2::element_line(linewidth = 1),
        axis.ticks.length = unit(0.3, "cm"),
        panel.border = ggplot2::element_rect(linewidth = 1.2),
        strip.background = ggplot2::element_rect(fill = "#F5F5F5", linewidth = 1, colour = "black"),
        plot.margin = margin(t = 1, r = 3, b = 1, l = 1, unit = "cm"))

# cairo_pdf(file = stringr::str_glue("{dir_work}res/Kmeans Test of Waterlogging Pressure in Australia _cluster3-4-5.pdf"), width = 12, height = 12, family = "Calibri", onefile = T)
res_k <- kmeans(data_kmeans[, 2:18], centers = 3)
round(res_k$betweenss / res_k$totss * 100, 2)
data_kmeans$cluster <- res_k$cluster
N_cluster <- data_kmeans[, .N, by = .(cluster)]
N_cluster

test_plot <- data_kmeans[, lapply(.SD, mean), .SDcols = names(data_kmeans)[-c(1, 19, 20)], by = .(cluster)] %>% 
  data.table::melt.data.table(id.vars = c("cluster"), variable.name = "tt", value.name = "fox") %>% 
  .[, tt := stringr::str_remove(tt, "fox_") %>% as.numeric()]

a <- sprintf("stress1 = %1.f%%", round(N_cluster[cluster %in% 1]$N / sum(N_cluster$N) * 100, 0))
b <- sprintf("stress2 = %1.f%%", round(N_cluster[cluster %in% 2]$N / sum(N_cluster$N) * 100, 0))
c <- sprintf("stress3 = %1.f%%", round(N_cluster[cluster %in% 3]$N / sum(N_cluster$N) * 100, 0))

require(ggplot2)
ggplot(data = test_plot, mapping = aes(x = tt, y = fox))+
  geom_line(mapping = aes(color = factor(cluster)), size = 1.5)+
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1.5)+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))+
  scale_x_continuous(breaks = seq(-900, 600, by = 300))+
  scale_color_manual(values = viridis::viridis(n = 11, alpha = 1, begin = 0, end = 1, direction = 1, option = "A") %>% .[c(3, 5, 8)] %>% rev(), name="Frequency", label = c(a, b, c))+
  labs(color = "cluster", x = "TT centered on anthesis (°C day)", y = "Waterlogging stress")+
  theme_kmeans

### cluster=4
res_k <- kmeans(data_kmeans[, 2:18], centers = 4)
round(res_k$betweenss / res_k$totss * 100, 2)
data_kmeans$cluster <- res_k$cluster
N_cluster <- data_kmeans[, .N, by = .(cluster)]
N_cluster

test_plot <- data_kmeans[, lapply(.SD, mean), .SDcols = names(data_kmeans)[-c(1, 19, 20)], by = .(cluster)] %>% 
  data.table::melt.data.table(id.vars = c("cluster"), variable.name = "tt", value.name = "fox") %>% 
  .[, tt := stringr::str_remove(tt, "fox_") %>% as.numeric()]

a <- sprintf("stress1 = %1.f%%", round(N_cluster[cluster %in% 1]$N / sum(N_cluster$N) * 100, 0))
b <- sprintf("stress2 = %1.f%%", round(N_cluster[cluster %in% 2]$N / sum(N_cluster$N) * 100, 0))
c <- sprintf("stress3 = %1.f%%", round(N_cluster[cluster %in% 3]$N / sum(N_cluster$N) * 100, 0))
d <- sprintf("stress4 = %1.f%%", round(N_cluster[cluster %in% 4]$N / sum(N_cluster$N) * 100, 0))

require(ggplot2)
ggplot(data = test_plot, mapping = aes(x = tt, y = fox))+
  geom_line(mapping = aes(color = factor(cluster)), size = 1.5)+
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1.5)+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))+
  scale_x_continuous(breaks = seq(-900, 600, by = 300))+
  scale_color_manual(values = viridis::viridis(n = 11, alpha = 1, begin = 0, end = 1, direction = 1, option = "A") %>% .[c(1, 3, 5, 8)] %>% rev(), name="Frequency", label = c(a, b, c, d))+
  labs(color = "cluster", x = "TT centered on anthesis (°C day)", y = "Waterlogging stress")+
  theme_kmeans

### cluster=5
res_k <- kmeans(data_kmeans[, 2:18], centers = 5)
round(res_k$betweenss / res_k$totss * 100, 2)
data_kmeans$cluster <- res_k$cluster
N_cluster <- data_kmeans[, .N, by = .(cluster)]
N_cluster

test_plot <- data_kmeans[, lapply(.SD, mean), .SDcols = names(data_kmeans)[-c(1, 19, 20)], by = .(cluster)] %>% 
  data.table::melt.data.table(id.vars = c("cluster"), variable.name = "tt", value.name = "fox") %>% 
  .[, tt := stringr::str_remove(tt, "fox_") %>% as.numeric()]

a <- sprintf("stress1 = %1.f%%", round(N_cluster[cluster %in% 1]$N / sum(N_cluster$N) * 100, 0))
b <- sprintf("stress2 = %1.f%%", round(N_cluster[cluster %in% 2]$N / sum(N_cluster$N) * 100, 0))
c <- sprintf("stress3 = %1.f%%", round(N_cluster[cluster %in% 3]$N / sum(N_cluster$N) * 100, 0))
d <- sprintf("stress4 = %1.f%%", round(N_cluster[cluster %in% 4]$N / sum(N_cluster$N) * 100, 0))
e <- sprintf("stress5 = %1.f%%", round(N_cluster[cluster %in% 5]$N / sum(N_cluster$N) * 100, 0))

require(ggplot2)
ggplot(data = test_plot, mapping = aes(x = tt, y = fox))+
  geom_line(mapping = aes(color = factor(cluster)), size = 1.5)+
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1.5)+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))+
  scale_x_continuous(breaks = seq(-900, 600, by = 300))+
  scale_color_manual(values = viridis::viridis(n = 11, alpha = 1, begin = 0, end = 1, direction = 1, option = "A") %>% .[c(1, 3, 5, 8, 10)] %>% rev(), name="Frequency", label = c(a, b, c, d, e))+
  labs(color = "cluster", x = "TT centered on anthesis (°C day)", y = "Waterlogging stress")+
  theme_kmeans

dev.off()

data <- data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation_FOX_5_data-means.csv"))
data_kmeans <- data[, lapply(.SD, mean), .SDcols = names(data)[-1], by = .(site)]
data_kmeans

set.seed(123)
res_k <- kmeans(data_kmeans[, 2:18], centers = 4)
round(res_k$betweenss / res_k$totss * 100, 2)
data_kmeans$Cluster <- res_k$cluster
data_kmeans[, cluster := fcase(Cluster == 1, 1, Cluster == 2, 2, Cluster == 3, 4, Cluster == 4, 3)]
data_kmeans[, Cluster := NULL]
data_kmeans
data.table::fwrite(data_kmeans, stringr::str_glue("{dir_work}res/WL_Ratation_FOX_5_data-means_cluster.csv"))

data_kmeans <- data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation_FOX_5_data-means_cluster.csv"))
N_cluster <- data_kmeans[, .N, by = .(cluster)]
N_cluster

data_map_plot <- config[data_kmeans, on = .(site)] %>% 
  sp2sf(longitude = "lon", latitude = "lat", index = "cluster")
data_map_plot$cluster <- as.factor(data_map_plot$cluster)

# cairo_pdf(file = stringr::str_glue("{dir_work}res/Kmeans classification map of waterlogging stress.pdf"), width = 24, height = 20, family = "Calibri", onefile = T)
ggplot() +
  geom_sf(data = data_map_plot, mapping = aes(fill = cluster), color = "grey", size = 9.4, shape = 22) +
  geom_sf(data = map, fill = "transparent", linewidth = 1) +
  # linear scale
  ggspatial::annotation_scale(location = "bl", text_cex = 2, pad_x = unit(2.3, "cm"), pad_y = unit(5.6, "cm")) +
  # compass
  ggspatial::annotation_north_arrow(location = "tr", pad_x = unit(3.2, "cm"), pad_y = unit(2.6, "cm"), which_north = "false", style = ggspatial::north_arrow_orienteering(text_size = 40, line_width = 2)) +
  scale_fill_manual(values = c("1" = "#0C0927FF", "2" = "#B63679FF", "3" = "#FD9969FF", "4" = "#FDDC9EFF")) +
  guides(fill = guide_legend(override.aes = list(size = 8)))+
  labs(fill = "Cluster")+
  theme_void() +
  theme(plot.title = element_text(size = 30), plot.subtitle = element_text(size = 30), legend.title = element_text(size = 35), legend.text = element_text(size = 30), legend.position = c(0.12, 0.22), legend.direction = "horizontal", legend.title.position = "top", plot.margin = margin(t = .1, r = .1, b = .1, l = .1, unit = "cm"), legend.key.width = unit(2, "cm"), strip.text = element_text(size = 55)) # legend.position = c(0.3, 0.12

dev.off()

##### Kmeans classification histogram
N_cluster$cluster <- as.factor(N_cluster$cluster)

# cairo_pdf(file = stringr::str_glue("{dir_work}res/Kmeans classification histogram of waterlogging trajectory.pdf"), width = 6, height = 10, family = "Calibri", onefile = T)
ggplot(data = N_cluster, mapping = aes(x = cluster, y = N, fill = cluster))+
  geom_col(color = "black", width = .7)+
  geom_text(mapping = aes(x = cluster, y = N + 5, label = N), size = 10)+
  scale_y_continuous(expand = c(0.01, 0.01), limits = c(0, 115))+
  guides(fill = "none")+
  labs(x = "Cluster", y = "Number of records")+
  scale_fill_manual(values = c("1" = "#0C0927FF", "2" = "#B63679FF", "3" = "#FD9969FF", "4" = "#FDDC9EFF")) +
  theme_test()+
  theme(axis.title = element_text(size = 45),
        axis.text = element_text(size = 40, color = "black"),
        strip.text = element_text(size = 20),
        axis.ticks = ggplot2::element_line(linewidth = 1),
        axis.ticks.length = unit(0.3, "cm"),
        panel.border = ggplot2::element_rect(linewidth = 1),
        strip.background = ggplot2::element_rect(fill = "#F5F5F5", linewidth = 1, colour = "black"),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"))

dev.off()

#### Waterlogging stress in Australia and different States
# cairo_pdf(file = stringr::str_glue("{dir_work}res/Kmeans-4_ Waterlogging stress _origin in Australia and States.pdf"), width = 7, height = 6, family = "Calibri", onefile = T)
##### 全澳
test_plot <- data_kmeans[, lapply(.SD, mean), .SDcols = names(data_kmeans)[-c(1, 19, 20)], by = .(cluster)] %>% 
  data.table::melt.data.table(id.vars = c("cluster"), variable.name = "tt", value.name = "fox") %>% 
  .[, tt := stringr::str_remove(tt, "fox_") %>% as.numeric()] %>% 
  .[, State := "Australia"]

a <- sprintf("stress1 = %1.f%%", round(N_cluster[cluster %in% 1]$N / sum(N_cluster$N) * 100, 0))
b <- sprintf("stress2 = %1.f%%", round(N_cluster[cluster %in% 2]$N / sum(N_cluster$N) * 100, 0))
c <- sprintf("stress3 = %1.f%%", round(N_cluster[cluster %in% 3]$N / sum(N_cluster$N) * 100, 0))
d <- sprintf("stress4 = %1.f%%", round(N_cluster[cluster %in% 4]$N / sum(N_cluster$N) * 100, 0))

p <- ggplot(data = test_plot, mapping = aes(x = tt, y = fox))+
  facet_grid(. ~ State)+
  geom_line(mapping = aes(color = factor(cluster)), size = 1)+
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1)+
  scale_y_continuous(limits = c(0.6, 1), breaks = seq(0, 1, 0.2))+
  scale_x_continuous(breaks = seq(-900, 600, by = 300))+
  scale_color_manual(values = c("1" = "#0C0927FF", "2" = "#B63679FF", "3" = "#FD9969FF", "4" = "#FDDC9EFF"), name="Frequency", label = c(a, b, c, d))+
  labs(color = "cluster", x = "TT centered on anthesis (°C day)", y = "Waterlogging stress")+
  theme_kmeans
print(p)

##### 分州
data_kmeans_all <- config[data_kmeans, on = .(site)]
i <- unique(data_kmeans_all$State)[1]
for (i in unique(data_kmeans_all$State)) {
  data_kmeans <- data_kmeans_all[State %in% i] %>% 
    .[, c("lon", "lat", "State") := NULL]
  N_cluster <- data_kmeans[, .N, by = .(cluster)]
  N_cluster
  
  test_plot <- data_kmeans[, lapply(.SD, mean), .SDcols = names(data_kmeans)[-c(1, 19, 20)], by = .(cluster)] %>% 
    data.table::melt.data.table(id.vars = c("cluster"), variable.name = "tt", value.name = "fox") %>% 
    .[, tt := stringr::str_remove(tt, "fox_") %>% as.numeric()] %>% 
    .[, State := i]
  
  a <- sprintf("stress1 = %1.f%%", round(N_cluster[cluster %in% 1]$N / sum(N_cluster$N) * 100, 0))
  b <- sprintf("stress2 = %1.f%%", round(N_cluster[cluster %in% 2]$N / sum(N_cluster$N) * 100, 0))
  c <- sprintf("stress3 = %1.f%%", round(N_cluster[cluster %in% 3]$N / sum(N_cluster$N) * 100, 0))
  d <- sprintf("stress4 = %1.f%%", round(N_cluster[cluster %in% 4]$N / sum(N_cluster$N) * 100, 0))
  
  p <- ggplot(data = test_plot, mapping = aes(x = tt, y = fox))+
    facet_grid(. ~ State)+
    geom_line(mapping = aes(color = factor(cluster)), size = 1)+
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1)+
    scale_y_continuous(limits = c(0.6, 1), breaks = seq(0, 1, 0.2))+
    scale_x_continuous(breaks = seq(-900, 600, by = 300))+
    scale_color_manual(values = c("1" = "#0C0927FF", "2" = "#B63679FF", "3" = "#FD9969FF", "4" = "#FDDC9EFF"), name="Frequency", label = c(a, b, c, d))+
    labs(color = "cluster", x = "TT centered on anthesis (°C day)", y = "Waterlogging stress")+
    theme_kmeans
  print(p)
}

dev.off()

### Cumulative output frequency
# cairo_pdf(file = stringr::str_glue("{dir_work}res/Kmeans-4_ Cumulative output frequency _origin in Australia and by state.pdf"), width = 7, height = 6, family = "Calibri", onefile = T)
data_all <- config[data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation_5.csv"), select = c("site", "Zone", "SimulationName", "Yield")) %>% .[Zone %in% "Field_WL"], on = .(site)]
head(data_all)
uniq_all <- unique(data_all[, .(site, State, SimulationName)])

row <- 1
res <- purrr::map_dfr(.x = 1:nrow(uniq_all), .f = function(row, data_all, uniq_all){
  single <- data_all[uniq_all[row], on = .(site, State, SimulationName)]
  floor <- nrow(single) %/% 4
  duo <- nrow(single) %% 4
  
  if (duo == 0) {
    single$rotation <- c(rep(1:floor, 4) %>% sort())
  } else {
    single$rotation <- c(rep(1:floor, 4) %>% sort(), (1:4)[1:duo])
  }
  
  res <- single[, .(mean_yield = mean(Yield)), by = .(site, State, SimulationName, rotation)]
  return(res)
}, data_all, uniq_all)
res

cluster <- data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation_FOX_5_data-means_cluster.csv")) %>% 
  .[, .(site, cluster)]
cluster

data <- cluster[res, on = .(site)]
data

# Cluster <- unique(data$cluster)[1]
res_yield <- purrr::map_dfr(.x = unique(data$cluster), .f = function(Cluster, data){
  single <- data[cluster %in% Cluster]
  # Extend range of density estimate beyond data
  e <- 0.3 * diff(range(single$mean_yield))
  # Kernel density estimates:
  # dens = density(dat$x, adjust = 1, from=min(dat$x)-e, to=max(dat$x) +e)
  dens <- density(single$mean_yield, adjust = 1, from = 0, to = 6000)
  dens <- data.table::data.table(x = dens$x, y = dens$y) %>%
    .[, c("y2", "cluster") := list(cumsum(y)/sum(y), Cluster)]
  
  return(dens)
}, data)

point <- purrr::map_dfr(.x = unique(res_yield$cluster), .f = function(Cluster, res_yield){
  single <- res_yield[cluster %in% Cluster]
  res <- single[which(abs(single$y2 - 0.5) == min(abs(single$y2 - 0.5))),]
  
  return(res)
}, res_yield)
point

res_yield$State <- "Australia"

res_yield$cluster <- as.factor(res_yield$cluster)
point$cluster <- as.factor(point$cluster)

p <- ggplot(data = res_yield, aes(x = x, y = y2, colour = cluster)) + 
  facet_grid(. ~ State)+
  geom_line(linewidth = 1.2) +
  geom_hline(aes(yintercept = 0.5),linetype = "dotdash", linewidth = 1)+
  geom_point(data = point, aes(x = x, y = y2, colour = cluster), size = 4)+
  guides(color = guide_legend(override.aes = list(size = 5, linewidth = 3)))+
  coord_cartesian(xlim = c(100, 6000))+
  labs(x = "Yield (kg/ha)", y = "Cumulativate yield frequency", color = "Cluster")+
  scale_color_manual(values = c("1" = "#0C0927FF", "2" = "#B63679FF", "3" = "#FD9969FF", "4" = "#FDDC9EFF"),name = "Frequency")+
  theme_kmeans+
  theme(legend.position = c(0.8, 0.2))

print(p)

##### state
data_all <- config[data, on = .(site)]
head(data_all)

# i <- unique(data_all$State)[4]
for (i in unique(data_kmeans_all$State)) {
  data <- data_all[State %in% i] %>% 
    .[, c("lon", "lat") := NULL]
  
  # Cluster <- 1
  res_yield <- purrr::map_dfr(.x = unique(data$cluster), .f = function(Cluster, data){
    single <- data[cluster %in% Cluster]
    # Extend range of density estimate beyond data
    e <- 0.3 * diff(range(single$mean_yield))
    # Kernel density estimates:
    # dens = density(dat$x, adjust = 1, from=min(dat$x)-e, to=max(dat$x) +e)
    dens <- density(single$mean_yield, adjust = 1, from = 0, to = 6000)
    dens <- data.table::data.table(x = dens$x, y = dens$y) %>%
      .[, c("y2", "cluster") := list(cumsum(y)/sum(y), Cluster)]
    
    return(dens)
  }, data)
  
  point <- purrr::map_dfr(.x = unique(res_yield$cluster), .f = function(Cluster, res_yield){
    single <- res_yield[cluster %in% Cluster]
    res <- single[which(abs(single$y2 - 0.5) == min(abs(single$y2 - 0.5))),]
    
    return(res)
  }, res_yield)
  point
  
  res_yield$cluster <- as.factor(res_yield$cluster)
  point$cluster <- as.factor(point$cluster)
  res_yield$State <- i
  
  p <- ggplot(data = res_yield, aes(x = x, y = y2, colour = cluster)) + 
    facet_grid(. ~ State)+
    geom_line(linewidth = 1.2) +
    geom_hline(aes(yintercept = 0.5),linetype = "dotdash", linewidth = 1)+
    geom_point(data = point, aes(x = x, y = y2, colour = cluster), size = 4)+
    guides(color = guide_legend(override.aes = list(size = 5, linewidth = 3)))+
    coord_cartesian(xlim = c(100, 6000))+
    labs(x = "Yield (kg/ha)", y = "Cumulativate yield frequency", color = "Cluster")+
    scale_color_manual(values = c("1" = "#0C0927FF", "2" = "#B63679FF", "3" = "#FD9969FF", "4" = "#FDDC9EFF"),name = "Frequency")+
    theme_kmeans+
    theme(legend.position = c(0.8, 0.2))
  
  print(p)
}

dev.off()

point_AUS <- data.table::copy(point) %>% 
  .[, State := "Aus"]

point_WA <- data.table::copy(point) %>% 
  .[, State := "WA"]

point_SA <- data.table::copy(point) %>% 
  .[, State := "SA"]

point_VIC <- data.table::copy(point) %>% 
  .[, State := "VIC"]

point_NSW <- data.table::copy(point) %>% 
  .[, State := "NSW"]

point <- rbind(point_AUS, point_WA, point_SA, point_VIC, point_NSW) %>% 
  data.table::dcast.data.table(State ~ cluster, value.var = "x") %>% 
  data.table::setnames(c("1", "2", "3", "4"), paste0("Cluster_", c("1", "2", "3", "4")))
point
data.table::fwrite(point, stringr::str_glue("{dir_work}res/WL_Ratation_FOX_5_data-means_cluster_point_0.5.csv"))

##### Calculate the average yield of each waterlogging type.
data <- config[, .(site, State)] %>% 
  .[data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation_FOX_5_data-means_cluster.csv"), select = c("site", "yield_-1000", "cluster")), on = .(site)] %>% 
  .[, .(mean_yield = mean(`yield_-1000`)), by = .(State, cluster)] %>% 
  data.table::setorder(State, cluster)
data

# Box diagram of nitrification and denitrification
data <- config[, .(site, lon, lat, State)] %>% 
  .[data.table::fread(stringr::str_glue("{dir_work}res/WL_Ratation_5.csv"), select = c("site", "Zone", "SimulationName", "Date", "CO2_N2O")) %>% .[Zone %in% "Field_WL"], on = .(site)] %>% 
  .[, .(CO2_N2O = mean(CO2_N2O)), by = .(site, State, SimulationName)]
data

data$SimulationName <- factor(data$SimulationName, levels = unique(data$SimulationName), labels = stringr::str_split(unique(data$SimulationName), "_")  %>% purrr::map_vec(.f = function(x) {stringr::str_sub(x, start = 1L, end = 1L) %>% paste0(collapse = "")}))
head(data)
data[, .(mean_CO2_N2O = round(mean(CO2_N2O), 1)), by = .(State, SimulationName)]

# cairo_pdf(file = stringr::str_glue("{dir_work}res/Box diagram of nitrification and denitrification.pdf"), width = 12, height = 16, family = "Calibri", onefile = T)
ggplot(data = data, mapping = aes(x = reorder(SimulationName, CO2_N2O, decreasing = T), y = CO2_N2O, fill = SimulationName))+
  facet_wrap(State ~ ., scales = "free_x")+
  geom_jitter(size = 2, shape = 21, mapping = aes(fill = SimulationName))+
  geom_boxplot(outliers = F, alpha = .8, width = .5)+
  scale_fill_manual(values = viridis::viridis(n = 18, alpha = 1, begin = 0, end = 1, direction = 1, option = "A") %>% .[5:18])+
  guides(fill = "none")+
  labs(x = "Rotations")+
  theme_huhu+
  theme(axis.text.x = element_text(size = 27, angle = 90, vjust = .5, hjust = 1))

dev.off()

# 4 Functions-------------------------------------------
sp2sf <- function(data, longitude, latitude, index, name = index) {
  data1 <- data.table::copy(data)
  data.sp <- sp::SpatialPointsDataFrame(coords =  data1[, c(longitude, latitude), with = F], data = data1[, index, with = F])
  data.sf <- sf::st_as_sf(x = data.sp)
  data.sf.4326 <- sf::st_set_crs(data.sf, value = "+proj=longlat +datum=WGS84 +no_defs")
  names(data.sf.4326) <- c(name, "geometry")
  
  return(data.sf.4326)
}

min_max_norm <- function(vector){
  return((vector - min(vector)) / (max(vector) - min(vector)))
}

calculate_entropy_weights <- function(data_norm) {
  data_matrix <- as.matrix(data_norm)
  p <- data_matrix / rowSums(data_matrix)
  k <- 1 / log(nrow(data_matrix))
  entropy <- -k * colSums(p * log(p + 1e-6),  na.rm = T) # 避免 log(0)
  weights <- (1 - entropy) / sum(1 - entropy)
  return(weights)
}

calculate_CRITIC_weights <- function(data_norm){
  sigma <- apply(data_norm, 2, sd)
  corr_matrix <- cor(data_norm)
  n <- ncol(data)
  info_content <- sigma * rowSums(1 - corr_matrix)
  weights <- info_content / sum(info_content)
  return(weights)
}







