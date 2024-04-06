


  library(rerddap)
  library(lubridate)
#
#   lizard_ts_DHW <- rerddap::griddap(
#     dataset="NOAA_DHW",
#     time = c('1995-01-01','2024-04-01'),
#     latitude = c(-14.70627, -14.64135),
#     longitude = c(145.43233, 145.47928),
#     fields = c("CRW_DHW","CRW_SST","CRW_SSTANOMALY"),
#     stride = 1,
#     fmt = "csv")
#
#   lizard_ts_DHW_2 <- rerddap::griddap(
#     dataset="NOAA_DHW",
#     time = c('1986-01-01','1995-04-01'),
#     latitude = c(-14.70627, -14.64135),
#     longitude = c(145.43233, 145.47928),
#     fields = c("CRW_DHW","CRW_SST","CRW_SSTANOMALY"),
#     stride = 1,
#     fmt = "csv")
#
#   lizard_DHW <- rbind(lizard_ts_DHW_2, lizard_ts_DHW)
#   write.csv(lizard_DHW, "data/lizard_DHW.csv")

  lizard_DHW <- read.csv("data/lizard_DHW.csv")

  lizard_DHW |>
    mutate(time=ymd_hms(time)) |>
    mutate(year= year(time)) |>
    mutate(month=month(time)) |>
    group_by(time) |>
    summarise(crw_dhw=mean(crw_dhw), crw_sst=max(crw_sst)) |>
  ggplot() + theme_bw() +
    geom_line(aes(time, crw_dhw)) +
    geom_area(aes(time, crw_dhw), fill="darkred") +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(limits=c(0,10), breaks=seq(0,10,2)) +
    ylab("Degree Heating Weeks")+ xlab("Year")

# spatial

lizard_spat <- lizard_DHW |>
    mutate(time=ymd_hms(time)) |>
    mutate(year= year(time)) |>
    group_by(year, longitude, latitude) |>
    summarise(crw_dhw=max(crw_dhw)) |>
    pivot_wider(names_from="year", values_from="crw_dhw") |>
    tidyterra::as_spatraster(crs="EPSG:4326", xycols = 1:2) |> project("EPSG:20353")



gbr_shape_dhw <- readRDS("/Users/rof011/GBR-coral-bleaching/data/gbr_shape_dhw.RDS")



lizard_shape_dhw_map <- gbr_shape_dhw %>%
  st_transform(20353) |>
  mutate(across(where(is.numeric), round, digits = 1)) |>
  filter(grepl('Lizard', gbr_name))



tmpthme <- theme(axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p1998 <- ggplot() + theme_bw() + tidyterra::geom_spatraster(data=lizard_spat, aes(fill=1998), show.legend=FALSE) + scale_fill_viridis_c(option ="C", limits=c(0,10)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + geom_sf(data=lizard_shape_dhw_map)
p2002 <- ggplot() + theme_bw() + tidyterra::geom_spatraster(data=lizard_spat, aes(fill=2002), show.legend=FALSE) + tmpthme + scale_fill_viridis_c(option ="C", limits=c(0,10)) + geom_sf(data=lizard_shape_dhw_map)
p2012 <- ggplot() + theme_bw() + tidyterra::geom_spatraster(data=lizard_spat, aes(fill=2012), show.legend=FALSE) + tmpthme + scale_fill_viridis_c(option ="C", limits=c(0,10)) + geom_sf(data=lizard_shape_dhw_map)
p2016 <- ggplot() + theme_bw() + tidyterra::geom_spatraster(data=lizard_spat, aes(fill=2016), show.legend=FALSE) + tmpthme + scale_fill_viridis_c(option ="C", limits=c(0,10)) + geom_sf(data=lizard_shape_dhw_map)
p2017 <- ggplot() + theme_bw() + tidyterra::geom_spatraster(data=lizard_spat, aes(fill=2017), show.legend=FALSE) + tmpthme + scale_fill_viridis_c(option ="C", limits=c(0,10)) + geom_sf(data=lizard_shape_dhw_map)
p2020 <- ggplot() + theme_bw() + tidyterra::geom_spatraster(data=lizard_spat, aes(fill=2020), show.legend=FALSE) + tmpthme + scale_fill_viridis_c(option ="C", limits=c(0,10)) + geom_sf(data=lizard_shape_dhw_map)
p2022 <- ggplot() + theme_bw() + tidyterra::geom_spatraster(data=lizard_spat, aes(fill=2022), show.legend=FALSE) + tmpthme + scale_fill_viridis_c(option ="C", limits=c(0,10)) + geom_sf(data=lizard_shape_dhw_map)
p2024 <- ggplot() + theme_bw() + tidyterra::geom_spatraster(data=lizard_spat, aes(fill=2024), show.legend=TRUE) + tmpthme + scale_fill_viridis_c(option ="C", limits=c(0,10)) + geom_sf(data=lizard_shape_dhw_map)

p1998|p2002|p2012|p2016|p2017|p2020|p2022|p2024


