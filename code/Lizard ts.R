

### the approach:

  library(rerddap)
  library(lubridate)

  #Sys.setenv(RERDDAP_DEFAULT_URL = "https://upwell.pfeg.noaa.gov/erddap/")
  #eurl()
  # now-60hours"


  lizard_ts_DHW <- rerddap::griddap(
    dataset="NOAA_DHW",
    time = c('1995-01-01','2024-04-01'),
    latitude = c(-14.70627, -14.64135),
    longitude = c(145.43233, 145.47928),
    fields = c("CRW_DHW","CRW_SST","CRW_SSTANOMALY"),
    stride = 1,
    fmt = "csv")

  lizard_ts_DHW_2 <- rerddap::griddap(
    dataset="NOAA_DHW",
    time = c('1986-01-01','1995-04-01'),
    latitude = c(-14.70627, -14.64135),
    longitude = c(145.43233, 145.47928),
    fields = c("CRW_DHW","CRW_SST","CRW_SSTANOMALY"),
    stride = 1,
    fmt = "csv")

  lizard_DHW <- rbind(lizard_ts_DHW_2, lizard_ts_DHW)


  lizard_DHW |>
    mutate(time=ymd_hms(time)) |>
    mutate(year= year(time)) |>
    mutate(month=month(time)) |>
    group_by(time) |>
    summarise(crw_dhw=mean(crw_dhw)) |>
  ggplot() + theme_bw() +
    geom_line(aes(time, crw_dhw)) +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


  lizard_DHW |>
    mutate(time=ymd_hms(time)) |>
    mutate(year= year(time)) |>
    mutate(month=month(time)) |>
    group_by(time) |>
    summarise(crw_dhw=mean(crw_dhw)) |>
    ggplot() + theme_bw() +
    geom_line(aes(time, crw_dhw)) +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


  gbr_dhw <- tidyterra::as_spatraster(csvdata, crs="EPSG:4326") |> project("EPSG:20353")
