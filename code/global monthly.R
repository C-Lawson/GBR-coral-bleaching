


# https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW_Lon0360.html
# https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW_Lon0360.csv?CRW_DHW%5B(1985-04-01T12:00:00Z):1:(2024-04-04T12:00:00Z)%5D%5B(23.27):1:(-23.27)%5D%5B(0.025):1:(359.975)%5D,CRW_HOTSPOT%5B(1985-04-01T12:00:00Z):1:(2024-04-04T12:00:00Z)%5D%5B(23.27):1:(-23.27)%5D%5B(0.025):1:(359.975)%5D,CRW_SST%5B(1985-04-01T12:00:00Z):1:(2024-04-04T12:00:00Z)%5D%5B(23.27):1:(-23.27)%5D%5B(0.025):1:(359.975)%5D,CRW_SSTANOMALY%5B(1985-04-01T12:00:00Z):1:(2024-04-04T12:00:00Z)%5D%5B(23.27):1:(-23.27)%5D%5B(0.025):1:(359.975)%5D


library(rerddap)

### download data
# global_NOAA_DHW_Lon0360 <- rerddap::griddap(
#     dataset="NOAA_DHW_Lon0360",
#     time = c('1985-04-01T12:00:00Z','2024-04-04T12:00:00Z'),
#     latitude = c(-23.4394, 23.4394),
#     longitude = c(0.025, 359.975),
#     fields = c("CRW_DHW","CRW_SST","CRW_SSTANOMALY"),
#     stride = 1,
#     fmt = "csv")
#
#
# saveRDS(global_NOAA_DHW_Lon0360, "data/global_NOAA_DHW_Lon0360.rds")



### download data
# 661,799,780 rows, switch to data.table

global_NOAA_DHW_Lon0360_monthly <- readRDS("data/global_NOAA_DHW_Lon0360.rds") |>
  as_data_frame() |>
  na.omit() |>
  filter(latitude==23.4) |>
  filter(longitude==35.6)

global_NOAA_DHW_Lon0360_monthly |>
  mutate(time=ymd_hms(time)) |>
  mutate(year=year(time)) |>
  mutate(month=month(time)) |>
  ggplot() + theme_bw() +
  geom_point(data=global_NOAA_DHW_Lon0360_monthly_df, aes(month, max_sst, color=year))


  # group_by(year, month) |>
  # summarise(max_sst=max(crw_sst), max_sstanomaly=max(crw_sstanomaly), max_dhw=max(crw_dhw))
