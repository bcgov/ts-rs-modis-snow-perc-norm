# Copyright 2020 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

# Input data from https://code.earthengine.google.com/536d9807cf652870ba9264793bf5ebd9

#### Load libraries ####

library(raster)
library(RStoolbox)
library(patchwork)
library(bcmaps)
library(bcdata)
library(tidyverse)
library(sf)

#### Set Variables ####

  mydate_jd <- 97
  ref_date <- as.Date("2020-01-01") + mydate_jd-1

#### Import Raster Data (GEE Output) ####

  r5 <- stack(paste0("data/MEAN_NDSI_AQUATERRA_DIF_2020_19992019_", mydate_jd,"_5.tif"))
  r10 <- stack(paste0("data/MEAN_NDSI_AQUATERRA_DIF_2020_19992019_", mydate_jd,"_10.tif"))
  r20 <- stack(paste0("data/MEAN_NDSI_AQUATERRA_DIF_2020_19992019_", mydate_jd,"_20.tif"))
  r30 <- stack(paste0("data/MEAN_NDSI_AQUATERRA_DIF_2020_19992019_", mydate_jd,"_30.tif"))

#### Import Vector Data ####

  bc <- bc_neighbours()

  pt_snow_man <- bcdc_query_geodata('manual-snow-survey-locations') %>%
    collect()
  pt_snow_aut <- bcdc_query_geodata('automated-snow-weather-station-locations') %>%
    collect()
  pt_wx_stn <- bcdc_query_geodata('meteorological-locations') %>%
    collect()
  pol_snow_area <-bcdc_query_geodata('snow-survey-administrative-basin-areas') %>%
    collect()

#### Plotting ####

  myp <- function(r = r5, l = 1, t = "Title", min = 0, max = 100, fill_lab= "Snow Cover (%)", cap = "")
    {
    ggR(img = r, layer = l, maxpixels = 1e6, geom_raster = T) +
      scale_fill_gradientn(colours = RColorBrewer::brewer.pal(name = "RdYlBu", n = 9), na.value = NA,
                           limits = c(min,max),
                           breaks = seq(min,max,20),
                           guide = guide_colorbar(frame.colour = "black", ticks.colour = "black", barheight = 10, barwidth = 1)) +
      geom_sf(data = bc, fill = NA, color = "black") +
      geom_sf(data = pol_snow_area, fill = NA, color = "black") +   #aes(fill = BASIN_NAME)
      theme_void() +
      theme(plot.margin = margin(0,0,0,0, "cm")) +
      labs(title = t, fill = fill_lab, caption = cap)
    }

  # last 5 days
  plot_5 <-
   myp(r = r5, l = 1, t = paste0("Average of the last 5 days\nReference years 1999-2019")) /
   myp(r = r5, l = 2, t = paste0("Average of the last 5 days\nCurrent year 2020")) /
   myp(r = r5, l = 4, t = paste0("Percent of Normal\nLast 5 days, compared to 1999-2019 average"), min = 0, max = 200, fill_lab = "Percent of Normal (%)", cap = paste("Date published:",ref_date,"\nAuthor: A. Bevington (FLNRORD)"));
  ggsave(plot = plot_5, filename = "out/BC_snow_plot_5days.png", height = 12)

  # last 10 days
  plot_10 <-
    myp(r = r10, l = 1, t = paste0("Average of the last 10 days\nReference years 1999-2019")) /
    myp(r = r10, l = 2, t = paste0("Average of the last 10 days\nCurrent year 2020")) /
    myp(r = r10, l = 4, t = paste0("Percent of Normal\nLast 10 days, compared to 1999-2019 average"), min = 0, max = 200, fill_lab = "Percent of Normal (%)", cap = paste("Date published:",ref_date,"\nAuthor: A. Bevington (FLNRORD)"));
  ggsave(plot = plot_10, filename = "out/BC_snow_plot_10days.png", height = 12)

  # last 20 days
  plot_20 <-
    myp(r = r20, l = 1, t = paste0("Average of the last 20 days\nReference years 1999-2019")) /
    myp(r = r20, l = 2, t = paste0("Average of the last 20 days\nCurrent year 2020")) /
    myp(r = r20, l = 4, t = paste0("Percent of Normal\nLast 20 days, compared to 1999-2019 average"), min = 0, max = 200, fill_lab = "Percent of Normal (%)", cap = paste("Date published:",ref_date,"\nAuthor: A. Bevington (FLNRORD)"));
  ggsave(plot = plot_20, filename = "out/BC_snow_plot_20days.png", height = 12)

  # last 30 days
  plot_30 <-
    myp(r = r30, l = 1, t = paste0("Average of the last 30 days\nReference years 1999-2019")) /
    myp(r = r30, l = 2, t = paste0("Average of the last 30 days\nCurrent year 2020")) /
    myp(r = r30, l = 4, t = paste0("Percent of Normal\nLast 30 days, compared to 1999-2019 average"), min = 0, max = 200, fill_lab = "Percent of Normal (%)", cap = paste("Date published:",ref_date,"\nAuthor: A. Bevington (FLNRORD)"));
  ggsave(plot = plot_30, filename = "out/BC_snow_plot_30days.png", height = 12)
