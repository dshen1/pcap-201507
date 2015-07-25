library(dplyr)
library(lubridate)
library(tidyr)
library(readr)
library(xts)
library(dygraphs)
library(rgdal)
library(leaflet)

# list.files("download/")
# read_csv("download/stormdata_2010.csv") %>%
#   glimpse

colsToExtract <- c("BEGIN_YEARMONTH", "BEGIN_DAY", "EVENT_TYPE", "STATE",
                   "DEATHS_DIRECT", "DEATHS_INDIRECT", "BEGIN_LAT", "BEGIN_LON",
                   "CZ_NAME", "STATE_FIPS", "CZ_FIPS")
stormData <- list.files("download/") %>%
  grep("^storm", ., value = TRUE) %>%
  lapply((function(x) read_csv(paste0("download/", x)) %>%
            select_(~ one_of(colsToExtract)) %>%
            transmute_(date = ~ paste(BEGIN_YEARMONTH, BEGIN_DAY, sep = "-") %>%
                         as.POSIXct(format = "%Y%m-%d", tz = "GMT"),
                       type = ~ EVENT_TYPE,
                       state = ~ STATE,
                       deaths = ~ DEATHS_DIRECT + DEATHS_INDIRECT,
                       lat = ~ BEGIN_LAT,
                       long = ~ BEGIN_LON,
                       county = ~ paste(CZ_NAME, STATE, sep = ", "),
                       stateFIPS = ~ STATE_FIPS,
                       countyFIPS = ~ CZ_FIPS))
         ) %>%
  bind_rows

stormTS <- stormData %>%
  filter(type %in% c("Tornado")) %>%
  group_by(date, type) %>%
  summarize(count =  n()) %>%
  spread(type, count) %>%
  (function(x) xts(x[, -1], order.by = x[[1]]))

# dygraph(stormTS) %>%
#   dyOptions(stackedGraph = TRUE)

outbreakSummary <- stormData %>%
  filter(type == "Tornado",
         date >= ymd("2011-4-25"), date <= ymd("2011-4-28")) %>%
  group_by(state) %>%
  summarize(count = n(),
            deaths = sum(deaths)) %>%
  arrange(desc(count)) %>%
  head


tornadoCountySummary <- stormData %>%
  filter(type == "Tornado") %>%
  group_by(stateFIPS, countyFIPS) %>%
  summarize(count = n())

counties <- readOGR("cb_2014_us_county_500k/cb_2014_us_county_500k.shp",
                    "cb_2014_us_county_500k", verbose = FALSE)
counties@data <- counties@data %>%
  transmute(stateFIPS = levels(STATEFP)[STATEFP] %>% as.integer,
            countyFIPS = levels(COUNTYFP)[COUNTYFP] %>% as.integer,
            name = levels(NAME)[NAME]) %>%
  left_join(tornadoCountySummary, by = c("stateFIPS", "countyFIPS"))
counties <- counties[!is.na(counties$count),]

pal <- colorNumeric(
  palette = "Reds",
  domain = counties$count
)

countyPopup <- paste0("<strong>County: </strong>",
                      counties@data$name, "<br />",
                      "<strong>Tornados in period: </strong>",
                      counties@data$count)

outbreakOccurences <- stormData %>%
  filter(type == "Tornado", deaths > 0)
deathPopup <- paste0("<strong>Deaths: </strong>", 
                      outbreakOccurences$deaths, "<br />",
                     "<strong>Date: </strong>",
                     outbreakOccurences$date, "<br />",
                     "<strong>County: </strong>",
                     outbreakOccurences$county)

# tornadoMapData %>%
#   leaflet() %>%
#   addTiles() %>%
#   addCircleMarkers(lng = ~ long, lat = ~ lat, radius = ~ deaths,
#                    fillOpacity = 0.2, color = "red", stroke = FALSE, popup = deathsPopup)
