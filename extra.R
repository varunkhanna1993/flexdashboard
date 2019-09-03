#rough work
#Loading in data
df <- read.csv("trade_data.csv")

#exports of goods only
df.exports <- df[df$type == "exports" & df$product_type =="good",]


# unique(df.exports$product_description)


###########################################################################
###########################################################################
###                                                                     ###
###                 TREEMAP: ROUGH WORK                               ###
###                                                                     ###
###########################################################################
###########################################################################

library(highcharter)
library(dplyr)
library(viridisLite)
library(forecast)
library(treemap)
library(flexdashboard)

thm <- 
  hc_theme(
    colors = c("#1a6ecc", "#434348", "#90ed7d"),
    chart = list(
      backgroundColor = "transparent",
      style = list(fontFamily = "Source Sans Pro")
    ),
    xAxis = list(
      gridLineWidth = 1
    )
  )


tree.map <- treemap(df.exports, index ="country",
                          vSize = "value", vColor = "value",
                          type = "value", palette = rev(viridis(6)))

highchart() %>% 
  hc_add_series_treemap(tree.map, allowDrillToNode = TRUE,
                        layoutAlgorithm = "squarified") %>% 
  hc_add_theme(thm)


leaflet() %>%
  setView(0, 0, zoom = 1) %>%
  addTiles()

# NOT RUN {
library("dplyr")

data("USArrests", package = "datasets")
data("usgeojson")

USArrests <- mutate(USArrests, state = rownames(USArrests))

highchart() %>% 
  hc_title(text = "Violent Crime Rates by US State") %>% 
  hc_subtitle(text = "Source: USArrests data") %>% 
  hc_add_series_map(usgeojson, USArrests, name = "Murder arrests (per 100,000)",
                    value = "Murder", joinBy = c("woename", "state"),
                    dataLabels = list(enabled = TRUE,
                                      format = '{point.properties.postalcode}')) %>% 
  hc_colorAxis(stops = color_stops()) %>% 
  hc_legend(valueDecimals = 0, valueSuffix = "%") %>%
  hc_mapNavigation(enabled = TRUE) 

# }
# NOT RUN {
data(worldgeojson, package = "highcharter")


highchart(type = "map") %>% 
  hc_add_series_map(map = worldgeojson, df =  df.test, value = "value", joinBy = c("name","country")) %>% 
  hc_colorAxis(stops = color_stops()) %>% 
  hc_tooltip(useHTML = TRUE, headerFormat = "",
             pointFormat = "This is {point.name} and NZ exports ${point.value} worth of goods")




###########################################################################
###########################################################################
###                                                                     ###
###                SELECTING VALUE OF EXPORTS BY COUNTRY                ###
###                                                                     ###
###########################################################################
###########################################################################


df.exports%>%
  group_by(year)%>%
  summarise(value = sum(value))%>%
  arrange(desc(value))%>%
  top_n(20)%>%
  hchart(type = "column", hcaes(x =year,y = value))%>%
  hc_yAxis(title = list(text = "Value in Billions"))



######################
#### IMPORTANT grouping #####
####################
df.test <-df.exports%>%
  group_by(country)%>%
  summarise(
    value = sum(value))
levels(df.test$country)[levels(df.test$country)=="China, People's Republic of"] <- "China"

options(WordpressLogin = c(admin = 'jj'),
        WordpressURL = 'https://varu')