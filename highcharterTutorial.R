#tutorial on highcharter
library(highcharter)

"""
hchart is same as qplot from ggplot

hc_add_series works like ggplot2's geom_S

hcaes works like ggplots2's aes



"""
glimpse(pokemon)

df.test<-df.exports%>%
  group_by(country)%>%
  count(value)%>%
  arrange(desc(n) )%>%
  hchart(type = "bar", hcaes(x = country,y = n))




############################################################################
############################################################################
###                                                                      ###
###                             REMOVE LATER                             ###
###                                                                      ###
############################################################################
############################################################################
pokemon%>%
  count(type_1)%>%
  arrange(n)%>%
  hchart(type = "column", hcaes(x = type_1, y = n))



#################################################################
##                          treemappp                          ##
#################################################################
df.test%>%
  hchart(type = "treemap", hcaes( x = country, value = value, color = value))



highchart()%>%
  hc_add_series(pokemon, "scatter", hcaes(x = height, y = weight))



#################################################################
  ##                  Diamond package                ##
#################################################################
library(ggplot2)
data(diamonds, package = "ggplot2")

set.seed(123)
data <- sample_n(diamonds, 300)

modlss <- loess(price ~ carat, data = data)
fit <- arrange(augment(modlss), carat)

highchart() %>%
  hc_add_series(data, type = "scatter",
                hcaes(x = carat, y = price, size = depth, group = cut)) %>%
  hc_add_series(fit, type = "line", hcaes(x = carat, y = .fitted),
                name = "Fit", id = "fit") %>%
  hc_add_series(fit, type = "arearange",
                hcaes(x = carat, low = .fitted - 2*.se.fit,
                      high = .fitted + 2*.se.fit),
                linkedTo = "fit")


##################################################################
##                 Historic and est. World Pop,                 ##
##################################################################


highchart() %>%
  hc_chart(type = "area") %>%
  hc_title(text = "Historic and Estimated Worldwide Population Distribution by Region") %>%
  hc_subtitle(text = "Source: Wikipedia.org") %>%
  hc_xAxis(categories = c("1750", "1800", "1850", "1900", "1950", "1999", "2050"),
           tickmarkPlacement = "on",
           title = list(enabled = FALSE)) %>%
  hc_yAxis(title = list(text = "Percent")) %>%
  hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
             <b>{point.percentage:.1f}%</b> ({point.y:,.0f} millions)<br/>",
             shared = TRUE) %>%
  hc_plotOptions(area = list(
    stacking = "percent",
    lineColor = "#ffffff",
    lineWidth = 1,
    marker = list(
      lineWidth = 1,
      lineColor = "#ffffff"
    ))
  ) %>%
  hc_add_series(name = "Asia", data = c(502, 635, 809, 947, 1402, 3634, 5268)) %>%
  hc_add_series(name = "Africa", data = c(106, 107, 111, 133, 221, 767, 1766)) %>%
  hc_add_series(name = "Europe", data = c(163, 203, 276, 408, 547, 729, 628)) %>%
  hc_add_series(name = "America", data = c(18, 31, 54, 156, 339, 818, 1201)) %>%
  hc_add_series(name = "Oceania", data = c(2, 2, 2, 6, 13, 30, 46))




#################################################################
##                      Stock market data                      ##
#################################################################
library(quantmod)
x <- getSymbols("ANZ.NZ", auto.assign = F)


y <- getSymbols("ATM.NZ", auto.assign =F  )
highchart(type = "stock")%>%
  hc_add_series(x)%>%
  hc_add_series(y, type = "ohlc")



#################################################################
##            Country wise mapping through highmaps   
##            hcmap()
##
#################################################################
hcmap("https://code.highcharts.com/mapdata/countries/in/in-all.js")%>%
  hc_title(text = "India")

#Well thats a plain map, lets convert that into choropleths
#keys to join data in highcharts maps


mapdata <- get_data_from_map(download_map_data("https://code.highcharts.com/mapdata/countries/in/in-all.js"))

glimpse(mapdata)


pop = as.data.frame(c(84673556, 1382611, 31169272, 103804637, 1055450, 25540196, 342853, 242911, 18980000, 1457723, 60383628, 25353081, 6864602,
                      12548926, 32966238, 61130704, 33387677, 64429, 72597565, 112372972, 2721756, 2964007, 1091014, 1980602, 41947358, 1244464,
                      27704236, 68621012, 607688, 72138958, 3671032, 207281477, 10116752,91347736))


##################################################################
##                  Indian Population by State                  ##
##################################################################


state= mapdata%>%
  select(`hc-a2`)%>%
  arrange(`hc-a2`)

State_pop = as.data.frame(c(state, pop))

names(State_pop)= c("State", "Population")


hcmap("https://code.highcharts.com/mapdata/countries/in/in-all.js", data = State_pop, value = "Population",
      joinBy = c("hc-a2", "State"), name = "Fake data",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#FAFAFA", borderWidth = 0.1,
      tooltip = list(valueDecimals = 0))




##################################################################
##              Checking the same on world mappppp              ##
##################################################################


mapdata <- get_data_from_map(download_map_data("https://code.highcharts.com/mapdata/custom/world.js"))


country= mapdata%>%
  select(`name`)%>%
  arrange(`name`)

df.test <-df.exports%>%
  group_by(country)%>%
  summarise(
    value = sum(value) )



hcmap("https://code.highcharts.com/mapdata/custom/world.js", data = df.test, value = "value",
      joinBy = c("hc-a2", "country"), name = "Fake data",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#FAFAFA", borderWidth = 0.1,
      tooltip = list(valueDecimals = 0))





