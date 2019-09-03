
##################################################################
##                       Balance of trade                       ##
##################################################################



t1<- df.exports%>%
  group_by(country)%>%
  summarise(value = sum(value))%>%
  arrange(desc(value))

t2<- df.imports%>%
  group_by(country)%>%
  summarise(value = sum(value))%>%
  arrange(desc(value))

t2<- t2[-231,]


t3<- merge(t2,t1, by.x = "country", by.y = "country")
names(t3) <- c("Country", "Imports", "Exports")

t3$BOT <- t3$Exports - t3$Imports

t3$BOT_f <- ifelse(t3$Deficit >= 0, "trade-surplus", "trade-deficit")

###TREEMAP
tree.map <- treemap(t3, index ="BOT",
                    vSize = "BOT", vColor = "BOT",
                    type = "value", palette = rev(viridis(6)))

highchart() %>% 
  hc_add_series_treemap(tree.map, allowDrillToNode = TRUE,
                        layoutAlgorithm = "squarified") %>% 
  hc_add_theme(thm)






