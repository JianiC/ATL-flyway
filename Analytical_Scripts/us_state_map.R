## Atlantic flyway map 
#install.packages("maps")
library(maps)
library(ggplot2)

us.map <-  map_data('state')

# add flyway zones
us.map$flyway[us.map$region %in% 
                c("connecticut","delaware",  "florida", "georgia", "maine","maryland","massachusetts", "new hampshire", 
                  "new jersey", "new york", "north carolina", "pennsylvania",  "rhode island", "south carolina" ,"vermont", 
                  "west virginia", "virginia")] <- "ATL - flyway"
us.map$flyway[us.map$region %in% 
                c("south dakota", "north dakota","nebraska", "kansas", "oklahoma", 
                  "minnesota", "iowa", "missouri", "wisconsin", "illinois", "indiana",
                  "michigan", "ohio", "kentucky", "tennessee","district of columbia","new mexico", 
                  "texas", "arkansas", "louisiana", "alabama", "mississippi","montana", "idaho", "wyoming",
                  "utah", "colorado","washington", "oregon", "nevada", "arizona", "california")] <- "Outside ATL flyway"

# subset the dataframe by flyway zones and move lat/lon accordingly
us.map$lat.transp[us.map$flyway == "ATL - flyway"] <- us.map$lat[us.map$flyway == "ATL - flyway"]
us.map$long.transp[us.map$flyway == "ATL - flyway"] <- us.map$long[us.map$flyway == "ATL - flyway"] + 5

us.map$lat.transp[us.map$flyway == "Outside ATL flyway"] <- us.map$lat[us.map$flyway == "Outside ATL flyway"]
us.map$long.transp[us.map$flyway == "Outside ATL flyway"] <- us.map$long[us.map$flyway == "Outside ATL flyway"]


# add labels
states <- aggregate(cbind(long.transp, lat.transp) ~ region, data=us.map, 
                    FUN=function(x)mean(range(x)))
states$labels <- c("AL", "AR", "AZ", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "IA", 
                   "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", 
                   "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", 
                   "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", 
                   "VT", "WA", "WI", "WV", "WY")

# plot and use padd zone as fill
ggplot(us.map,  aes(x=long.transp, y=lat.transp), colour="white") + 
  geom_polygon(aes(group = group, fill=flyway)) +
  #geom_text(data=states, aes(long.transp, lat.transp, label=labels), size=3) +
  theme(panel.background = element_blank(),  # remove background
        panel.grid = element_blank(), 
        axis.line = element_blank(), 
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  coord_equal()