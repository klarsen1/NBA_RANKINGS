city_lat_long <- data.frame(fread("/Users/thirdlovechangethisname/Documents/worldcitiespop.csv"))
  mutate(CityState=paste0(AccentCity," - ",Region)) %>%
  filter(CityState %in% c("Atlanta - GA",
                          "Boston - MA",
                          "Brooklyn - NY",
                          "Charlotte - NC",
                          "Chicago - IL",
                          "Cleveland - OH",
                          "Dallas - TX",
                          "Denver - CO",
                          "Detroit - MI",
                          "Oakland - CA",
                          "Houston - TX",
                          "Indianapolis - IN",
                          "Los Angeles - CA",
                          "Memphis - TN",
                          "Miami - FL",
                          "Milwaukee - WI",
                          "Minneapolis - MN",
                          "New Orleans - LA",
                          "New York - NY",
                          "Oklahoma City - OK",
                          "Orlando - FL",
                          "Philadelphia - PA",
                          "Phoenix - AZ",
                          "Portland - OR",
                          "Sacramento - CA",
                          "San Antonio - TX",
                          "Toronto - 08",
                          "Salt Lake City - UT",
                          "Washington - DC")) %>%
  select(-City) %>%
  rename(lat=Latitude, lon=Longitude, City=AccentCity) %>%
  inner_join(team_map, by="City") %>%
  arrange(City) %>%
  select(lat, lon, OWN_TEAM)

