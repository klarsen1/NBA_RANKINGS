require(rgdal)
require(gpclib) 
require(maptools) # make sure rgeos is not available when loading maptools
require(jsonlite)
require(magrittr)
require(dplyr)
require(ggplot2)
require(Cairo)
require(scales)

# use gpclib because of bugs in rgeos
gpclibPermit()

# read in map polygons from file
map = readOGR("nielsentopo.json","nielsen_dma")
map_df <- fortify(map, region = "id") %>% 
  tbl_df() %>%
  mutate(dma=as.integer(id))

# read in toposjon as plain json file for data
json_full <- fromJSON("nielsentopo.json")
properties <- json_full$objects$nielsen_dma$geometries$properties %>%
  tbl_df() %>%
  select(dma, dma_name=dma1) %>%
  mutate(dma_name=gsub("\\s*\\([^\\)]+\\)", "",dma_name , perl=T)) %>%
  mutate(dma_name=gsub("Ft.", "Fort", dma_name, perl=T)) %>%
  arrange(dma_name) %>%
  rename(DMA=dma_name)

# read stats data
stats <- readRDS("stats.rda") %>%
  filter(!grepl("AK",DMA)) %>%
  filter(!grepl("HI",DMA)) %>%
  mutate(DMA=ifelse(DMA=="Florence-Myrtle Beach, SC","Myrtle Beach-Florence, SC",DMA)) %>%
  mutate(DMA=ifelse(DMA=="Columbus, MS","Columbus-Tupelo-West Point, MS",DMA)) %>%
  mutate(DMA=ifelse(DMA=="Springfield, MA","Springfield-Holyoke, MA",DMA)) %>%
  arrange(DMA)

# join to map and properties
stats$DMA <- properties$DMA

dma_data <- properties %>% 
  left_join(stats, by="DMA")

# join map, map properties and Kim's signup numbers
plot_df <- map_df %>%
  left_join(dma_data, by="dma")

# plot map
p1 <- ggplot()
p1 <- p1 + geom_map(data=plot_df, map=map_df,
                    aes(map_id=id, x=long, y=lat, group=group, fill=stats_reservations),
                    color="white", size=0.25)
p1 <- p1 + coord_map()
p1 <- p1 + labs(x="", y="")
p1 <- p1 + theme_bw()
p1 <- p1 + theme(panel.grid=element_blank())
p1 <- p1 + theme(panel.border=element_blank())
p1 <- p1 + theme(axis.ticks=element_blank())
p1 <- p1 + theme(axis.text=element_blank())
p1 <- p1 + scale_fill_gradient(low='gray88', high='royalblue4', labels=percent)
p1 <- p1 + theme(legend.title=element_blank())
