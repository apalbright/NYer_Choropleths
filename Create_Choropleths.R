#How to make the choropleths from the New Yorker article "Which U.S. State Performs Best in the New Yorker Caption Contest?" 
#code written by Alex Albright 
#thelittledataset.com
#email: alexpalbright@gmail.com

#load packages
library(maptools);library(mapproj);library(rgeos);library(rgdal)
library(RColorBrewer);library(ggplot2)
library(choroplethr);library(choroplethrMaps)
library(maps);library(scales);library(RColorBrewer);library(grid);library(gridExtra);library(ggthemes)
library(BAMMtools)

#additional explanation as to code methods can be found here http://www.r-bloggers.com/moving-the-earth-well-alaska-hawaii-with-r/
#map from here https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html

#define my theme
my_theme <- function() {

  # Define colors for the chart
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[1]
  color.grid.major = palette[1]
  color.panel = palette[1]
  color.axis.text = palette[9]
  color.axis.title = palette[9]
  color.title = palette[9]

  # Create basic construction of chart
  theme_bw(base_size=9, base_family="Georgia") + 

  # Set the entire chart region to a light gray color
  theme(panel.background=element_rect(fill=color.panel, color=color.background)) +
  theme(plot.background=element_rect(fill=color.background, color=color.background)) +
  theme(panel.border=element_rect(color=color.background)) +

  # Format grid
  theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
  theme(panel.grid.minor=element_blank()) +
  theme(axis.ticks=element_blank()) +

  # Format legend
  theme(legend.position="right") +
  theme(legend.background = element_rect(fill=color.panel)) +
  theme(legend.text = element_text(size=10,color=color.axis.title)) + 

  # Format title and axes labels these and tick marks
  theme(plot.title=element_text(color=color.title, size=16, vjust=0.5, hjust=0, face="bold")) +
  theme(axis.text.x=element_text(size=0,color=color.axis.text)) +
  theme(axis.text.y=element_text(size=0,color=color.axis.text)) +
  theme(axis.title.x=element_text(size=10,color=color.axis.title, vjust=-1, face="italic")) +
  theme(axis.title.y=element_text(size=0,color=color.axis.title, vjust=1.8, face="italic")) +

  # Plot margins
  theme(plot.margin = unit(c(.5, .5, .5, .5), "cm"))
}

#using geojson thanks to sarah levine qgis tinkering
setwd("data")
us <- readOGR(dsn="usa_alex_2.geojson", layer="OGRGeoJSON")

us_aea <- spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
us_aea@data$id <- rownames(us_aea@data)

# extract, then rotate, shrink & move alaska (and reset projection)
# need to use state IDs via # https://www.census.gov/geo/reference/ansi_statetables.html
alaska <- us_aea[us_aea$STATEFP=="02",]
alaska <- elide(alaska, rotate=-50)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(us_aea)
 
# extract, then rotate & shift hawaii
hawaii <- us_aea[us_aea$STATEFP=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1700000))
proj4string(hawaii) <- proj4string(us_aea)
 
# remove old states and put new ones back in; note the different order
us_aea <- us_aea[!us_aea$STATEFP %in% c("02", "15", "72"),]
us_aea <- rbind(us_aea, alaska, hawaii)

#let's plot the number of winners!
data<-read.csv("winners_new.csv", header=TRUE)
fips<-read.csv("fips.csv",header=T)
merge<-merge(data,fips,by.x="winnerstate",by.y="state")
merge$id <- sprintf("%02d", as.numeric(as.character(merge$fips)))

# get ready for ggplotting it... this takes a couple of seconds
map <- fortify(us_aea, region="GEOID")

#using Jenks natural breaks classification method!
q=getJenksBreaks(merge$n, 10, subset = NULL)
merge$value.binned = cut(merge$n,breaks=q,include.lowest=TRUE)

# get ready for ggplotting it... this takes a couple seconds
map <- fortify(us_aea, region="GEOID")
 
# plot it
gg <- ggplot()
gg <- gg + geom_map(data=map, map=map,
                    aes(x=long, y=lat, map_id=id, group=group),
                    fill="#ffffff", color="#0e0e0e", size=0.15)
gg <- gg + geom_map(data=merge, map=map, aes(map_id=id, fill=value.binned),
                    color="#0e0e0e", size=0.15)
gg <- gg + scale_fill_brewer(type="div",palette="YlGnBu",name="Number of\nWinners") +              # change fill colors
  guides(fill=guide_legend(override.aes = list(colour = NULL)))
gg <- gg + my_theme()
gg <- gg + ggtitle(expression(bold(paste("The United States of the ", bolditalic("New Yorker "), bold("Caption Contest: Total Wins")))))
gg <- gg + theme(plot.title = element_text(size = 15, face = "bold", colour = "black", vjust = 0.5, hjust=0.5), legend.title=element_text(size=10),legend.key = element_rect(colour = "black"))  
gg <- gg + coord_equal() +xlab("Data via the New Yorker Caption Contest Archive & Cartoon Editor Bob Mankoff | Visualization via Alex Albright (thelittledataset.com)")
a<-gg

#let's plot number of submissions now!
data<-read.csv("subs.csv", header=TRUE)
df<-data.frame(state.abb,state.name)
merge<-merge(data,df,by.x="State",by.y="state.abb")
fips<-read.csv("fips.csv",header=T)
merge<-merge(merge,fips,by.x="state.name",by.y="state")
merge$id <- sprintf("%02d", as.numeric(as.character(merge$fips)))
merge$count1<-merge$numsubs/1000

# get ready for ggplotting it... this takes a couple seconds
map <- fortify(us_aea, region="GEOID")
 
#using Jenks natural breaks classification method!
q=getJenksBreaks(merge$count1, 10, subset = NULL)
merge$value.binned = cut(merge$count1,breaks=q,include.lowest=TRUE)
# get ready for ggplotting it... this takes a couple seconds
map <- fortify(us_aea, region="GEOID")
 
# plot it
gg <- ggplot()
gg <- gg + geom_map(data=map, map=map,
                    aes(x=long, y=lat, map_id=id, group=group),
                    fill="#ffffff", color="#0e0e0e", size=0.15)
gg <- gg + geom_map(data=merge, map=map, aes(map_id=id, fill=value.binned),
                    color="#0e0e0e", size=0.15)
gg <- gg + scale_fill_brewer(type="div",palette="YlGnBu",name="Thousands of\nSubmissions") +              # change fill colors
  guides(fill=guide_legend(override.aes = list(colour = NULL)))
gg <- gg + my_theme()
gg <- gg + ggtitle(expression(bold(paste("The United States of the ", bolditalic("New Yorker "), bold("Caption Contest: Thousands of Submissions")))))
gg <- gg + theme(plot.title = element_text(size = 15, face = "bold", colour = "black", vjust = 0.5, hjust=0.5), legend.title=element_text(size=10),legend.key = element_rect(colour = "black"))  
gg <- gg + coord_equal() +xlab("Data via the New Yorker Caption Contest Archive & Cartoon Editor Bob Mankoff | Visualization via Alex Albright (thelittledataset.com)")
b<-gg

#let's plot wins per 10k submissions!
data1<-read.csv("winners_new.csv", header=TRUE)
data1$region<-tolower(data1$winnerstate)
data2<-read.csv("subs.csv", header=TRUE)
df<-data.frame(state.abb,state.name)
data2<-merge(data2,df,by.x="State",by.y="state.abb")
data2$region<-tolower(data2$state.name)
data2$sub<-as.numeric(data2$numsubs/10000)
merge<-merge(data1,data2,by="region")
merge$winspsub<-merge$n/merge$sub
fips<-read.csv("fips.csv",header=T)
merge<-merge(merge,fips,by.x="state.name",by.y="state")
merge$id <- sprintf("%02d", as.numeric(as.character(merge$fips)))

# get ready for ggplotting it...
map <- fortify(us_aea, region="GEOID")

#jenks again!
q=unique(getJenksBreaks(merge$winspsub, 10, subset = NULL))
merge$value.binned = cut(merge$winspsub,breaks=q,include.lowest=TRUE)
# get ready for ggplotting it... this takes a cpl seconds

map <- fortify(us_aea, region="GEOID")
 
# plot it
gg <- ggplot()
gg <- gg + geom_map(data=map, map=map,
                    aes(x=long, y=lat, map_id=id, group=group),
                    fill="#ffffff", color="#0e0e0e", size=0.15)
gg <- gg + geom_map(data=merge, map=map, aes(map_id=id, fill=value.binned),
                    color="#0e0e0e", size=0.15)
gg <- gg + scale_fill_brewer(type="div",palette="YlGnBu",name="Wins per 10K\n Submissions") +              # change fill colors
  guides(fill=guide_legend(override.aes = list(colour = NULL)))
gg <- gg + my_theme()
gg <- gg + ggtitle(expression(bold(paste("The United States of the ", bolditalic("New Yorker "), bold("Caption Contest: Wins Per 10 Thousand Submissions")))))
gg <- gg + theme(plot.title = element_text(size = 15, face = "bold", colour = "black", vjust = 0.5, hjust=0.5), legend.title=element_text(size=10),legend.key = element_rect(colour = "black"))  
gg <- gg + coord_equal() +xlab("Data via the New Yorker Caption Contest Archive & Cartoon Editor Bob Mankoff | Visualization via Alex Albright (thelittledataset.com)")
c<-gg

#Done! a is the total wins choropleth, b is the thousand of submissions choropleth, and c is the wins per 10k submissions choropleth
