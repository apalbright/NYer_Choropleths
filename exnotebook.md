Baby's First R Markdown Notebook: *New Yorker* Caption Contest Choropleths Example
================

I recently updated RStudio *(yeahhhh 1.0.143!)* and have been hearing lots about notebook workflow for a while now, so here we are: welcome to my first attempt at making a [R Markdown](http://rmarkdown.rstudio.com) Notebook!

I decided that I would try out the workflow by creating a notebook for a previous project of mine. I landed on the [*New Yorker* caption contest choropleths project](http://www.newyorker.com/culture/culture-desk/which-u-s-state-performs-best-in-the-new-yorker-caption-contest) since it was one of the most difficult to code for me. I spent lots of time tripping my way through using geodata with R and running (clicking) around in circles on StackOverflow. Moreover, Alaska and Hawaii became the bane of my existence for a few weeks in generating these images. ([This post](http://www.r-bloggers.com/moving-the-earth-well-alaska-hawaii-with-r/) ended up being super useful in the end for dealing with those states. Nathan Yau made [a tutorial](http://flowingdata.com/2015/09/01/how-to-make-maps-in-r-that-include-alaska-and-hawaii/) on this also if you're curious and a FlowingData member.)

### tl;dr Hopefully this notebook can be a useful resource for people who want to make similar US choropleth visuals in R.

First things first: load necessary libraries for the code!
==========================================================

``` r
library(maptools);library(mapproj);library(rgeos);library(rgdal);library(RColorBrewer);library(ggplot2);library(choroplethr);library(choroplethrMaps);library(maps);library(scales);library(grid);library(gridExtra);library(ggthemes);library(BAMMtools)
```

Then, I define the theme that I like to use for my plots.
---------------------------------------------------------

I will call this in a later chunk.

``` r
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
  theme_bw(base_size=7, base_family="Georgia") + 

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
  theme(legend.text = element_text(size=12,color=color.axis.title)) + 

  # Format title and axes labels these and tick marks
  theme(plot.title=element_text(color=color.title, size=18, vjust=0.5, hjust=0, face="bold")) +
  theme(axis.text.x=element_text(size=0,color=color.axis.text)) +
  theme(axis.text.y=element_text(size=0,color=color.axis.text)) +
  theme(axis.title.x=element_text(size=12,color=color.axis.title, vjust=-1, face="italic")) +
  theme(axis.title.y=element_text(size=0,color=color.axis.title, vjust=1.8, face="italic")) +

  # Plot margins
  theme(plot.margin = unit(c(.5, .5, .5, .5), "cm"))
}
```

Now, we get into getting all the geo data in shape to make something with it!
-----------------------------------------------------------------------------

I call the geojson in my data folder for grabbing the important layer to manipulate. State IDs of 2 and 15 for Alaska and Hawaii are from [Census source](https://www.census.gov/geo/reference/ansi_statetables.html).

``` r
setwd("data")
us <- readOGR(dsn="usa_alex_2.geojson", layer="OGRGeoJSON")
```

    ## OGR data source with driver: GeoJSON 
    ## Source: "usa_alex_2.geojson", layer: "OGRGeoJSON"
    ## with 51 features
    ## It has 9 fields

``` r
us_aea <- spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
us_aea@data$id <- rownames(us_aea@data)

# extract, then rotate, shrink & move alaska (and reset projection)
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
```

### Now that we have the data in shape. We can now get to generating each of the three images in the article.

First off, I see how absolute wins are distributed across the country:
======================================================================

``` r
#let's plot the number of winners!
data<-read.csv("data/winners_new.csv", header=TRUE)
fips<-read.csv("data/fips.csv",header=T)
merge<-merge(data,fips,by.x="winnerstate",by.y="state")
merge$id <- sprintf("%02d", as.numeric(as.character(merge$fips)))

# get ready for ggplotting it... this takes a couple of seconds
map <- fortify(us_aea, region="GEOID")

#using Jenks natural breaks classification method!
q=getJenksBreaks(merge$n, 10, subset = NULL)
merge$value.binned = cut(merge$n,breaks=q,include.lowest=TRUE)

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
gg <- gg + theme(plot.title = element_text(size = 18, face = "bold", colour = "black", vjust = 0.5, hjust=0.5), legend.title=element_text(size=12),legend.key = element_rect(colour = "black"))  
gg <- gg + coord_equal() +xlab("Data via the New Yorker Caption Contest Archive & Cartoon Editor Bob Mankoff | Visualization via Alex Albright (thelittledataset.com)")
gg
```

![](exnotebook_files/figure-markdown_github/plot%20total%20wins-1.png)

However, it could be that states who win the most are simply submitting the most. So, I illustrate how submission numbers look across the country:
==================================================================================================================================================

``` r
#let's plot submissions now!
data<-read.csv("data/subs.csv", header=TRUE)
df<-data.frame(state.abb,state.name)
merge<-merge(data,df,by.x="State",by.y="state.abb")
fips<-read.csv("data/fips.csv",header=T)
merge<-merge(merge,fips,by.x="state.name",by.y="state")
merge$id <- sprintf("%02d", as.numeric(as.character(merge$fips)))
merge$count1<-merge$numsubs/1000

# get ready for ggplotting it
map <- fortify(us_aea, region="GEOID")

#using Jenks natural breaks classification method!
q=getJenksBreaks(merge$count1, 10, subset = NULL)
merge$value.binned = cut(merge$count1,breaks=q,include.lowest=TRUE)

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
gg <- gg + theme(plot.title = element_text(size = 18, face = "bold", colour = "black", vjust = 0.5, hjust=0.5), legend.title=element_text(size=12),legend.key = element_rect(colour = "black"))  
gg <- gg + coord_equal() +xlab("Data via the New Yorker Caption Contest Archive & Cartoon Editor Bob Mankoff | Visualization via Alex Albright (thelittledataset.com)")
gg
```

![](exnotebook_files/figure-markdown_github/plot%20submissions-1.png)

Based on the wins and submissions numbers, one metric of success could be wins per 10K submission *(think of this as a state batting average for caption contest success)*:
===========================================================================================================================================================================

``` r
#let's plot wins per 10k submissions!
data1<-read.csv("data/winners_new.csv", header=TRUE)
data1$region<-tolower(data1$winnerstate)
data2<-read.csv("data/subs.csv", header=TRUE)
df<-data.frame(state.abb,state.name)
data2<-merge(data2,df,by.x="State",by.y="state.abb")
data2$region<-tolower(data2$state.name)
data2$sub<-as.numeric(data2$numsubs/10000)
merge<-merge(data1,data2,by="region")
merge$winspsub<-merge$n/merge$sub
fips<-read.csv("data/fips.csv",header=T)
merge<-merge(merge,fips,by.x="state.name",by.y="state")
merge$id <- sprintf("%02d", as.numeric(as.character(merge$fips)))

# get ready for ggplotting it...
map <- fortify(us_aea, region="GEOID")

#jenks again!
q=unique(getJenksBreaks(merge$winspsub, 10, subset = NULL))
merge$value.binned = cut(merge$winspsub,breaks=q,include.lowest=TRUE)

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
gg <- gg + theme(plot.title = element_text(size = 18, face = "bold", colour = "black", vjust = 0.5, hjust=0.5), legend.title=element_text(size=12),legend.key = element_rect(colour = "black"))  
gg <- gg + coord_equal() +xlab("Data via the New Yorker Caption Contest Archive & Cartoon Editor Bob Mankoff | Visualization via Alex Albright (thelittledataset.com)")
gg
```

![](exnotebook_files/figure-markdown_github/plot%20wins%20per%2010k%20submissions-1.png)

The above leads to the following conclusion from the article: *This map shows that Alaska is actually the most successful state in terms of wins per ten thousand submissions. Though Alaska has only won the contest twice, the fact that it had a mere twenty-one hundred and two documented contest entries renders its rate of approximately 9.5 wins per ten thousand submissions the highest in the country. Mississippi, another state not commonly associated with the New Yorker crowd, comes in at a close second, with 8.14 wins per ten thousand submissions. Beyond Alaska and Mississippi, the other states in the top ten with respect to this metric are Hawaii, Utah, Oklahoma, Illinois, Virginia, Kentucky, Vermont, and Connecticut. Only three of these ten were in the Top Ten in terms of the total win count—namely, Illinois, Virginia, and Connecticut.*

*Therefore, after all that discussions about the mores of the cultural élite and familiarity with New Yorker-esque sensibilities, it’s not the states saturated with Prius-driving soccer fans that are the most successful caption-contest competitors. Instead, the most successful states are scattered across the country, no longer confined to the coasts, and the ultimate winner is able to see Russia from its house.*

### So, that is how to generate those three graphs.

This has been an R Markdown Notebook!
-------------------------------------

The End.
========
