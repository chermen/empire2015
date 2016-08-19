setwd("~")

library(foreign)
library(maptools)
library(gpclib)
library(ggplot2)
library(RColorBrewer)

#read in the shape file and data
cau<- readShapePoly("CaucRayonData.shp")
cau2<-read.csv("cau2.csv", header=T, sep=",")


###mapping function for ggplot2

poly_coords<- function(shapefile){

if (nrow(data.frame(shapefile$ID))< 1) 

	{

	print ("No ID field in SpatialPolygon")

	}else{

	Order<-0 
	YX3<- as.numeric("XX", "XX", "XX", "XX")
	num_polys<- nrow(shapefile@data)+1
	YX3<- as.numeric("XX", "XX", "XX")
	
	curr_poly<- shapefile@data[1,]
	curr_poly_start_row <- 1
	poly_old= F
	
	for(curr_row in curr_poly_start_row:num_polys)
	{
	curr_poly_row<-shapefile@data[curr_row,]
	curr_poly_end_row = curr_row - 1	
	Poly_n= shapefile@data[curr_poly_start_row:curr_poly_end_row,]
	curr_poly_start_row = curr_row
	Poly_Name<-as.vector(Poly_n$ID)
	Poly<-shapefile[shapefile$ID==Poly_Name,]
	PolyCoords<-lapply(slot(Poly, "polygons"), function(x) lapply(slot(x,
  		 "Polygons"), function(y) slot(y, "coords")))
	PolyCoordsY<-PolyCoords[[1]][[1]][,1]
	PolyCoordsX<-PolyCoords[[1]][[1]][,2]
	Order<- 1:nrow(data.frame(PolyCoordsX)) + max(Order)
	if (poly_old != Poly_n$ID)
	{
	YX1<- data.frame(Poly_Name, Order, PolyCoordsY, PolyCoordsX)
	YX2<-rbind(YX3,YX1)
	YX3<-YX2
	}
	poly_old<-Poly_n$ID
	}
	
	join<-merge(YX3, shapefile@data, by.x="Poly_Name", by.y= "ID", all=T)
	join[order(join$Order),][1:nrow(join)-1,]
	}
}

names(cau)<- c("RAYON_CITY", "REPUB_KRAI", "CENT_LAT",   
               "CENT_LON", "EVNT_CNT", "ID", "EVNT1000",   
               "CHECH_DV", "URB_AREA", "PCT_URB", "ELEV_MEAN",  
               "SH_RUSSIAN", "PCT_FRST", "DIST_TOHWY", "LNEVNT1000")


cau_geom<- poly_coords(cau)
total <- merge(cau_geom, cau2,by="RAYON_CITY")

head(cau_geom)
head(total)



map<- qplot(PolyCoordsY, PolyCoordsX, data=total, group=RAYON_CITY, 
           fill= logfire, geom="polygon")
map+theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank())+
scale_fill_continuous(low="white", high="black", name="Intensity", breaks=c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0)
)
ggsave("indisc.pdf")
dev.off()

map2<- qplot(PolyCoordsY, PolyCoordsX, data=total, group=RAYON_CITY, 
           fill= g1_02, geom="polygon")
map2+theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank())+scale_fill_continuous(low="white", high="black", name="Indigenous\nEthnicity", breaks=c(0.1, 0.3, 0.5, 0.7, 0.9, 1.0))
ggsave("native.pdf")
dev.off()

map3<- qplot(PolyCoordsY, PolyCoordsX, data=total, group=RAYON_CITY, 
           fill=FOREST , geom="polygon")
map3+theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank())+
 scale_fill_continuous(low="white", high="black", name="Forested\nTerrain", breaks=c(0.1, 0.3, 0.5, 0.7, 0.9, 1.0))
ggsave("forested.pdf")
dev.off()


map4<- qplot(PolyCoordsY, PolyCoordsX, data=total, group=RAYON_CITY, 
           fill= loggovplus2, geom="polygon")
map4+theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank(),axis.ticks.x=element_blank(),axis.title.x=element_blank())+
#ggtitle("Distribution of Government Indiscriminate Acts")
 scale_fill_continuous(low="white", high="black", name="Intensity", breaks=c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0))
  
ggsave("gov6.eps")
dev.off()


scale_fill_gradient("mypalette")

ggplot(data=total, aes(PolyCoordsY,  PolyCoordsX, group=RAYON_CITY,
                          fill= ELF, geom="polygon")) + 
  geom_polygon(colour='black',
               fill="grey") +
  theme_bw()

map
map + scale_fill_gradient(low="white", high="black")

ggplot() + geom_map(data = cau_geom, 
                    aes(map_id = RAYON_CITY), 
                    map = cau_geom) +
  expand_limits(x = PolyCoordsX, y = PolyCoordsY) +
  coord_map("polyconic")

map + geom_map(data = cau2,
                    aes(map_id = RAYON_CITY, fill = REB), 
                    map = map) + 
  expand_limits(x = cau$CENT_LON, y = cau$CENT_LAT) +
  coord_map("polyconic")

new_fill<- function(pal, lowerlim, upperlim){
scale_fill_gradientn(colours= pal, limits=c(lowerlim, upperlim))
}

