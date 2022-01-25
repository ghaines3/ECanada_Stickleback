ECcoords<-read.csv("EastCoast_stickleback-data-reduced.csv")

library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(ggplot2)
library(cowplot)
library(devtools)
library(patchwork)
library(dplyr)
#map
devtools::install_github("ropensci/rnaturalearthhires")
library(rnaturalearthhires)
##### loads map data
ne_download(scale=10,category='physical',type="rivers_lake_centerlines",returnclass ="sf")
ne_download(scale=10,category='cultural',type="admin_1_states_provinces",
            returnclass ="sf")# gives error that can be ignored
ne_download(scale = 10, type = 'lakes', category = 'physical')
water<-ne_load(scale=10,category='physical',type="rivers_lake_centerlines",returnclass ="sf")
lakes<-ne_load(scale=10,category='physical',type="lakes",returnclass ="sf")
provinces<-ne_load(scale=10,category='cultural',type="admin_1_states_provinces",
                   returnclass ="sf")# gives error that can be ignored

usa <- subset(provinces, admin == "United States of America")
can <- subset(provinces, admin == "Canada")

usacan<-rbind(usa,can)

#eastern canada map
ECanada<-ggplot(data = provinces) +
  geom_sf(color = "grey45", fill="gray90", size=.3) +
  geom_sf(data= water, color="lightskyblue", size=.25)+
  geom_sf(data= lakes, color="lightskyblue", fill="lightskyblue1", size=.25)+
  theme_cowplot(font_size = 9)+panel_border(color = "grey50")+
  theme(axis.line = element_blank())+coord_sf(xlim = c(-101, -53), 
           ylim = c(43, 68), expand = FALSE)+
  theme(plot.margin =margin(0.01,0.01,0.01,0.01, "pt"), legend.position = "none")+
  geom_rect(xmin = -69.5, xmax = -62, ymin = 44.5, ymax = 49, 
            fill = NA, colour = "seagreen3", size = .3)+
  geom_rect(xmin = -65.5, xmax = -57, ymin = 43.5, ymax = 50, 
            fill = NA, colour = "firebrick2", size = .3)+
  geom_point(data=filter(ECcoords, other!="W")%>% filter(long<(-69.5) | long>(-62) | lat>49)%>%
               filter(Morph!="F(2)"&Morph!="UC"), aes(x=long, y=lat,shape=Morph), size=2, alpha=0.8)+
  geom_point(data=filter(ECcoords, other!="W")%>% filter(long<(-69.5) | long>(-62) | lat>49)%>%
               filter(Morph=="F(2)"|Morph=="UC"), aes(x=long, y=lat,color=Morph), size=2, alpha=0.8)+
  scale_color_manual(values = c("seagreen2","firebrick1"))+
  geom_text_repel(data=filter(ECcoords, other!="W")%>% filter(long<(-69.5) | long>(-62) | lat>49),aes(x=long, y=lat,label=citation_num),
                  box.padding=.3,point.padding = .08,size = 3, 
                  segment.size = 0.35,fontface = "bold")+
  scale_shape_manual(values=c(1,17,25,8))+labs(x=NULL, y=NULL)

#Gaspé, NB, PEI, NS map
QCmaritime<-ggplot(data = usacan, fill="palegreen") +
  geom_sf(color = "grey45", fill="gray90", size=.3) +
  geom_sf(data= water, color="lightskyblue", size=.25)+
  geom_sf(data= lakes, color="lightskyblue", fill="lightskyblue1", size=.25)+
  theme_cowplot(font_size = 9)+panel_border(color = "seagreen3")+
  theme(axis.line = element_blank())+coord_sf(xlim = c(-69.5, -62), 
           ylim = c(44.5, 49), expand = FALSE)+
  theme(plot.margin =margin(0.15,0.15,0.15,0.15, "in"), legend.position = "none")+
  geom_point(data=filter(ECcoords, other!="W" & long>(-69.5) & long<(-62) & lat<49), 
             aes(x=long, y=lat,shape=Morph), size=2, alpha=0.8)+
  scale_shape_manual(values=c(1,25,15))+  labs(x=NULL, y=NULL)+
  geom_text_repel(data=filter(ECcoords, other!="W" & long>(-69.5) & 
                  long<(-62)& lat<49 & citation_num!="1"),
                  aes(x=long, y=lat,label=citation_num),
                box.padding=.15,point.padding = .15,size = 3, 
                segment.size = 0.1,fontface = "bold")

#white stickleback map
white.stickles<-ggplot(data = usacan, fill="palegreen") +
  geom_sf(color = "grey45", fill="gray90", size=.3) +
  geom_sf(data= water, color="lightskyblue", size=.25)+
  geom_sf(data= lakes, color="lightskyblue", fill="lightskyblue1", size=.25)+
  theme_cowplot(font_size = 9)+panel_border(color = "firebrick2")+
  theme(axis.line = element_blank())+coord_sf(xlim = c(-65.5, -57), 
           ylim = c(43.5, 50), expand = FALSE)+
  theme(plot.margin =margin(0.15,0.15,0.15,0.15, "in"), legend.position = "none")+
  geom_point(data=filter(ECcoords, other=="W"), aes(x=long, y=lat), size=1)+
  scale_shape_manual(values=c(1))+labs(x=NULL, y=NULL)+
  geom_text_repel(data=filter(ECcoords, other=="W" & citation_num!="6"),aes(x=long, y=lat,label=citation_num),
                  box.padding=.7,point.padding = .35,size = 3, 
                  segment.size = 0.35,fontface = "bold")
#compound plot of maps
compound<-(ECanada + {QCmaritime /
    white.stickles} + plot_layout(width = c(2.6, 1)))
compound
ggsave("ECstickles-1.jpg", width = 12, height = 8, dpi = 300)

 
## Plate freq graph
ECfreq<-read.csv("EastCoast_stickleback-data.csv")

plate.freq<-filter(melt(ECfreq[,c("site","Province","plate_low",
               "plate_partial","plate_complete","habitat")],
     id=c("site","Province","habitat"),value.name = "freq", variable.name = "plate_morph"), is.na(freq)==F)
plate.freq$Province<-as.factor(plate.freq$Province)
plate.freq$habitat<-as.factor(plate.freq$habitat)
plate.freq<-with(plate.freq, plate.freq[order(Province),])
plate.freq$Province<-factor(plate.freq$Province,levels = unique(plate.freq$Province))
plate.freq$site<-factor(plate.freq$site, levels = unique(plate.freq[order(plate.freq$Province), "site"]))

freq.plot<-ggplot(data = plate.freq, aes(x=site,weight=freq, fill=plate_morph, color = habitat))+
  geom_bar(position="fill",size=.25)+coord_flip()+theme_minimal()+
  scale_color_manual(values = c("firebrick4","skyblue1","skyblue1"), guide = NULL)+
  theme(axis.text = element_text(size = 6),legend.position = "bottom")+
  scale_fill_manual(values = c("grey80","gray50","black"), name="Plate Morph")+
  labs(y="Frequency", x=NULL)

ggsave("freq_plot.jpg", width = 6, height = 10, dpi = 300)




