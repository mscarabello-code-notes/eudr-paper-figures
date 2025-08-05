#-------------------------------------------------------------------------------

#Limpar o ambiente
rm(list = ls())
library(ggplot2)
library(reshape2)

# Armazena o caminho da pasta do Projeto
path <- getwd()
pathdir <- paste(path, "data/", sep = "/")
if (!dir.exists(pathdir)) {dir.create(pathdir)}

#--------------------------  AREA  -----------------------------------------
#---------------------------------------------------------------------------
#Crp+Soy     	2000	2005	2010	2015	2020	2025	2030	2035	2040	2045	2050
#BASE,Brazil,CrpSoy,39.5631,46.275,56.9626,61.1114,66.727,68.3693,73.7589,76.2728,78.0063,82.8631,83.4487
#FC,Brazil,CrpSoy,39.5631,46.275,56.9626,61.1114,66.727,68.2601,73.271,75.2284,76.8047,81.3452,81.7482
#ZD_F,Brazil,CrpSoy,39.5631,46.275,56.9626,61.1114,66.727,68.4013,73.7667,76.2715,77.8683,82.7154,83.3665
#ZD_FOWL,Brazil,CrpSoy,39.5631,46.275,56.9626,61.1114,66.727,68.207,73.4958,76.0233,77.8591,82.7004,83.2641
#ZD_ALL,CrpSoy,39.5631,46.275,56.9626,61.1114,66.727,68.1104,73.2579,75.3097,76.9942,81.39,81.6261

mydf = data.frame(
  ZD_ALL	     = c(66.727,68.1104,73.2579,75.3097,76.9942,81.39,81.6261),
  ZD_FOWL           = c(66.727,68.207,73.4958,76.0233,77.8591,82.7004,83.2641),
  ZD_F	     = c(66.727,68.4013,73.7667,76.2715,77.8683,82.7154,83.3665),
  FC      = c(66.727,68.2601,73.271,75.2284,76.8047,81.3452,81.7482),
  BASE	       = c(66.727,68.3693,73.7589,76.2728,78.0063,82.8631,83.4487),
  Year         = c(2020,	2025,	2030,	2035,	2040,	2045,	2050))


cPalette <- c("darkgreen","forestgreen","green","#9DC209","Blue")
cMarker <- c(15,17,16,4,18,25)




mydf = melt(mydf, id.vars = "Year")


gp1 = ggplot(mydf, aes(x = as.factor(Year), y = value, group = variable, colour = variable, shape = variable)) + 
  scale_fill_manual(values=cPalette) +
  scale_colour_manual(values=cPalette) +
  scale_shape_manual(values=cMarker) +
  geom_line(size=1) +
  geom_point(size=4) +
  ylim(0,100) +
  xlab("Year") +
  ylab("Crop area (Mha)") +
  theme_bw() +
  guides(fill=guide_legend(title=NULL)) +
  theme(text = element_text(size=20)) +
  theme(legend.title=element_blank()) +
  theme(legend.position="top") +
  theme(legend.key.height=unit(2,"line")) +
  theme(legend.key.width=unit(2,"line")) 


gp1 
ggsave(filename = file.path(pathdir, "FigureResultCropLand.pdf"), plot = gp1, device = "pdf", width =  9)
ggsave(filename = file.path(pathdir, "FigureResultCropLand.png"), plot = gp1, device = "png", width =  9)
dev.off()



#--------------------------  Pasture Area  -----------------------------------------
#-------------------------------------------------------------------------------

#GRSLND     	2000	2005	2010	2015	2020	2025	2030	2035	2040	2045	2050

#BASE,Brazil,GrsLnd,165.449,157.648,164.268,173.555,181.034,185.07,187.993,188.037,186.963,182.71,177.07
#FC,Brazil,GrsLnd,165.449,157.648,164.268,173.555,181.034,179.914,178.715,172.973,171.389,169.037,166.746
#ZD_F,Brazil,GrsLnd,165.449,157.648,164.268,173.555,181.034,182.073,183.244,182.371,181.607,179.103,176.909
#ZD_FOWL,Brazil,GrsLnd,165.449,157.648,164.268,173.555,181.034,178.516,176.018,174.163,171.982,169.257,166.261
#ZD_ALL,Brazil,GrsLnd,165.449,157.648,164.268,173.555,181.034,173.816,168.347,164.011,160.844,157.261,153.888

mydfpast = data.frame(
  ZD_ALL	     = c(181.034,173.816,168.347,164.011,160.844,157.261,153.888),
  ZD_FOWL           = c(181.034,178.516,176.018,174.163,171.982,169.257,166.261),
  ZD_F	     = c(181.034,182.073,183.244,182.371,181.607,179.103,176.909),
  FC      = c(181.034,179.914,178.715,172.973,171.389,169.037,166.746),
  BASE	       = c(181.034,185.07,187.993,188.037,186.963,182.71,177.07),
  Year         = c(2020,	2025,	2030,	2035,	2040,	2045,	2050))


cPalette <- c("darkgreen","forestgreen","green","#9DC209","Blue")
cMarker <- c(15,17,16,4,18,25)




mydfpast = melt(mydfpast, id.vars = "Year")


gp3 = ggplot(mydfpast, aes(x = as.factor(Year), y = value, group = variable, colour = variable, shape = variable)) + 
  #  scale_x_continuous(breaks=mydf1$Year) +
  scale_fill_manual(values=cPalette) +
  scale_colour_manual(values=cPalette) +
  scale_shape_manual(values=cMarker) +
  geom_line(size=1) +
  geom_point(size=4) +
  ylim(0,200) +
  xlab("Year") +
  ylab("Pasture area (Mha)") +
  theme_bw() +
  guides(fill=guide_legend(title=NULL)) +
  theme(text = element_text(size=20)) +
  theme(legend.title=element_blank()) +
  theme(legend.position="top") +
  theme(legend.key.height=unit(2,"line")) +
  theme(legend.key.width=unit(2,"line")) 


gp3 
ggsave(filename = file.path(pathdir, "FigureResultPasture.pdf"), plot = gp3, device = "pdf", width =  9)
ggsave(filename = file.path(pathdir, "FigureResultPasture.png"), plot = gp3, device = "png", width =  9)
dev.off()


#-------------------------- ForReg (Mha)	  -----------------------------------------
#------------------------------------------------------------------------------------------------------




# 2025	2030	2035	2040	2045	2050
#,,,2025,2030,2035,2040,2045,2050
#FC,Brazil,ForReg,0.551835,3.61091,12.2471,12.3738,12.5393,12.569

mydfforreg = data.frame(
  FC           = c(0.0, 0.551835,3.61091,12.2471,12.3738,12.5393,12.569),
  Year         = c(2020, 2025,	2030,	2035,	2040,	2045,	2050))


mydfforreg = melt(mydfforreg, id.vars = "Year")


gp8 = ggplot(mydfforreg, aes(x = as.factor(Year), y = value, group = variable, colour = variable, shape = variable)) + 
  #  scale_x_continuous(breaks=mydf1$Year) +
  scale_fill_manual(values=cPalette) +
  scale_colour_manual(values=cPalette) +
  scale_shape_manual(values=cMarker) +
  geom_line(size=1) +
  geom_point(size=4) +
  #  ylim(0,1.5) +
  xlab("Year") +
  ylab("Forest regrowth (Mha)") +
  scale_y_continuous(limits = c(0,15))+
  theme_bw() +
  guides(fill=guide_legend(title=NULL)) +
  theme(text = element_text(size=20)) +
  theme(legend.title=element_blank()) +
  theme(legend.position="top") +
  theme(legend.key.height=unit(2,"line")) +
  theme(legend.key.width=unit(2,"line")) 


gp8 
ggsave(filename = file.path(pathdir, "FigureResultForReg.pdf"), plot = gp8, device = "pdf", width =  9)
ggsave(filename = file.path(pathdir, "FigureResultForReg.png"), plot = gp8, device = "png", width =  9)

dev.off()


#-------------------------- Native vegetation (PRI+MNG) (Mha)	  -----------------------------------------
#------------------------------------------------------------------------------------------------------

#,,,2000,2005,2010,2015,2020,2025,2030,2035,2040,2045,2050
#BASE,Brazil,PriMng,595.577,586.235,564.182,545.003,530.071,514.927,501.303,490.365,480.701,473.122,466.339
#FC,Brazil,PriMng,595.577,586.235,564.182,545.003,530.071,522.184,514.582,508.846,504.408,500.915,497.826
#ZD_F,Brazil,PriMng,595.577,586.235,564.182,545.003,530.071,519.221,509.286,501.524,494.779,489.299,484.407
#ZD_FOWL,Brazil,PriMng,595.577,586.235,564.182,545.003,530.071,523.887,518.573,514.008,510.512,507.144,504.306
#ZD_ALL,Brazil,PriMng,595.577,586.235,564.182,545.003,530.071,529.834,528.838,527.852,526.596,525.254,523.8

mydfnatveg = data.frame(
  ZD_ALL	     = c(530.071,529.834,528.838,527.852,526.596,525.254,523.8),
  ZD_FOWL           = c(530.071,523.887,518.573,514.008,510.512,507.144,504.306),
  ZD_F	     = c(530.071,519.221,509.286,501.524,494.779,489.299,484.407),
  FC      = c(530.071,522.184,514.582,508.846,504.408,500.915,497.826),
  BASE	       = c(530.071,514.927,501.303,490.365,480.701,473.122,466.339),
  Year         = c(2020,	2025,	2030,	2035,	2040,	2045,	2050))

mydfnatveg = melt(mydfnatveg, id.vars = "Year")


gp9 = ggplot(mydfnatveg, aes(x = as.factor(Year), y = value, group = variable, colour = variable, shape = variable)) + 
  #  scale_x_continuous(breaks=mydf1$Year) +
  scale_fill_manual(values=cPalette) +
  scale_colour_manual(values=cPalette) +
  scale_shape_manual(values=cMarker) +
  geom_line(size=1) +
  geom_point(size=4) +
  #  ylim(0,1.5) +
  xlab("Year") +
  ylab("Native Vegetation (Mha)") +
  theme_bw() +
  guides(fill=guide_legend(title=NULL)) +
  theme(text = element_text(size=20)) +
  theme(legend.title=element_blank()) +
  theme(legend.position="top") +
  theme(legend.key.height=unit(2,"line")) +
  theme(legend.key.width=unit(2,"line")) 


gp9 
ggsave(filename = file.path(pathdir, "FigureResultNatVegetation.pdf"), plot = gp9, device = "pdf", width =  9)
ggsave(filename = file.path(pathdir, "FigureResultNatVegetation.png"), plot = gp9, device = "png", width =  9)

dev.off()


#-------------------------- Native vegetation (PRI+MNG+FORREG) (Mha)	  -----------------------------------------
#------------------------------------------------------------------------------------------------------

#,,,2000,2005,2010,2015,2020,2025,2030,2035,2040,2045,2050
#FC,Brazil,Forest,595.577,586.235,564.182,545.003,530.071,522.736,518.193,521.093,516.782,513.454,510.395

mydftotnatveg = data.frame(
  ZD_ALL	     = c(530.071,529.834,528.838,527.852,526.596,525.254,523.8),
  ZD_FOWL           = c(530.071,523.887,518.573,514.008,510.512,507.144,504.306),
  ZD_F	     = c(530.071,519.221,509.286,501.524,494.779,489.299,484.407),
  FC      = c(530.071,522.736,518.193,521.093,516.782,513.454,510.395),
  BASE	       = c(530.071,514.927,501.303,490.365,480.701,473.122,466.339),
  Year         = c(2020,	2025,	2030,	2035,	2040,	2045,	2050))

mydftotnatveg = melt(mydftotnatveg, id.vars = "Year")


gp10 = ggplot(mydftotnatveg, aes(x = as.factor(Year), y = value, group = variable, colour = variable, shape = variable)) + 
  #  scale_x_continuous(breaks=mydf1$Year) +
  scale_fill_manual(values=cPalette) +
  scale_colour_manual(values=cPalette) +
  scale_shape_manual(values=cMarker) +
  geom_line(size=1) +
  geom_point(size=4) +
  #  ylim(0,1.5) +
  xlab("Year") +
  ylab("Total Native Vegetation (Mha)") +
  theme_bw() +
  scale_y_continuous(limits = c(450,550))+
  guides(fill=guide_legend(title=NULL)) +
  theme(text = element_text(size=20)) +
  theme(legend.title=element_blank()) +
  theme(legend.position="top") +
  theme(legend.key.height=unit(2,"line")) +
  theme(legend.key.width=unit(2,"line")) 

gp10 
ggsave(filename = file.path(pathdir, "FigureResultTotalNatVegetation.pdf"), plot = gp10, device = "pdf", width =  9)
ggsave(filename = file.path(pathdir, "FigureResultTotalNatVegetation.png"), plot = gp10, device = "png", width =  9)

dev.off()


#-------------------------- Planted Forest (Mha)	  -----------------------------------------
#------------------------------------------------------------------------------------------------------

#,,,2000,2005,2010,2015,2020,2025,2030,2035,2040,2045,2050
#BASE,Brazil,PltFor,3.93115,5.02012,6.07871,7.22085,8.50493,9.84746,11.1567,12.5446,13.7695,14.9936,16.1992
#FC,Brazil,PltFor,3.93115,5.02012,6.07871,7.22085,8.50493,9.84746,11.18,12.5361,13.7778,15.0427,16.3154
#ZD_F,Brazil,PltFor,3.93115,5.02012,6.07871,7.22085,8.50493,9.84746,11.1862,12.5564,13.7849,15.0361,16.2233
#ZD_FOWL,Brazil,PltFor,3.93115,5.02012,6.07871,7.22085,8.50493,9.84746,11.1772,12.5558,13.771,14.9893,16.1855
#ZD_ALL,Brazil,PltFor,3.93115,5.02012,6.07871,7.22085,8.50493,9.84746,11.1588,12.5316,13.7704,14.984,16.1671


mydfpltfor = data.frame(
  ZD_ALL	     = c(8.50493,9.84746,11.1588,12.5316,13.7704,14.984,16.1671),
  ZD_FOWL           = c(8.50493,9.84746,11.1772,12.5558,13.771,14.9893,16.1855),
  ZD_F	     = c(8.50493,9.84746,11.1862,12.5564,13.7849,15.0361,16.2233),
  FC      = c(8.50493,9.84746,11.18,12.5361,13.7778,15.0427,16.3154),
  BASE	       = c(8.50493,9.84746,11.1567,12.5446,13.7695,14.9936,16.1992),
  Year         = c(2020,	2025,	2030,	2035,	2040,	2045,	2050))

mydfpltfor = melt(mydfpltfor, id.vars = "Year")


gp11 = ggplot(mydfpltfor, aes(x = as.factor(Year), y = value, group = variable, colour = variable, shape = variable)) + 
  #  scale_x_continuous(breaks=mydf1$Year) +
  scale_fill_manual(values=cPalette) +
  scale_colour_manual(values=cPalette) +
  scale_shape_manual(values=cMarker) +
  geom_line(size=1) +
  geom_point(size=4) +
  #  ylim(0,1.5) +
  xlab("Year") +
  ylab("Planted Forest (Mha)") +
  theme_bw() +
  scale_y_continuous(limits = c(0,50))+
  guides(fill=guide_legend(title=NULL)) +
  theme(text = element_text(size=20)) +
  theme(legend.title=element_blank()) +
  theme(legend.position="top") +
  theme(legend.key.height=unit(2,"line")) +
  theme(legend.key.width=unit(2,"line")) 

gp11
ggsave(filename = file.path(pathdir, "FigureResultPltFor.pdf"), plot = gp11, device = "pdf", width =  9)
ggsave(filename = file.path(pathdir, "FigureResultPltFor.png"), plot = gp11, device = "png", width =  9)
dev.off()