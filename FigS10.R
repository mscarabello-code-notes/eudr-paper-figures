#-------------------------------------------------------------------------------

#Limpar o ambiente
rm(list = ls())
library(ggplot2)
library(reshape2)

# Armazena o caminho da pasta do Projeto
path <- getwd()
pathdir <- paste(path, "data/", sep = "/")
if (!dir.exists(pathdir)) {dir.create(pathdir)}

#--------------------------  Soybean area  -----------------------------------------
#,,,2000,2005,2010,2015,2020,2025,2030,2035,2040,2045,2050
#BASE,NDC,Brazil,Soya,13.1988,19.7173,27.9838,32.8853,37.8804,40.8689,44.9448,46.6494,47.5817,51.1093,50.7559
#FC,NDC,Brazil,Soya,13.1988,19.7173,27.9838,32.8853,37.8804,40.7746,44.5526,45.8794,46.8083,50.1734,49.851
#ZD_F, NDC,Brazil,Soya,13.1988,19.7173,27.9838,32.8853,37.8804,40.8829,44.9236,46.6348,47.4563,50.9718,50.776
#ZD_FOWL,NDC,Brazil,Soya,13.1988,19.7173,27.9838,32.8853,37.8804,40.6931,44.487,46.1712,47.1984,50.645,50.3884
#ZD_ALL,NDC,Brazil,Soya,13.1988,19.7173,27.9838,32.8853,37.8804,40.5937,44.2454,45.4522,46.4184,49.4127,48.8686

mydf = data.frame(
  ZD_ALL	     = c(37.8804,40.5937,44.2454,45.4522,46.4184,49.4127,48.8686),
  ZD_FOWL           = c(37.8804,40.6931,44.487,46.1712,47.1984,50.645,50.3884),
  ZD_F	     = c(37.8804,40.8829,44.9236,46.6348,47.4563,50.9718,50.776),
  FC      = c(37.8804,40.7746,44.5526,45.8794,46.8083,50.1734,49.851),
  BASE	       = c(37.8804,40.8689,44.9448,46.6494,47.5817,51.1093,50.7559),
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
  ylab("Soyland (Mha)") +
  theme_bw() +
  guides(fill=guide_legend(title=NULL)) +
  theme(text = element_text(size=20)) +
  theme(legend.title=element_blank()) +
  theme(legend.position="top") +
  theme(legend.key.height=unit(2,"line")) +
  theme(legend.key.width=unit(2,"line")) 


gp1 
ggsave(filename = file.path(pathdir, "FigureResultSoyLand.pdf"), plot = gp1, device = "pdf", width =  9)
ggsave(filename = file.path(pathdir, "FigureResultSoyLand.png"), plot = gp1, device = "png", width =  9)
dev.off()

#--------------------------  Soybean production  -----------------------------------------
#,,,2000,2005,2010,2015,2020,2025,2030,2035,2040,2045,2050
#BASE, BrazilReg,Soya,Supply,32.376,52.6867,80.6715,99.874,120.181,133.83,154.694,162.731,170.77,188.773,190.481
#FC,BrazilReg,Soya,Supply,32.376,52.6867,80.6715,99.874,120.181,133.697,153.296,161.389,167.71,184.055,185.503
#ZD_F,BrazilReg,Soya,Supply,32.376,52.6867,80.6715,99.874,120.181,133.937,154.64,162.784,170.428,188.544,190.505
#ZD_FOWL,BrazilReg,Soya,Supply,32.376,52.6867,80.6715,99.874,120.181,133.634,152.882,161.478,167.575,183.787,185.061
#ZD_ALL,BrazilReg,Soya,Supply,32.376,52.6867,80.6715,99.874,120.181,133.483,151.89,159.414,165.565,180.563,180.305

mydf = data.frame(
  ZD_ALL	     = c(120.181,133.483,151.89,159.414,165.565,180.563,180.305),
  ZD_FOWL           = c(120.181,133.634,152.882,161.478,167.575,183.787,185.061),
  ZD_F	     = c(120.181,133.937,154.64,162.784,170.428,188.544,190.505),
  FC      = c(120.181,133.697,153.296,161.389,167.71,184.055,185.503),
  BASE	       = c(120.181,133.83,154.694,162.731,170.77,188.773,190.481),
  Year         = c(2020,	2025,	2030,	2035,	2040,	2045,	2050))


cPalette <- c("darkgreen","forestgreen","green","#9DC209","Blue")
cMarker <- c(15,17,16,4,18,25)

mydf = melt(mydf, id.vars = "Year")

gp2 = ggplot(mydf, aes(x = as.factor(Year), y = value, group = variable, colour = variable, shape = variable)) + 
  scale_fill_manual(values=cPalette) +
  scale_colour_manual(values=cPalette) +
  scale_shape_manual(values=cMarker) +
  geom_line(size=1) +
  geom_point(size=4) +
  ylim(0,250) +
  xlab("Year") +
  ylab("Soy Production (Mton)") +
  theme_bw() +
  guides(fill=guide_legend(title=NULL)) +
  theme(text = element_text(size=20)) +
  theme(legend.title=element_blank()) +
  theme(legend.position="top") +
  theme(legend.key.height=unit(2,"line")) +
  theme(legend.key.width=unit(2,"line")) 


gp2 
ggsave(filename = file.path(pathdir, "FigureResultSoyProduction.pdf"), plot = gp2, device = "pdf", width =  9)
ggsave(filename = file.path(pathdir, "FigureResultSoyProduction.png"), plot = gp2, device = "png", width =  9)
dev.off()

#--------------------------  Soybean Exports  -----------------------------------------
#,,,2000,2005,2010,2015,2020,2025,2030,2035,2040,2045,2050
#BASE,BrazilReg,Soya,Export,22.2162,39.203,61.3723,77.3786,88.8137,100.467,109.385,118.603,127.921,136.527,141.078
#FC,BrazilReg,Soya,Export,22.2162,39.203,61.3723,77.3786,88.8137,100.395,108.717,117.288,125.005,131.952,136.19
#ZD_F,BrazilReg,Soya,Export,22.2162,39.203,61.3723,77.3786,88.8137,100.571,109.406,118.66,127.694,136.423,141.088
#ZD_FOWL,BrazilReg,Soya,Export,22.2162,39.203,61.3723,77.3786,88.8137,100.276,108.375,117.483,125.023,131.855,135.998
#ZD_ALL,BrazilReg,Soya,Export,22.2162,39.203,61.3723,77.3786,88.8137,100.135,107.471,115.641,123.256,128.888,131.556


mydf = data.frame(
  ZD_ALL	     = c(88.8137,100.135,107.471,115.641,123.256,128.888,131.556),
  ZD_FOWL           = c(88.8137,100.276,108.375,117.483,125.023,131.855,135.998),
  ZD_F	     = c(88.8137,100.571,109.406,118.66,127.694,136.423,141.088),
  FC      = c(88.8137,100.395,108.717,117.288,125.005,131.952,136.19),
  BASE	       = c(88.8137,100.467,109.385,118.603,127.921,136.527,141.078),
  Year         = c(2020,	2025,	2030,	2035,	2040,	2045,	2050))


cPalette <- c("darkgreen","forestgreen","green","#9DC209","Blue")
cMarker <- c(15,17,16,4,18,25)

mydf = melt(mydf, id.vars = "Year")

gp3 = ggplot(mydf, aes(x = as.factor(Year), y = value, group = variable, colour = variable, shape = variable)) + 
  scale_fill_manual(values=cPalette) +
  scale_colour_manual(values=cPalette) +
  scale_shape_manual(values=cMarker) +
  geom_line(size=1) +
  geom_point(size=4) +
  ylim(0,200) +
  xlab("Year") +
  ylab("Soy Exports (Mton)") +
  theme_bw() +
  guides(fill=guide_legend(title=NULL)) +
  theme(text = element_text(size=20)) +
  theme(legend.title=element_blank()) +
  theme(legend.position="top") +
  theme(legend.key.height=unit(2,"line")) +
  theme(legend.key.width=unit(2,"line")) 


gp3 
ggsave(filename = file.path(pathdir, "FigureResultSoyExports.pdf"), plot = gp3, device = "pdf", width =  9)
ggsave(filename = file.path(pathdir, "FigureResultSoyExports.png"), plot = gp3, device = "png", width =  9)
dev.off()


#--------------------------  Cattle HERD  -----------------------------------------
#,,,2000,2005,2010,2015,2020,2025,2030,2035,2040,2045,2050
#BASE,NDC,Brazil,BOVI,116.742,115.845,123.92,135,147.329,159.386,168.981,177.887,185.099,190.795,193.736
#FC,NDC,Brazil,BOVI,116.742,115.845,123.92,135,147.329,158.366,165.885,171.349,175.26,178.158,180.491
#ZD_F,NDC,Brazil,BOVI,116.742,115.845,123.92,135,147.329,158.874,167.613,174.946,179.741,184.07,186.689
#ZD_FOWL,NDC,Brazil,BOVI,116.742,115.845,123.92,135,147.329,158.299,165.511,171.399,175.297,178.311,180.996
#ZD_ALL,NDC,Brazil,BOVI,116.742,115.845,123.92,135,147.329,157.048,164.263,167.8,171.134,173.043,173.706

mydf = data.frame(
  ZD_ALL	     = c(147.329,157.048,164.263,167.8,171.134,173.043,173.706),
  ZD_FOWL           = c(147.329,158.299,165.511,171.399,175.297,178.311,180.996),
  ZD_F	     = c(147.329,158.874,167.613,174.946,179.741,184.07,186.689),
  FC      = c(147.329,158.366,165.885,171.349,175.26,178.158,180.491),
  BASE	       = c(147.329,159.386,168.981,177.887,185.099,190.795,193.736),
  Year         = c(2020,	2025,	2030,	2035,	2040,	2045,	2050))


cPalette <- c("darkgreen","forestgreen","green","#9DC209","Blue")
cMarker <- c(15,17,16,4,18,25)

mydf = melt(mydf, id.vars = "Year")

gp4 = ggplot(mydf, aes(x = as.factor(Year), y = value, group = variable, colour = variable, shape = variable)) + 
  scale_fill_manual(values=cPalette) +
  scale_colour_manual(values=cPalette) +
  scale_shape_manual(values=cMarker) +
  geom_line(size=1) +
  geom_point(size=4) +
  ylim(0,250) +
  xlab("Year") +
  ylab("Cattle Herd (MTLU)") +
  theme_bw() +
  guides(fill=guide_legend(title=NULL)) +
  theme(text = element_text(size=20)) +
  theme(legend.title=element_blank()) +
  theme(legend.position="top") +
  theme(legend.key.height=unit(2,"line")) +
  theme(legend.key.width=unit(2,"line")) 


gp4 
ggsave(filename = file.path(pathdir, "FigureResultCattleHerd.pdf"), plot = gp4, device = "pdf", width =  9)
ggsave(filename = file.path(pathdir, "FigureResultCattleHerd.png"), plot = gp4, device = "png", width =  9)
dev.off()


#--------------------------  BVMEAT PRODUCTION  -----------------------------------------
#,,,2000,2005,2010,2015,2020,2025,2030,2035,2040,2045,2050
#BASE,BrazilReg,BVMEAT,Supply,6.54443,7.02142,7.52115,8.38774,9.42862,10.5048,11.3833,12.3762,13.2119,13.9921,14.56
#FC,BrazilReg,BVMEAT,Supply,6.54443,7.02142,7.52115,8.38774,9.42862,10.4952,11.2656,11.9833,12.6069,13.1615,13.6905
#ZD_F,BrazilReg,BVMEAT,Supply,6.54443,7.02142,7.52115,8.38774,9.42862,10.5028,11.3421,12.2327,12.9209,13.5999,14.0892
#ZD_FOWL,BrazilReg,BVMEAT,Supply,6.54443,7.02142,7.52115,8.38774,9.42862,10.4952,11.2563,11.9945,12.6086,13.1669,13.7257
#ZD_ALL,BrazilReg,BVMEAT,Supply,6.54443,7.02142,7.52115,8.38774,9.42862,10.4115,11.1954,11.7661,12.3413,12.8002,13.1561


mydf = data.frame(
  ZD_ALL	     = c(9.42862,10.4115,11.1954,11.7661,12.3413,12.8002,13.1561),
  ZD_FOWL           = c(9.42862,10.4952,11.2563,11.9945,12.6086,13.1669,13.7257),
  ZD_F	     = c(9.42862,10.5028,11.3421,12.2327,12.9209,13.5999,14.0892),
  FC      = c(9.42862,10.4952,11.2656,11.9833,12.6069,13.1615,13.6905),
  BASE	       = c(9.42862,10.5048,11.3833,12.3762,13.2119,13.9921,14.56),
  Year         = c(2020,	2025,	2030,	2035,	2040,	2045,	2050))


cPalette <- c("darkgreen","forestgreen","green","#9DC209","Blue")
cMarker <- c(15,17,16,4,18,25)

mydf = melt(mydf, id.vars = "Year")

gp5 = ggplot(mydf, aes(x = as.factor(Year), y = value, group = variable, colour = variable, shape = variable)) + 
  scale_fill_manual(values=cPalette) +
  scale_colour_manual(values=cPalette) +
  scale_shape_manual(values=cMarker) +
  geom_line(size=1) +
  geom_point(size=4) +
  ylim(0,16) +
  xlab("Year") +
  ylab("Beef Production (Mton)") +
  theme_bw() +
  guides(fill=guide_legend(title=NULL)) +
  theme(text = element_text(size=20)) +
  theme(legend.title=element_blank()) +
  theme(legend.position="top") +
  theme(legend.key.height=unit(2,"line")) +
  theme(legend.key.width=unit(2,"line")) 


gp5 
ggsave(filename = file.path(pathdir, "FigureResultBeefProduction.pdf"), plot = gp5, device = "pdf", width =  9)
ggsave(filename = file.path(pathdir, "FigureResultBeefProduction.png"), plot = gp5, device = "png", width =  9)
dev.off()


#--------------------------  BVMEAT EXPORT  -----------------------------------------
#,,,2000,2005,2010,2015,2020,2025,2030,2035,2040,2045,2050
#BASE,BrazilReg,BVMEAT,Export,0.526328,0.841762,1.13239,1.73298,2.41863,3.12788,3.62297,4.28725,4.80188,5.34467,5.658
#FC,BrazilReg,BVMEAT,Export,0.526328,0.841762,1.13239,1.73298,2.41863,3.11832,3.57909,4.06427,4.49285,4.90094,5.26588
#ZD_F,BrazilReg,BVMEAT,Export,0.526328,0.841762,1.13239,1.73298,2.41863,3.12591,3.58172,4.22793,4.6327,5.1621,5.43937
#ZD_FOWL,BrazilReg,BVMEAT,Export,0.526328,0.841762,1.13239,1.73298,2.41863,3.1183,3.57912,4.07551,4.49453,4.90638,5.28274
#ZD_ALL,BrazilReg,BVMEAT,Export,0.526328,0.841762,1.13239,1.73298,2.41863,3.11611,3.51822,3.93454,4.40814,4.72377,4.98988


mydf = data.frame(
  ZD_ALL	     = c(2.41863,3.11611,3.51822,3.93454,4.40814,4.72377,4.98988),
  ZD_FOWL           = c(2.41863,3.1183,3.57912,4.07551,4.49453,4.90638,5.28274),
  ZD_F	     = c(2.41863,3.12591,3.58172,4.22793,4.6327,5.1621,5.43937),
  FC      = c(2.41863,3.11832,3.57909,4.06427,4.49285,4.90094,5.26588),
  BASE	       = c(2.41863,3.12788,3.62297,4.28725,4.80188,5.34467,5.658),
  Year         = c(2020,	2025,	2030,	2035,	2040,	2045,	2050))


cPalette <- c("darkgreen","forestgreen","green","#9DC209","Blue")
cMarker <- c(15,17,16,4,18,25)

mydf = melt(mydf, id.vars = "Year")

gp6 = ggplot(mydf, aes(x = as.factor(Year), y = value, group = variable, colour = variable, shape = variable)) + 
  scale_fill_manual(values=cPalette) +
  scale_colour_manual(values=cPalette) +
  scale_shape_manual(values=cMarker) +
  geom_line(size=1) +
  geom_point(size=4) +
  ylim(0,10) +
  xlab("Year") +
  ylab("Beef Exports (Mton)") +
  theme_bw() +
  guides(fill=guide_legend(title=NULL)) +
  theme(text = element_text(size=20)) +
  theme(legend.title=element_blank()) +
  theme(legend.position="top") +
  theme(legend.key.height=unit(2,"line")) +
  theme(legend.key.width=unit(2,"line")) 


gp6
ggsave(filename = file.path(pathdir, "FigureResultBeefExports.pdf"), plot = gp6, device = "pdf", width =  9)
ggsave(filename = file.path(pathdir, "FigureResultBeefExports.png"), plot = gp6, device = "png", width =  9)
dev.off()


