library(data.table)
library(future)
library(lidR)
library(sp)
library(sf)

#read the file with the coordinates
lasc73 <- readALSLAScatalog("D:/1_Work/5_Bauges/Data/ULM/LAS/unnorm/02_NUAGE_Z_L93/")
lasc74 <- readALSLAScatalog("D:/1_Work/5_Bauges/Data/ULM/LAS/unnorm/LAS_Altitude_500m/")


bauges.db <- fread("D:/1_Work/__R_codes/Projects/scangle_effect_ba/data/bauges_db15jul.csv", sep = ",")

#changing name from Id_plac to id_placette.
colnames(bauges.db)[2] <- "id_placette"  

#reclassifying the plots based on the G that includes G for small trees i.e. >7.5cm ; computed by Kamel 
bauges.db <- bauges.db[, newstratum := ifelse(comp_R_G>75 & comp_F_G<25 , "Coniferes",
                                              ifelse(comp_F_G>75 & comp_R_G<25, "Feuillus", "Mixte"))]

#setting a key for the database
setkey(bauges.db, "id_placette")

#subset only the necessary columns
bauges.db <- bauges.db[, c("id_placette","X", "Y" ,"G75", "volume_total", "volume_tige", "stratum", "newstratum")]
bauges.db <- bauges.db[, `:=`(volume_total=(volume_total*10000)/(pi*15*15),
                              volume_tige=(volume_tige*10000)/(pi*15*15))]
ref.73 <- readxl::read_xlsx("D:/1_Work/__R_codes/Projects/scangle_effect_ba/data/table_placette - savoirB.xlsx", sheet = "placette" )
ref.74 <- readxl::read_xlsx("D:/1_Work/__R_codes/Projects/scangle_effect_ba/data/table_placette - PNR74.xlsx", sheet = "placette" )

ref.73 <- as.data.table(ref.73)
ref.74 <- as.data.table(ref.74)

ref.73 <- ref.73[A_exclure=="N"]
ref.74 <- ref.74[A_exclure=="N"]
ref.73 <- ref.73[!Id_plac%in%ref.74$Id_plac]
ref.7374 <- rbind(ref.73, ref.74)

bauges.db <- bauges.db[,dep:=ifelse(id_placette%in%ref.74$Id_plac, 74, 73)]
bauges.db <- bauges.db[id_placette %in% ref.7374$Id_plac]
bauges.db73 <- bauges.db[id_placette%in%ref.73$Id_plac]
bauges.db74 <- bauges.db[id_placette%in%ref.74$Id_plac]


pcs <- c(pc73, pc74)
pc73 <- pcs[names(pcs)%in%ref.73$Id_plac]
pc74 <- pcs[names(pcs)%in%ref.74$Id_plac]



pc <- NULL
db <- bauges.db73
for(i in 1:nrow(db))
{
  xmin <- as.numeric(db$X[i]-750)
  xmax <- as.numeric(db$X[i]+750)
  ymin <- as.numeric(db$Y[i]-750)
  ymax <- as.numeric(db$Y[i]+750)
  pc[[db$id_placette[i]]] <- as(raster::extent(xmin, xmax, ymin, ymax), 
                                   "SpatialPolygons")
}

for(name in names(pc))
{
  pc[[name]]@polygons[[1]]@ID <- name
  
}

joined = SpatialPolygons(lapply(pc, function(x){x@polygons[[1]]}))

plot(lasc74)
plot(joined, add=TRUE, col="red")


las <- clip_roi(lasc73, joined[1])
las <- filter_duplicates(las)
las1 <- filter_firstlast(las)
las1@data[, n := .N , by = c("gpstime")]
las1 <- filter_poi(las1, n==4)
traj <- track_sensor(las1, Roussel2020())
las2 <- filter_poi(las, gpstime %in% las1@data$gpstime)
las1.single <- filter_single(las)

traj74.all <- NULL
traj74.all.bkp <- traj74.all
traj74.all <- traj74.all[,c(1,2,3,4)]
for(row in 111:nrow(bauges.db74))
{
  db <- bauges.db74
  xmin <- as.numeric(db$X[row]-500)
  xmax <- as.numeric(db$X[row]+500)
  ymin <- as.numeric(db$Y[row]-500)
  ymax <- as.numeric(db$Y[row]+500)
  poi <- clip_rectangle(lasc74, xmin, ymin, xmax, ymax)
  poi <- filter_duplicates(poi)
  poi1 <- filter_firstlast(poi)
  poi1@data[, n := .N , by = c("gpstime")]
  poi1 <- filter_poi(poi1, n==2)
  traj74 <- track_sensor(poi1 , Roussel2020(), extra_check = T, thin_pulse_with_time = 0)
  traj74.df <- cbind(traj74@coords,traj74@data)
  traj74.df <- traj74.df[,c(1,2,3,4)]
  traj74.all <- rbind(traj74.all, traj74.df)
  remove(poi1)
  # poi2 <- filter_poi(poi, gpstime %in% poi1@data$gpstime)
  poi3 <- clip_circle(poi, db$X[row], db$Y[row], 15)
  writeLAS(poi3, paste0("D:/1_Work/5_Bauges/Data/test_traj/74/", db$id_placette[row], ".las"))
  writeLAS(poi, paste0("D:/1_Work/5_Bauges/Data/test_traj/ref_tile_for_generation_of_trajectory/74/", db$id_placette[row], ".laz"))
  remove(poi)
  }

write.csv(traj74.all, "D:/1_Work/5_Bauges/Data/test_traj/test_traj74.csv")



for(row in 1:nrow(bauges.db74))
{
  db <- bauges.db74
  xmin <- as.numeric(db$X[row]-500)
  xmax <- as.numeric(db$X[row]+500)
  ymin <- as.numeric(db$Y[row]-500)
  ymax <- as.numeric(db$Y[row]+500)
  poi.dtm <- grid_terrain(clip_circle(lasc74, db$X[row], db$Y[row], 60), algorithm = tin(), res=0.1)
  writeRaster(poi.dtm, paste0("D:/1_Work/5_Bauges/Data/ULM/DTM/plots/december2021/", db$id_placette[row], ".asc"))
}





d


plot(traj)

poi4 <- clip_circle(lasc73, db$X[row], db$Y[row], 30)




traj <- fread("D:/1_Work/5_Bauges/Data/ULM/trajectory/trajec_all_74.txt")
las <- readALSLAS("D:/1_Work/5_Bauges/Data/ULM/LAS/unnorm/LAS_Altitude_500m/001294.laz")
tr <- track_sensor(lasc, algorithm = Roussel2020())

chunk <- readRDS("C:\\Users\\KARUN~1.DAY\\AppData\\Local\\Temp\\RtmpY54KWJ\\chunk2.rds")
las <- readLAS(chunk)
las <- readALSLAS("D:/1_Work/5_Bauges/Data/ULM/LAS/unnorm/LAS_Altitude_500m/001294.laz")

xx <- filter_firstlast(xx)
xx@data[, n := .N , by = c("gpstime")]
xx1 <- filter_poi(xx, n==5)
y=plot(xx1)

x <- (track_sensor(xx1, Roussel2020()))
add_flightlines3d(y, x)

plan(multisession, workers = 11L)
opt_chunk_buffer(lasc) <- 100
opt_chunk_size(lasc)   <- 720
opt_stop_early(lasc) <- FALSE
ss <- track_sensor(lasc, algorithm = Roussel2020())





traj.plots <- NULL
for(name in names(pc73[1:3]))
{
  pc <- pc73[[name]]
  # pc@header@EVLR <- list()
  print(name)
  traj.plots[[name]] <- track_sensor(pc, algorithm = Roussel2020(), extra_check = TRUE)
  warnings()
}

x <- pc74$`171_IRSTEA`
xy <- track_sensor(x, Gatziolis2019())

plot(xy)
plot(xx)


pc@header@EVLR <- list()
pc@header


x <- readLAS("D:/1_Work/5_Bauges/Data/ULM/LAS/unnorm/10_74_ONF.las")
x@header
