library(data.table)
library(tools)
library(lidR)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(gstat)




allpcs <- list.files("D:/1_Work/2_Ciron/Data/ULM/LAS/unnorm/plots/15m_rad_test/may2021/allpoints/",
                     pattern = "*.las",
                     full.names = TRUE)

alldtms <- sapply(allpcs, function(x){
  ls <- readLAS(x)
  dtm <- grid_terrain(ls, res = 0.1, algorithm = tin())},
  simplify = FALSE,
  USE.NAMES = TRUE)
names(alldtms) <- basename(file_path_sans_ext(names(alldtms)))


allvoxfiles <- as.data.table(list.files("D:/1_Work/2_Ciron/Voxelisation/Results/May/wo_interpolation/voxfiles/all/", 
                                        pattern = "*.vox",
                                        full.names = TRUE))


allvoxfiles[, grp := sub("@.*","",basename(file_path_sans_ext(V1)))]
# allvoxfiles[, grp := sub("_.*","",basename(file_path_sans_ext(V1)))]
allvoxfiles[, grp1 := sub(".*@","",basename(file_path_sans_ext(V1)))]
allvoxfiles[, c("m1","m2","m3"):=tstrsplit(grp1,"_",fixed=T),]
allvoxfiles$m1 <- as.numeric(allvoxfiles$m1)
allvoxfiles$m2 <- as.numeric(allvoxfiles$m2)
allvoxfiles$m3 <- as.numeric(allvoxfiles$m3)
allvoxfiles <- allvoxfiles[, m2 := ifelse(is.na(m2), m1, m2),]
allvoxfiles <- allvoxfiles[, m3 := ifelse(is.na(m3), m2, m3),]
allvoxfiles <- allvoxfiles[m1 < 40]
allvoxfiles <- allvoxfiles[m2 < 40]
allvoxfiles <- allvoxfiles[m3 < 40]
allvoxfiles <- allvoxfiles[!m1 %in% c(37.24, 7.16)]
allvoxfiles <- allvoxfiles[!m2 %in% c(37.24, 7.16)]
allvoxfiles <- allvoxfiles[!m3 %in% c(37.24, 7.16)]

allvoxfiles <- allvoxfiles[allvoxfiles$grp1!="n",]
voxmergedall <- list()
allplots <- list()
for(group in unique(allvoxfiles$grp))
{
  voxfiles <- allvoxfiles[allvoxfiles$grp==group,]
  txt <- readLines(voxfiles$V1[[1]], n=3)[2:3]
  zmin <- as.numeric(unlist(strsplit(txt[[1]], "\\s+"))[4])
  lasnm <- paste0("D:/1_Work/2_Ciron/Data/ULM/LAS/unnorm/plots/15m_rad_test/may2021/allpoints/",
                  group,
                  "@all",
                  ".las")
  ls <- readLASheader(lasnm)
  empty_raster <- raster(ncol=round(ls@PHB[["Max X"]]-ls@PHB[["Min X"]]), 
                         nrow=round(ls@PHB[["Max Y"]]-ls@PHB[["Min Y"]]),
                         xmn=ls@PHB[["Min X"]], xmx= ls@PHB[["Max X"]], 
                         ymn=ls@PHB[["Min Y"]], ymx= ls@PHB[["Max Y"]])
  dt <- alldtms[[paste0(group, "@all")]]
  dt <- resample(dt, empty_raster, method='bilinear')
  dt_mat <- as.matrix(dt)
  
  voxtbls <- sapply(voxfiles$V1, 
                    function(voxfile) {
                      voxtbl <- fread(voxfile, na.strings = "NA" , skip = 5)
                      voxtbl <- voxtbl[,1:4][,alt := k+zmin+0.5][,dtm := dt_mat[cbind(nrow(dt_mat)-j, i+1)]]
                      #voxtbl <- na.omit(voxtbl)
                      voxtbl <- voxtbl[alt>dtm]
                      voxtbl <- voxtbl[, k1:= order(k), by=list(i,j)]
                      voxtbl <- voxtbl[, k1:= k1-0.5]
                      voxtbl <- voxtbl[, .(m = mean(PadBVTotal, na.rm = TRUE)), by=list(k1)]
                      return(voxtbl)
                    },
                    simplify = FALSE,
                    USE.NAMES = TRUE)
  
  voxmerged <- data.table::rbindlist(voxtbls, idcol = "id")
  voxmerged[, meanang := sub(".*@", "", basename(tools::file_path_sans_ext(id)))]
  voxmerged[, id_placette := group,]
  voxmerged <- voxmerged %>% 
    mutate(test=if_else(lag(m)==0 & m==0,"x", "y")) %>% 
    filter(test!="x")
  voxmergedall[[group]] <- voxmerged
}

voxmergedall <- lapply(voxmergedall, function(x){
  x <- x[, grp:=ifelse(str_count(meanang, "_")==0, 1,
                        ifelse(str_count(meanang, "_")==1, 2, 3))]
  return(x)
})


profile_plot <- function(profs)
{
  plt <- ggplot(data=profs[k1>2 & grp==2], aes(x=k1, y=m, colour = meanang, linetype = meanang))+
    scale_colour_brewer(palette="Paired")+
    theme_minimal()+
    geom_line(size=0.8)+
    coord_flip()+
    labs(title = paste0("Vertical profile for ", unique(profs$id_placette)),
         y ="PAD", 
         x = "Height above ground (m)", 
         colour="Mean scan angle", 
         linetype = "Mean scan angle")+
    facet_wrap(~grp)
  return(plt)
}

allplots <-  lapply(voxmergedall, profile_plot)


for(name in names(allplots))
{
  nm <- paste0("D:/1_Work/2_Ciron/voxelisation/Results/26jan/w_wt/",name,".png")
  ggsave(nm, allplots[[name]], device="png")
}


















voxtbl <- fread(allvoxfiles$V1[1], na.strings = "NA" , skip = 5)
voxtbl <- voxtbl[,1:4][,alt := k+zmin][,dtm := dt_mat[cbind(nrow(dt_mat)-j, i+1)]]
#voxtbl <- na.omit(voxtbl)
voxtbl <- voxtbl[alt>dtm]
voxtbl <- voxtbl[, k1:= order(k), by=list(i,j)]
voxtbl <- voxtbl[, .(m = mean(PadBVTotal, na.rm = TRUE)), by=list(k1)]
