library(lidR)
library(dplyr)
library(tidyr)
library(purrr)
library(data.table)
library(tools)

library(ggplot2)
library(ggthemes)
library("RColorBrewer")

lasfiles <- list.files("D:/1_Work/2_Ciron/voxelisation/Results/fl/")

plts <- unique(sort(as.numeric(sub("\\_.*", "", 
                                   list.files("D:/1_Work/2_Ciron/voxelisation/Results/fl/test/")))))


setwd("D:/1_Work/2_Ciron/voxelisation/Results/fl/test/")
vxfile <- as.data.frame(read.csv("D:/1_Work/2_Ciron/voxelisation/test2.vox", 
                                 na.strings = NaN, skip = 5, sep = ""))


ls <- readLAS("D:/1_Work/2_Ciron/Data/ULM/LAS/unnorm/plots/17_5m_rad/16.las")
dt <- grid_terrain(ls, res = 1, algorithm =tin())
dt <- resample()




voxnorm <- function(voxfiles)
{
  lapply(voxfiles, function(x) read.csv(x, na.strings = NaN, skip = 5, sep = ""))
  
  dtm_array <- as.array(dtm)
  
  txt <- readLines("D:/1_Work/2_Ciron/voxelisation/test2.vox", n=3)[2:3]
  
  xmin <- as.numeric(unlist(strsplit(txt[[1]], "\\s+"))[2]) 
  
  ymin <- as.numeric(unlist(strsplit(txt[[1]], "\\s+"))[3])
  
  zmin <- as.numeric(unlist(strsplit(txt[[1]], "\\s+"))[4])
  
  xmax <- as.numeric(unlist(strsplit(txt[[2]], "\\s+"))[2])
  
  ymax <- as.numeric(unlist(strsplit(txt[[2]], "\\s+"))[3])
  
  zmin <- as.numeric(unlist(strsplit(txt[[2]], "\\s+"))[4])
  
}
 


allpcs <- list.files("D:/1_Work/2_Ciron/Data/ULM/LAS/unnorm/plots/17_5m_rad/",
                     pattern = "*.las",
                     full.names = TRUE)




dtms <- function(files)
{
  dtms <- list()
  for(name in allpcs)
  {
    nm <- sub("\\_.*","",basename(file_path_sans_ext(name)))
    ls <- readLAS(name)
    #dtms[nm] <- grid_terrain(ls, res=1, tin())
  }
  return(dtms)
}

dtms <- dtms(allpcs)



alldtms <- sapply(allpcs, function(x){
  ls <- readLAS(x)
  dtm <- grid_terrain(ls, res = 0.1, algorithm = tin())},
simplify = FALSE,
USE.NAMES = TRUE)
names(alldtms) <- basename(file_path_sans_ext(names(alldtms)))


for(name in names(alldtms))
{
  ls
  empty_raster <- raster(ncol=round(ls@PHB[["Max X"]]-ls@PHB[["Min X"]]), 
                         nrow=round(ls@PHB[["Max Y"]]-ls@PHB[["Min Y"]]),
                         xmn=ls@PHB[["Min X"]], xmx= ls@PHB[["Max X"]], 
                         ymn=ls@PHB[["Min Y"]], ymx= ls@PHB[["Max Y"]])
  dt <- resample(dt, empty_raster, method='bilinear')
  dt_mat <- as.matrix(dt)
}



allvoxfiles <- as.data.table(list.files("D:/1_Work/2_Ciron/voxelisation/Results/fl/test/", 
                                        full.names = TRUE))

allvoxfiles[, grp := sub("\\_.*","",basename(file_path_sans_ext(V1)))]



################################################################################
allvoxfiles <- as.data.table(list.files("D:/1_Work/2_Ciron/voxelisation/Results/fl/w_wt/test/", 
                                        pattern = "*.vox",
                                        full.names = TRUE))

allvoxfiles[, grp := sub("\\_.*","",basename(file_path_sans_ext(V1)))]

voxmergedall <- list()
allplots <- list()
for(group in unique(allvoxfiles$grp))
{
  voxfiles <- allvoxfiles[allvoxfiles$grp==group,]
  txt <- readLines(voxfiles$V1[[1]], n=3)[2:3]
  zmin <- as.numeric(unlist(strsplit(txt[[1]], "\\s+"))[4])
  lasnm <- paste0("D:/1_Work/2_Ciron/Data/ULM/LAS/unnorm/plots/17_5m_rad/",
                  group,
                  ".las")
  ls <- readLASheader(lasnm)
  empty_raster <- raster(ncol=round(ls@PHB[["Max X"]]-ls@PHB[["Min X"]]), 
                         nrow=round(ls@PHB[["Max Y"]]-ls@PHB[["Min Y"]]),
                         xmn=ls@PHB[["Min X"]], xmx= ls@PHB[["Max X"]], 
                         ymn=ls@PHB[["Min Y"]], ymx= ls@PHB[["Max Y"]])
  dt <- alldtms[[group]]
  dt <- resample(dt, empty_raster, method='bilinear')
  dt_mat <- as.matrix(dt)
  
  voxtbls <- sapply(voxfiles$V1, 
                    function(voxfile) {
                      voxtbl <- fread(voxfile, na.strings = "NA" , skip = 5)
                      voxtbl <- voxtbl[,1:4][,alt := k+zmin][,dtm := dt_mat[cbind(nrow(dt_mat)-j, i+1)]]
                      voxtbl <- na.omit(voxtbl)
                      voxtbl <- voxtbl[alt>dtm]
                      voxtbl <- voxtbl[, k1:= order(k), by=list(i,j)]
                      voxtbl <- voxtbl[, .(m = mean(PadBVTotal, na.rm = TRUE)), by=list(k1)]
                      return(voxtbl)
                    },
                    simplify = FALSE,
                    USE.NAMES = TRUE)
  voxmerged <- data.table::rbindlist(voxtbls, idcol = "id")
  voxmerged[, id := basename(file_path_sans_ext(id))]
  voxmerged <- voxmerged %>% 
    mutate(test=if_else(lag(m)==0 & m==0,"x", "y")) %>% 
    filter(test!="x")
  voxmergedall[[group]] <- voxmerged
  
  
}

profile_plot <- function(profs, nm)
{
  plt <- ggplot(data=profs[profs$k1>1,], aes(x=k1, y=m, colour = id, linetype = id))+
    scale_colour_brewer(palette="Paired")+
    theme_minimal()+
    geom_line(size=0.8)+
    coord_flip()+
    labs(title = paste0("Vertical profile for ", nm),
         y ="PAD", 
         x = "Height above ground (m)", 
         colour="Mean scan angle", 
         linetype = "Mean scan angle")
  
  
  return(plt)
}

allplots <-  mapply(profile_plot, voxmergedall, names(voxmergedall))


profile_plot()



####################################################################################






















out <- tapply(allvoxfiles$V1, allvoxfiles$grp, function(voxfiles)
{
  
  voxtbls <- sapply(voxfiles, 
                    function(voxfile) {
                      voxtbl <- fread(voxfile, na.strings = "NA" , skip = 5)
                      voxtbl <- voxtbl[,1:4][,alt := k+104.7][,dtm := dt_mat[cbind(nrow(dt_mat)-j, i+1)]]
                      voxtbl <- na.omit(voxtbl)
                      voxtbl <- voxtbl[alt>dtm]
                      voxtbl <- voxtbl[, k1:= order(k), by=list(i,j)]
                      voxtbl <- voxtbl[, .(m = mean(PadBVTotal, na.rm = TRUE)), by=list(k1)]
                      return(voxtbl)
                    },
                    simplify = FALSE,
                    USE.NAMES = TRUE)
  voxmerged <- data.table::rbindlist(voxtbls, idcol = "id")
  voxmerged[, id := basename(file_path_sans_ext(id))]
  voxmerged <- voxmerged %>% 
    mutate(test=if_else(lag(m)==0 & m==0,"x", "y")) %>% 
    filter(test!="x")
  

  return(voxmerged)
  
})












voxtbls <- sapply(voxfiles, 
                  function(x) {
                    voxtbl <- fread(x, na.strings = "NA" , skip = 5)
                    voxtbl <- voxtbl[,1:4][,alt := k+104.7][,dtm := dt_mat[cbind(nrow(dt_mat)-j, i+1)]]
                    voxtbl <- na.omit(voxtbl)
                    voxtbl <- voxtbl[alt>dtm]
                    voxtbl <- voxtbl[, k1:= order(k), by=list(i,j)]
                    voxtbl <- voxtbl[, .(m = mean(PadBVTotal, na.rm = TRUE)), by=list(k1)]
                    return(voxtbl)
                  },
                  simplify = FALSE,
                  USE.NAMES = TRUE)
voxmerged <- data.table::rbindlist(voxtbls, idcol = "id")
voxmerged[, id := basename(file_path_sans_ext(id))]
voxmerged <- voxmerged %>% 
  mutate(test=if_else(lag(m)==0 & m==0,"x", "y")) %>% 
  filter(test!="x")

return(voxmerged)



x <- ggplot(data=voxmerged[voxmerged$k1>1,], aes(x=k1, y=m, colour = id, linetype = id))+
  geom_line()+
  coord_flip()+ 
  theme_minimal()+
  scale_color_brewer(palette="Dark2")+
  labs(y ="PAD", 
       x = "Height above ground (m)", 
       colour="Mean scan angle", 
       linetype = "Mean scan angle")

  
ggsave(filename="D:/1_Work/2_Ciron/voxelisation/Results/fl/test/16a.png", x, device="png" )
  
  





out1 <- lapply(out, function(x)
  {
  x <- x %>% 
    select(1:4) %>% 
    mutate(h = k+as.numeric(sub(".*\\s","",txt)),
           d = as.vector(dt_mat[cbind(nrow(dt_mat)-j, i+1)])) %>% 
    drop_na() %>% 
    group_by(i,j) %>% 
    filter(h>=d) %>% 
    mutate(k1 = row_number()) %>% 
    ungroup() %>% 
    group_by(k1) %>% 
    summarise(m=mean(PadBVTotal, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(test=if_else(lag(m)==0 & m==0,"x", "y")) %>% 
    filter(test!="x")
})









out <- voxnorm(vox)









nrow(dt_mat)

str(vxfile)

vxfile <- as.data.frame(vxfile, terrain)






vxfile <- vxfile %>% 
  select(1:4) %>% 
  mutate(h = k+as.numeric(sub(".*\\s","",txt)),
         d = as.vector(dt_mat[cbind(nrow(dt_mat)-j, i+1)])) %>% 
  drop_na() %>% 
  group_by(i,j) %>% 
  filter(h>=d) %>% 
  mutate(k1 = row_number()) %>% 
  ungroup() %>% 
  group_by(k1) %>% 
  summarise(m=mean(PadBVTotal, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(test=if_else(lag(m)==0 & m==0,"x", "y")) %>% 
  filter(test!="x")




all_dfs <- data.frame()
for(plt in plts)
{
 
  ptrn <- paste0("^", plt, ".*.vox")
  temp = list.files(path="D:/1_Work/2_Ciron/voxelisation/Results/fl/", 
                    pattern=ptrn)
  vxfiles <- list()
  for(subfile in temp)
  {
    nm <- paste0("D:/1_Work/2_Ciron/voxelisation/Results/fl/",subfile)
    vxfiles[[subfile]] <- as.data.frame(read.csv(nm, na.strings = NaN, skip = 5, sep = ""))
  }
  dfs <- data.frame()
    for(name in names(vxfiles))
    {
      mn <- tools::file_path_sans_ext(name)
      if(!is.na(as.numeric(tools::file_path_sans_ext(mn))))
      {
        #print(mn)
        pl <- toString(mn)
        mn <- "0.0"
      }
      else
      {
        #print(mn)
        pl <- sub("\\_.*","",mn)
        mn <- sub(".*\\_","",mn)
      }
      
      fl <- vxfiles[[name]]
      vxfile_smry <- fl %>% 
        select(c(1,2,3,4)) %>%
        group_by(i,j) %>%
        mutate(k1=k-(min(which(!is.na(PadBVTotal)))-1))%>%
        drop_na() %>% 
        group_by(k1) %>% 
        summarise(m=mean(PadBVTotal, na.rm = TRUE)) %>% 
        ungroup() 
      
      mnang <- rep(mn, dim(vxfile_smry)[1])
      id_plac <- rep(pl, dim(vxfile_smry)[1])
      vxfile_smry <- cbind(vxfile_smry, mnang, id_plac)
      dfs <- as.data.frame(rbind(dfs, vxfile_smry))
    }
  all_dfs <- as.data.frame(rbind(all_dfs, dfs))
}


vxfile_smry <- fl %>% 
  select(c(1,2,3,4)) %>%
  group_by(i,j) %>% 
  summarise(x=mean(PadBVTotal))



all_dfs$mnang <- as.numeric(all_dfs$mnang)
all_dfs$mnang <- as.factor(all_dfs$mnang)
all_dfs <- all_dfs %>% 
  mutate(Type=if_else(mnang=="0","All flight lines","Single flight line"))


cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
plots <- list()
for(plt in unique(all_dfs$id_plac))
{
  ttl <- paste0("PAD profiles for plot ", plt)
  plots[[plt]] <- ggplot(data = all_dfs[which(all_dfs$id_plac==plt & all_dfs$k1>0),], 
                         aes(x=k1, y=m, colour = mnang, linetype=Type))+
    geom_line(lwd=0.6)+
    coord_flip()+ 
    theme_minimal()+
    labs(title=ttl,
         y ="PAD", 
         x = "Height above ground (m)", 
         colour="Mean scan angle")+
    scale_colour_manual(values=cbPalette)
}

for(name in names(allplots))
{
  nm <- paste0("D:/1_Work/2_Ciron/voxelisation/Results/fl/w_wt/",
               name, "w_wt", ".png")
  ggsave(filename=nm, allplots[[name]],
         width = 17,
         height = 15,
         units = "cm",
         dpi = "retina",
         device="png" )
}



setwd("D:/1_Work/2_Ciron/voxelisation/Results/fl/w_wt/")



strs <- c("sdfn;/.", "karj ud rj", "...//./...")

sub("\\.", "x", strs)



