library(lidr)
library(dplyr)
library(tidyr)
library(ggplot2)

vxfile <- as.data.frame(read.csv("D:/1_Work/2_Ciron/voxelisation/1_6_2/10.vox", 
                                 na.strings = NaN, skip = 5, sep = ""))


# vxfile_smry <- vxfile %>% 
#   select(c(1,2,3,4)) %>%
#   group_by(k) %>%
#   drop_na() %>% 
#   summarise(m = mean(PadBVTotal, na.rm = TRUE)) %>% 
#   ungroup()
#   
# ggplot(data = vxfile_smry, aes(x=k1, y=m))+geom_line()+coord_flip()


vxfile_smry <- vxfile %>% 
  select(c(1,2,3,4)) %>%
  group_by(i,j) %>%
  mutate(k1=k-(min(which(!is.na(PadBVTotal)))-1))%>%
  drop_na() %>% 
  group_by(k1) %>% 
  summarise(m=mean(PadBVTotal, na.rm = TRUE)) %>% 
  ungroup()

ggplot(data = vxfile_smry, aes(x=k1, y=m))+geom_line()+coord_flip()  
