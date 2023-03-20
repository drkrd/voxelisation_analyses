# Load the package required to read XML files.
library(XML)
library(xml2)
library(lidR)
library(data.table)
library(tools)

# Also load the other required package.
library("methods")



# pth_input <- paste0("D:/1_Work/5_Bauges/Data/ULM/LAS/unnorm/plots/15m_rad/december2021/allpoints/")
xml_template <- read_xml("D:/1_Work/2_Ciron/CironOct2022/voxelisation/winter/2m_offset.xml")

# lasfiles <- list.files(paste0("D:/1_Work/5_Bauges/Data/ULM/LAS/unnorm/plots/15m_rad/december2021/allpoints/"))
plot.info <- fread("D:/1_Work/5_Bauges/plot_info.csv")

allpcnames <- as.data.table(list.files(paste0("D:/1_Work/2_Ciron/CironOct2022/subsets/Winter/unnorm/"), 
                                        full.names = TRUE, pattern = "*.las"))

#allpcnames[, grp := sub("_[^_]*$","",basename(file_path_sans_ext(V1)))]
allpcnames[, grp := basename(file_path_sans_ext(V1))]
#allpcnames <- allpcnames[grp%in%plot.info[dep==74]$id_placette]


output.pth <- "D:/1_Work/2_Ciron/CironOct2022/voxelisation/winter/unnorm/voxels/"
xml.pth <- "D:/1_Work/2_Ciron/CironOct2022/voxelisation/winter/unnorm/config/"



for(row in 1:nrow(allpcnames))
{
  xml_file <- xml_template
  
  placette <- tools::file_path_sans_ext(basename(allpcnames$V1[row]))
  input.pc <- allpcnames$V1[row]
  input.dtm <- paste0("D:/1_Work/2_Ciron/CironOct2022/subsets/5m_buffer_dtms_asc/", placette, ".asc")
  
  
  ls <- readLASheader(paste0("D:/1_Work/2_Ciron/CironOct2022/subsets/Summer/unnorm/",
                             allpcnames$grp[row],
                             ".las"))
  
  
  
  
  xml_set_attr(xml_child(xml_child(xml_child(xml_file, 1), 1), 1), "src", input.pc)
  xml_set_attr(xml_child(xml_child(xml_child(xml_child(xml_file, 1), 1), 2), 1), "src", input.pc)
  xml_set_attr(xml_child(xml_child(xml_child(xml_file, 1), 2), 1), "src", output.pth)
  
  min <- paste0("(",ls@PHB[["Min X"]], ", ", ls@PHB[["Min Y"]], ", ", ls@PHB[["Min Z"]],")")
  xml_set_attr(xml_child(xml_child(xml_file, 1), 3), "min", min)
  
  max <- paste0("(",ls@PHB[["Max X"]], ", ", ls@PHB[["Max Y"]], ", ", ls@PHB[["Max Z"]],")")
  xml_set_attr(xml_child(xml_child(xml_file, 1), 3), "max", max)
  
  xcells <- ceiling(ls@PHB[["Max X"]])-floor(ls@PHB[["Min X"]])
  ycells <- ceiling(ls@PHB[["Max Y"]])-floor(ls@PHB[["Min Y"]])
  zcells <- ceiling(ls@PHB[["Max Z"]])-floor(ls@PHB[["Min Z"]])
  split <- paste0("(", xcells, ", ", ycells, ", ", zcells, ")")
  xml_set_attr(xml_child(xml_child(xml_file, 1), 3),"split", split)
  
  
  xml_set_attr(xml_child(xml_child(xml_child(xml_file, 1), 8), 1), "src", input.dtm)
  
  out <- paste0(xml.pth, placette, ".xml")
  
  write_xml(xml_file, out)
  
}








xml_mod <- function(lasfile, inpth, outpthvox, outpthxml, tmpl)
{
  xml_file1 <- tmpl
  
  pth1 <- paste0(inpth,lasfile)
  
  ls <- readLASheader(pth1)
  
  pth2 <- paste0(outpthvox, tools::file_path_sans_ext(lasfile), ".vox")
  
  xml_set_attr(xml_child(xml_child(xml_child(xml_file1, 1), 1), 1), "src", pth1)
  
  xml_set_attr(xml_child(xml_child(xml_child(xml_file1, 1), 2), 1), "src", pth2)
  
  xml_set_attr(xml_child(xml_child(xml_file1, 1), 3), "xmin", ls@PHB[["Min X"]]) 
  xml_set_attr(xml_child(xml_child(xml_file1, 1), 3), "ymin", ls@PHB[["Min Y"]]) 
  xml_set_attr(xml_child(xml_child(xml_file1, 1), 3), "zmin", ls@PHB[["Min Z"]]) 
  xml_set_attr(xml_child(xml_child(xml_file1, 1), 3), "xmax", ls@PHB[["Max X"]]) 
  xml_set_attr(xml_child(xml_child(xml_file1, 1), 3), "ymax", ls@PHB[["Max Y"]]) 
  xml_set_attr(xml_child(xml_child(xml_file1, 1), 3), "zmax", ls@PHB[["Max Z"]]) 
  
  
  xml_set_attr(xml_child(xml_child(xml_file1, 1), 3), 
               "splitX", 
               ceiling(ls@PHB[["Max X"]])-floor(ls@PHB[["Min X"]])) 
  
  xml_set_attr(xml_child(xml_child(xml_file1, 1), 3), 
               "splitY", 
               ceiling(ls@PHB[["Max Y"]])-floor(ls@PHB[["Min Y"]])) 
  
  xml_set_attr(xml_child(xml_child(xml_file1, 1), 3), 
               "splitZ", 
               ceiling(ls@PHB[["Max Z"]])-floor(ls@PHB[["Min Z"]]))
  
  
  
  out <- paste0(outpthxml, tools::file_path_sans_ext(lasfile), ".xml")
  
  write_xml(xml_file1, out)
}

xml_set_attr(xml_child(xml_child(xml_child(xml_file1, 1), 2), 1), "src", pth2)







gpstimes <- list()
for(row in 1:nrow(allpcnames))
{
  ls <- readLAS(paste0("D:/1_Work/5_Bauges/Data/ULM/LAS/unnorm/plots/feuillus/15m_rad/flightlines_3/", 
                       allpcnames$grp[row],
                       ".las"), select = "xyzgpstime")
  gpstimes <- c(gpstimes, ls@data$gpstime)
  
}


