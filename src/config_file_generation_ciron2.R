# Load the package required to read XML files.
library(XML)
library(xml2)
library(lidR)
library(data.table)
library(tools)

# Also load the other required package.
library("methods")



pcloud.loc <- paste0("D:/1_Work/2_Ciron/Data/ULM/LAS/unnorm/plots/15m_rad_test/may2021/flightlines_3/")
xml_template <- read_xml("D:/1_Work/2_Ciron/voxelisation/Results/October/ciron_template_oct2021.xml")

lasfiles <- list.files(paste0("D:/1_Work/2_Ciron/Data/ULM/LAS/unnorm/plots/15m_rad_test/may2021/flightlines_3/"))


allpcnames <- as.data.table(list.files(paste0("D:/1_Work/2_Ciron/Data/ULM/LAS/unnorm/plots/15m_rad_test/may2021/flightlines_3/"), 
                                        full.names = TRUE, pattern = "*.las"))

#allpcnames[, grp := sub("_[^_]*$","",basename(file_path_sans_ext(V1)))]
allpcnames[, grp := basename(file_path_sans_ext(V1))]



for(row in 1:nrow(allpcnames))
{
  xml_file1 <- xml_template
  
  input.pcloud <- allpcnames$V1[row]
  

  
  
   ls <- readLASheader(paste0("D:/1_Work/2_Ciron/Data/ULM/LAS/unnorm/plots/15m_rad_test/may2021/flightlines_3/", 
                             allpcnames$grp[row],
                             ".las"))
  
  output.path.vox <- paste0("D:/1_Work/2_Ciron/Voxelisation/Results/October/wo_interpolation/voxels/flightlines_3/", 
                        file_path_sans_ext(basename(allpcnames$V1[row])),
                        ".vox")
  
  xml_set_attr(xml_child(xml_child(xml_child(xml_file1, 1), 1), 1), "src", input.pcloud)
  
  xml_set_attr(xml_child(xml_child(xml_child(xml_file1, 1), 2), 1), "src", output.path.vox)
  
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

  
  out.pth.xml <- paste0("D:/1_Work/2_Ciron/Voxelisation/Results/October/wo_interpolation/xmlfiles/flightlines_3/",
                tools::file_path_sans_ext(basename(allpcnames$V1[row])),
                ".xml")
  
  write_xml(xml_file1, out.pth.xml)
  
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


