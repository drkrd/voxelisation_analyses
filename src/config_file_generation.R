# Load the package required to read XML files.
library(XML)
library(xml2)
library(lidR)

# Also load the other required package.
library("methods")

pth_input <- "D:\\1_Work\\2_Ciron\\Data\\ULM\\LAS\\unnorm\\plots\\"
xml_template <- read_xml("D:/1_Work/2_Ciron/voxelisation/xml_template.xml")

lasfiles <- list.files("D:/1_Work/2_Ciron/Data/ULM/LAS/unnorm/plots/17_5m_rad/")

for(lasfile in lasfiles)
{
  xml_file1 <- xml_template
  
  pth1 <- paste0("D:\\1_Work\\2_Ciron\\Data\\ULM\\LAS\\unnorm\\plots\\17_5m_rad\\",
                 lasfile)
  
  ls <- readLASheader(pth1)
  
  pth2 <- paste0("D:\\1_Work\\2_Ciron\\voxelisation\\Results\\", 
                 tools::file_path_sans_ext(lasfile),
                 ".vox")
  
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

  
  out <- paste0("D:\\1_Work\\2_Ciron\\voxelisation\\xml_files\\",
                tools::file_path_sans_ext(lasfile),
                ".xml")
  
  write_xml(xml_file1, out)
  
}



xml_set_attr(xml_child(xml_child(xml_child(xml_file1, 1), 2), 1), "src", pth2)
