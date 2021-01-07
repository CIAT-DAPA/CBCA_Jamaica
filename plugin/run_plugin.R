##Test # 2 plugin
library(tidyverse)


#Function to distribuite files in different plugin folders
dist_files <- function(from, to, patt) {
  
  file.rename(list.files(from, full.names =  T, pattern = patt),
              paste0(to, "/", list.files(from, pattern = patt )))
}

#test Tiempo

files <- list.files("LIST/New folder/")

files %>% map(function(x){
  dist_files("LIST/New folder/", "LIST/", x)
  system("ACsaV60.exe", timeout = 215)
  dist_files("LIST/", "LIST/New folder/", x)
})

