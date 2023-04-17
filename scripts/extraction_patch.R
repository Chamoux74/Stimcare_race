library(tibble)
library(purrr)
library(dplyr)
library(readr)

racepatch <-
  list.files(
    path = "C:/Users/maxch/Git/RACE/data/Patch",
    pattern = "\\.csv",
    all.files = TRUE,
    full.names = TRUE
  )



listdfracepatch <- for (i in 1:length(racepatch)) {function(sep_char){
    if(sep_char == ";"){
      read.csv2(i)
    } else if(sep_char == ","){
      read.csv(i)
    } else {
      stop("Invalid separator character. Please use either ';' or ','.")
    }
  }
  return(racepatch[[i]])
}

listdfrace <- lapply(racepatch , import_csv_files)
names(listdfrace) <- tools::file_path_sans_ext(basename(racepatch))
