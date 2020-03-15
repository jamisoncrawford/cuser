# CUSER PACKAGE

    # Date: 2020-03-15
    # R Version: 3.6.3
    # RStudio Version: 1.2.5033
    # OS: Windows 10



# SET WORKING DIRECTORY, LOAD PACKAGES, READ IN DATA

setwd("~/CNYCF/Functions & Packages")

library(readr)
library(dplyr)
library(readxl)

file <- "C:/Users/jamis/Downloads/Ridzi_3_3_20.xlsx"

url <- paste0("https://raw.githubusercontent.com/ja",
              "misoncrawford/Syracuse-Crime-Analysi",
              "s/master/Data/fips_geoid.csv")

fips <- read_csv(file = url, 
                 col_types = "c"); rm(url)

saveRDS(object = fips, 
        file = "syracuse_fips.RDATA")

dss <- read_excel(file); rm(file)



# FILTER DSS TA DATA FUNCTION: filter_ta()

filter_ta <- function(data, tracts = 1, cases = 2, individuals = 3){
  
  error_df <- paste0("Argument 'data =' is not of class ",
                     "'data.frame'; please cast as 'data",
                     ".frame' with function 'as.data.frame()'.")
  
  if (!is.data.frame(data)){stop(error_df, call. = F)}
  
  fips <- readRDS(file = "syracuse_fips.RDATA")
  
  d <- data
  f <- fips
  t <- as.character(d[[1]])
  g <- as.character(d[[1]])
  c <- as.numeric(d[[2]])
  i <- as.numeric(d[[3]])
  
  g <- ifelse(grepl("\\.", g), g, paste0(g, ".00"))
  
  g <- gsub("\\.", "", g)
  
  g <- ifelse(nchar(g) == 3, 
              paste0("00", g),
              ifelse(nchar(g) == 4,
                     paste0("0", g), g))
  
  g <- paste0("360670", g)
  
  df <- data.frame(geoid = g, 
                   tract = t,
                   cases = c, 
                   individuals = i, 
                   stringsAsFactors = F)
  
  
  df <- merge(x = f, y = df, by = "geoid", all.x = TRUE)
  
  df <- df[order(df$geoid), ]

  return(df)

}



# SAVE FUNCTION: filter_ta()

saveRDS(object = filter_ta, 
        "filter_ta.RDATA")

rm(fips)



# EXPAND CENSUS TRACT DATA PREP

file <- "C:/Users/jamis/Downloads/CT Conversion Sheet.csv"

trct <- read_csv(file, col_types = "ccccc")

saveRDS(trct, "tracts.RDATA")







# EXPAND CENSUS TRACT FUNCTION

expand_ct <- function(tracts, order = TRUE, duplicate.rm = FALSE, na.rm = TRUE){
  
  error_vct <- paste0("Argument 'tracts =' is not of ",
                      "class 'vector'; please cast as",
                      " vector with function 'as.vect",
                      "or()' or extract the variable ",
                      "as a vector using 'data$variab",
                      "le' syntax.")
  
  warn_leng <- paste0("One or more values passed to 'tracts =' were either duplicates")
  
  if(!is.vector(tracts)){stop(error_vct, call. = F)}
  
  trc <- tracts
  ord <- order
  dup <- duplicate.rm
  nar <- na.rm
  
  all <- readRDS("tracts.RDATA")
  
  full <- sum(all$full_ct %in% trc, T)
  trim <- sum(all$trim_ct %in% trc, T)
  decm <- sum(all$dec_ct %in% trc, T)
  intg <- sum(all$int_ct %in% trc, T)
  labl <- sum(all$lab_ct %in% trc, T)
  
  index <- which(c(full,
                   trim, 
                   decm,
                   intg,
                   labl) == max(full,
                                trim,
                                decm,
                                intg,
                                labl))
  
  col <- names(all[, index])
  
  trc <- data.frame(trc)
  
  names(trc) <- col
  
  trc$order <- 1:nrow(trc)
  
  df <- merge(x = trc, y = all, by = col, all.x = TRUE)
  
  df <- df[sort.list(df$order), -which(names(df) == "order")]
  
  index_ord <- order(df$full_ct)
  
  if (ord == TRUE){df <- df[index_ord, ]}
  
  index_dup <- which(!duplicated(df))
  
  if (dup == TRUE){df <- df[index_dup, ]}
  
  index_nar <- which(complete.cases(df))
  
  if (nar == TRUE){df <- df[index_nar, ]}
  
  names(df)[which(names(df) == "int_ct")] <- "abbreviated"
  names(df)[which(names(df) == "dec_ct")] <- "decimal_zeroes"
  names(df)[which(names(df) == "lab_ct")] <- "labels"
  names(df)[which(names(df) == "full_ct")] <- "fips_geoid"
  names(df)[which(names(df) == "trim_ct")] <- "decimal_trimmed"
  
  return(df)
  
}



