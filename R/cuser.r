
globalVariables("sysdata.rds")

#' Clean, Filter, & Order DSS Temporary Assistance Data
#'
#' \code{filter_ta()} processes tables of class \code{data.frame}
#' by filtering data exclusively to the 55 census tracts within Syracuse,
#' NY, ordering values according to Federal Information Processing Standard
#' Publication 6-4 (FIPS) GEOID, and preserving census tracts which exclude
#' Temporary Assistance (TA) data that are not reported by Onondaga County
#' Department of Social Services (DSS).
#'
#' @param data A tabular object of class \code{data.frame} containing
#' DSS TA data.
#'
#' @param tracts The column number of the \code{data.frame} object passed to
#' argument \code{data =} which contains the table's census tract values.
#' Defaults to \code{1}.
#'
#' @param cases The column number of the \code{data.frame} object passed to
#' argument \code{data =} which contains the table's TA cases. Defaults to
#' \code{2}.
#'
#' @param individuals The column number of the \code{data.frame} object passed
#' to argument \code{data =} which contains the table's TA-assisted
#' individuals. Defaults to \code{3}.
#'
#' @return A tabular object of class \code{data.frame} containing only DSS TA
#' data pertaining to the 55 census tracts in Syracuse, NY, including tracts in
#' which no TA cases are reported. Arguments \code{tracts =}, \code{cases =},
#' and \code{individuals} are renamed \code{tract}, \code{cases}, and
#' \code{individuals}, prefaced by the FIPS values in column \code{geoid}.
#'
#' @author Jamison R. Crawford, Social Sector Data Science Consultant
#'
#' @seealso \code{merge}, \code{order}
#'
#' @export



filter_ta <- function(data, tracts = 1, cases = 2, individuals = 3){

  error_df <- paste0("Argument 'data =' is not of class ",
                     "'data.frame'; please cast as 'data",
                     ".frame' with function 'as.data.frame()'.")

  if (!is.data.frame(data)){stop(error_df, call. = F)}

  tracts <- readRDS("sysdata.rds")

  names(tracts)[1] <- "geoid"

  d <- data
  f <- data.frame(geoid = tracts$geoid)
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



#' Detect & Validate Syracuse, NY Census Tract Format Variants
#'
#' \code{expand_ct()} accepts a series of census tract values of class
#' \code{vector}, detects their format, and returns an object of
#' class \code{data.frame} containing four other variants for speedy
#' reformatting. Sorting is optional, as is preservation of duplicates
#' and missing values.
#'
#' @param tracts A series of census tracts of class \code{vector}, either as
#' an independent object or an extraction from a \code{data.frame} object in
#' \code{data$variable} syntax.
#'
#' @param order Logical \code{TRUE} or \code{FALSE} indicating whether output
#' should be in numeric or alphanumeric order. Defaults to \code{TRUE}.
#'
#' @param duplicate.rm Logical \code{TRUE} or \code{FALSE} indicating whether
#' output should remove duplicate census tract values. Defaults to \code{FALSE}.
#'
#' @param na.rm Logical \code{TRUE} or \code{FALSE} indicating whether output
#' should remove incomplete reformatting due to missing (\code{NA}) values.
#' Defaults to \code{TRUE}.
#'
#' @return A tabular object of class \code{data.frame} containing five variants
#' of 55 Syracuse, NY census tracts. Variants may be previewed in package
#' \code{cuser} dataset \code{tracts}. Output may be ordered with duplicates
#' and missing values removed depending on inputs to logical arguments
#' \code{order =}, \code{duplicate.rm =}, and \code{na.rm = }, respectively.
#'
#' @author Jamison R. Crawford, Social Sector Data Science Consultant
#'
#' @seealso \code{tracts}, \code{merge}, \code{order}, \code{duplicated}, \code{complete.cases}
#'
#' @export



expand_ct <- function(tracts, order = TRUE, duplicate.rm = FALSE, na.rm = TRUE){

  error_vct <- paste0("Argument 'tracts =' is not of ",
                      "class 'vector'; please cast as",
                      " vector with function 'as.vect",
                      "or()' or extract the variable ",
                      "as a vector using 'data$variab",
                      "le' syntax.")

  warn_leng <- paste0("One or more values passed to 'tracts =' were either duplicates")

  if(!is.vector(tracts)){stop(error_vct, call. = F)}

  all <- readRDS("sysdata.rds")

  trc <- tracts
  ord <- order
  dup <- duplicate.rm
  nar <- na.rm

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

  col <- names(all)[index]

  trc <- data.frame(trc,
                    stringsAsFactors = FALSE)

  names(trc) <- col

  trc$order <- 1:nrow(trc)

  df <- merge(x = trc, y = all, by = col, all.x = TRUE)

  df <- df[sort.list(df$order), -which(names(df) == "order")]

  index_ord <- order(df[[col]])

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
