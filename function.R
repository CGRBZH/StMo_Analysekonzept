function_richtung <- function(path, number) {
  
  firstup <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  } 
  
  sheets <- path %>% 
    excel_sheets() %>% 
    purrr::set_names() %>%
    purrr::map(read_excel, path = path)
  
  richtung <-sheets[[number]]
  colnames(richtung) = richtung[7, ] # the 8th row will be the header
  richtung <- richtung[-7, ] # removing the first row.
  
  # subset additional information and save to a data frame
  subset_zusatzinfo <- richtung[2:5, 1:2] %>% 
    pivot_wider(values_from = Zeit, names_from = Datum) %>% 
    rename(Messstelle = 1, Richtung = 2) %>%
    select(Messstelle, Richtung)
  
  # adding direction and station id to data frame
  richtung <- richtung[-c(1, 2, 3, 4, 5, 6, 7),]
  richtung <- richtung %>%
    mutate('Messstelle' = subset_zusatzinfo$Messstelle,
           'Richtung' = subset_zusatzinfo$Richtung,
           'date' = as.POSIXct(paste(.data$Datum, "%d.%m.%Y"), format="%d.%m.%Y"))
}

function_rbind <- function(dat1, dat2){
  dat <- rbind(dat1, dat2) %>%
    select(Datum, Zeit, Total, Richtung, Messstelle)
}