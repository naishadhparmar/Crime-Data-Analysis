getdata.2005 <- function(crimes_links, year) {
  library(httr)
  tmp <- tempfile(fileext = '.xls')
  writeBin(GET(crimes_links[crimes_links['Year'] == year, 'Link'])$content, tmp)
  crimes <- read_excel(tmp)
  rows_to_remove <- c(1,2)
  for(i in nrow(crimes):1) {
    if(is.na(crimes[i, 3]) | crimes[i, 3] != 'Rate per 100,000 inhabitants') rows_to_remove <- append(rows_to_remove, i)
    else break
  }
  crimes <- crimes[-rows_to_remove, ]
  cols_to_remove <- c()
  for(i in 1:ncol(crimes)) {
    if(is.na(crimes[1, i])) cols_to_remove <- append(cols_to_remove, i)
  }
  crimes <- crimes[-cols_to_remove]
  colnames(crimes) <- crimes[1, ]
  crimes <- crimes[-1, ]
  crimes_condensed <- data.frame(Year=c(), State=c(), Area=c(), Population=c(),
                                      Violent=c(), Property=c(), Murder=c(),
                                      Rape=c(), Robbery=c(), Assault=c(),
                                      Burglary=c(), Theft=c(), Motor=c(), Arson=c())
  new_state <- ''
  i <- 1
  while(i <= nrow(crimes)) {
    if(new_state == '') new_state <- gsub("[0-9]*", "", crimes[i, 1])
    else {
      area <- gsub("[0-9]*", "", crimes[i, 2])
      where <- 0
      if(grepl('Total', area, fixed=TRUE)) where <- i
      else if(!is.na(crimes[i+2, 3]) & crimes[i+2, 3] == 1) where <- i+2
      else if(!is.na(crimes[i+1, 3]) & crimes[i+1, 3] == 1) where <- i+1
      else where <- i
      if(!is.na(crimes[i, 3]) & crimes[i, 3] != 'None') {
        row <- data.frame(Year=c(year), State=c(new_state), Area=c(area), Population=c(crimes[i, 3]),
                          Violent=c(crimes[where, 4]), Property=c(crimes[where, 9]), Murder=c(crimes[where, 5]),
                          Rape=c(crimes[where, 6]), Robbery=c(crimes[where, 7]), Assault=c(crimes[where, 8]),
                          Burglary=c(crimes[where, 10]), Theft=c(crimes[where, 11]), Motor=c(crimes[where, 12]))
        crimes_condensed <- rbind(crimes_condensed, row)
      }
      if(grepl('Total', area, fixed=TRUE)) {
        i = where+2
        new_state = ''
      }
      else i = where+1
    }
  }
  colnames(crimes_condensed) <- c('Year', 'State', 'Area', 'Population', 'Violent', 'Property',
                                  'Murder', 'Rape', 'Robbery', 'Assault', 'Burglary', 'Theft', 'Motor')
  return(crimes_condensed)
}