getdata.2003 <- function(crimes_links, year) {
  library(httr)
  library(stringr)
  tmp <- tempfile(fileext = '.xls')
  writeBin(GET(crimes_links[crimes_links['Year'] == year, 'Link'])$content, tmp)
  crimes <- read_excel(tmp)
  rows_to_remove <- c(1,2,3)
  for(i in nrow(crimes):1) {
    if(is.na(crimes[i, 1]) | crimes[i, 1] != 'Rate per 100,000 inhabitants') rows_to_remove <- append(rows_to_remove, i)
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
    if(new_state == '') {
      new_state <- gsub("[0-9]*", "", crimes[i, 1])
      i = i+1
    }
    else {
      area <- gsub("[0-9]*", "", crimes[i, 1])
      where <- 0
      if(grepl('Total', area, fixed=TRUE, ignore.case=FALSE)) where <- i
      else if(str_trim(crimes[i+2, 1]) == 'Estimated total') where <- i+2
      else if(str_trim(crimes[i+1, 1]) == 'Area actually reporting') where <- i+1
      else where <- i
      if(!is.na(crimes[i, 2]) & crimes[i, 2] != 'None') {
        row <- data.frame(Year=c(year), State=c(new_state), Area=c(area), Population=c(crimes[i, 2]),
                          Violent=c(crimes[where, 3]), Property=c(crimes[where, 8]), Murder=c(crimes[where, 4]),
                          Rape=c(crimes[where, 5]), Robbery=c(crimes[where, 6]), Assault=c(crimes[where, 7]),
                          Burglary=c(crimes[where, 9]), Theft=c(crimes[where, 10]), Motor=c(crimes[where, 11]))
        crimes_condensed <- rbind(crimes_condensed, row)
      }
      if(grepl('Total', area, fixed=TRUE, ignore.case=FALSE)) {
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