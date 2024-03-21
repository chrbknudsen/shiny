segment7 <- function(value = 0, 
                     color = "#00FF00",
                     stroke = "#004D00",
                     background = "#000000",
                     height = 320){
  all_segments <- c("a", "b", "c", "d", "e", "f", "g")
  
  width <- height/(1+2/3)
  
  segments <- list(
                 "0" = c("a", "b", "c", "d", "e", "f"),
                 "1" = c("b", "c"),
                 "2" = c("a", "b", "d", "e", "g"),
                 "3" = c("a", "b", "c", "d", "g"),
                 "4" = c("b", "c", "f", "g"),
                 "5" = c("a", "c", "d", "f", "g"),
                 "6" = c("a", "c", "d", "e", "f", "g"),
                 "7" = c("a", "b", "c"),
                 "8" = c("a", "b", "c", "d", "e", "f", "g"),
                 "9" = c("a", "b", "c", "d", "f", "g")
                 )

  if(!(as.character(value) %in% names(segments))){
    return(NA)
  }


  segments_on <- list(
    a = paste('<polygon id="a" fill="', color, '" stroke = "', stroke,'" points="2, 2  3, 1  9, 1  10, 2  9, 3  3, 3"/>', sep  = ""),
    b = paste('<polygon id="b" fill="', color, '" stroke = "', stroke,'" points="10, 2 11, 3 11, 9  10, 10  9, 9  9, 3"/>', sep  = ""),
    c = paste('<polygon id="c" fill="', color, '" stroke = "', stroke,'" points="10, 10 11,11 11,17  10,18  9,17  9,11"/>', sep  = ""),
    d = paste('<polygon id="d" fill="', color, '" stroke = "', stroke,'" points="10,18  9,19  3,19  2,18  3,17  9,17"/>', sep = ""),
    e = paste('<polygon id="e" fill="', color, '" stroke = "', stroke,'" points="2,18  1,17  1,11  2, 10  3,11  3,17"/>', sep = ""),
    f = paste('<polygon id="f" fill="', color, '" stroke = "', stroke,'" points="2, 10  1, 9  1, 3  2, 2  3, 3  3, 9"/>', sep = ""),
    g = paste('<polygon id="g" fill="', color, '" stroke = "', stroke,'" points="2, 10  3, 9  9, 9  10, 10  9,11  3,11"/>', sep = "")
  )
  
    
  segments_off <- list(
    a = paste('<polygon id="a" fill="', background, '" stroke = "', stroke,'" points="2, 2  3, 1  9, 1  10, 2  9, 3  3, 3"/>', sep = ""),
    b = paste('<polygon id="b" fill="', background, '" stroke = "', stroke,'" points="10, 2 11, 3 11, 9  10, 10  9, 9  9, 3"/>', sep  = ""),
    c = paste('<polygon id="c" fill="', background, '" stroke = "', stroke,'" points="10, 10 11,11 11,17  10,18  9,17  9,11"/>', sep = ""),
    d = paste('<polygon id="d" fill="', background, '" stroke = "', stroke,'" points="10,18  9,19  3,19  2,18  3,17  9,17"/>', sep = ""),
    e = paste('<polygon id="e" fill="', background, '" stroke = "', stroke,'" points="2,18  1,17  1,11  2, 10  3,11  3,17"/>', sep = ""),
    f = paste('<polygon id="f" fill="', background, '" stroke = "', stroke,'" points="2, 10  1, 9  1, 3  2, 2  3, 3  3, 9"/>', sep = ""),
    g = paste('<polygon id="g" fill="', background, '" stroke = "', stroke,'" points="2, 10  3, 9  9, 9  10, 10  9,11  3,11"/>', sep = "")
  )
  
  active_segments <- segments[[as.character(value)]]
  passive_segments <- all_segments[!all_segments %in% active_segments]
  
  active_segments <- segments_on[active_segments] 
  active_segments <- unlist(active_segments)
  active_segments <- paste0(active_segments, collapse = "\n")
  
  passive_segments <- segments_off[passive_segments]
  passive_segments <- unlist(passive_segments)
  passive_segments <- paste0(passive_segments, collapse = "\n")
  beginning <- paste0('<?xml version="1.0"?><svg xmlns="http://www.w3.org/2000/svg"  
  width="', width, '" height="', height,' " viewBox="-1 -1 13 21" stroke="',background,'"  stroke-width=".25">
                      <rect width = "100%" height = "100%" fill = "',background,'"/>', collapse = "")  
  end <- '</svg>'
  paste0(beginning, active_segments, passive_segments, end, collapse = "\n")
}

