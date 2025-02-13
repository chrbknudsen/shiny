# funktioner til brug for tvangshygge appen

letter_pool <- c(LETTERS, letters, "æ", "ø", "å", "Æ", "Ø", "Å")


update_ord <- function(){
  if(length(which(ord != target_ord)) == 0){
    return(ord)
  }else{
    ord[get_index()] <<- get_letter()
    return(ord)}
  
}


# Identificer hvilket bogstav der skal ændres
get_index <- function(){
  sample(which(ord != target_ord), 1)
}

get_letter <- function(){
  sample(letter_pool, 1)
}


set_target_ord <- function(ord = "TVANGSHYGGE"){
  target_ord <<- unlist(strsplit(ord, ""))
}

gen_start_ord <- function(ord = "TVANGSHYGGE"){
  ord <<- sample(letter_pool, length(unlist(strsplit(ord, ""))), replace = TRUE)
}

saml_ord <- function(){
  paste(ord, collapse = "")
}



