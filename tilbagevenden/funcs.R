# funktioner til brug for tvangshygge appen

letter_pool <- c(LETTERS, letters, "æ", "ø", "å", "Æ", "Ø", "Å")

update_ord <- function(state) {
  # Læs reactive værdier med isolate()
  if (all(isolate(state$ord) == isolate(state$target_ord))) {
    return(isolate(state$ord))
  } else {
    index <- get_index(state)
    state$ord[index] <- get_letter()
    return(isolate(state$ord))
  }
}

get_index <- function(state) {
  available <- which(isolate(state$ord) != isolate(state$target_ord))
  sample(available, 1)
}

get_letter <- function() {
  sample(letter_pool, 1)
}

set_target_ord <- function(state, word = "TVANGSHYGGE") {
  # Skrivning til reactiveValues behøver ikke isolate()
  state$target_ord <- unlist(strsplit(word, ""))
}

gen_start_ord <- function(state, word = "TVANGSHYGGE") {
  state$ord <- sample(letter_pool, nchar(word), replace = TRUE)
}

saml_ord <- function(state) {
  paste(isolate(state$ord), collapse = "")
}
