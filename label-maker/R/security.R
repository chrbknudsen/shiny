admin_token_status <- function(
    token = Sys.getenv("LABEL_ADMIN_TOKEN", unset = ""),
    minimum_bytes = 32L) {
  valid_scalar <- is.character(token) &&
    length(token) == 1L &&
    !is.na(token)

  if (!valid_scalar || !nzchar(token)) {
    return(
      list(
        configured = FALSE,
        reason = "missing"
      )
    )
  }

  if (nchar(enc2utf8(token), type = "bytes") < minimum_bytes) {
    return(
      list(
        configured = FALSE,
        reason = "too_short"
      )
    )
  }

  list(
    configured = TRUE,
    reason = "configured"
  )
}

secure_token_equal <- function(provided, expected) {
  valid_scalar <- function(value) {
    is.character(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      nzchar(value)
  }

  if (!valid_scalar(provided) || !valid_scalar(expected)) {
    return(FALSE)
  }

  provided_raw <- charToRaw(enc2utf8(provided))
  expected_raw <- charToRaw(enc2utf8(expected))
  comparison_length <- max(length(provided_raw), length(expected_raw))

  pad_raw <- function(value, size) {
    c(value, raw(size - length(value)))
  }

  differences <- bitwXor(
    as.integer(pad_raw(provided_raw, comparison_length)),
    as.integer(pad_raw(expected_raw, comparison_length))
  )
  length_difference <- bitwXor(
    length(provided_raw),
    length(expected_raw)
  )

  identical(
    Reduce(bitwOr, c(differences, length_difference), init = 0L),
    0L
  )
}

verify_admin_token <- function(
    provided,
    expected = Sys.getenv("LABEL_ADMIN_TOKEN", unset = "")) {
  if (!isTRUE(admin_token_status(expected)$configured)) {
    return(FALSE)
  }

  secure_token_equal(provided, expected)
}
