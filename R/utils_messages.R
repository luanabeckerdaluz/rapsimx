CHARS_TO_BREAK <<- 100

custom_warning <- function(text) {
  cat(
    paste0(
      paste(
        strwrap(
          paste("🟡 WARNING:", sub("\n$", "", text)),
          width = CHARS_TO_BREAK
        ),
        collapse = "\n"
      ),
      "\n"
    )
  )
}

custom_stop <- function(text) {
  CHARS_TO_BREAK <<- 80
  cat(paste(strwrap(paste("🛑 STOP:", text), width = CHARS_TO_BREAK), collapse = "\n"))
  stop()
}

custom_cat <- function(text) {
  cat(paste0(paste(strwrap(paste0("🟢 ", text), width = CHARS_TO_BREAK), collapse = "\n")), "\n")
}

custom_cat_nobreaks <- function(text) {
  cat(paste0("🟢 ", text, "\n"))
}

custom_summary <- function(text) {
  cat(paste0("📌 ", text, "\n"))
}