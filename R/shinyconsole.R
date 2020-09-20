capture_console_output <- function(x) {
  is_0_length <- length(x) == 0
  is_null <- is.null(x)

  if (is_null) {
    "NULL"
  } else if (is_0_length) {
    utils::capture.output(x)
  } else {
    x
  }
}

console_header <- function() {
  cat("[ shiny log @", as.character(Sys.time()), "]\n")
}

console_inputs <- function(session) {
  lapply(names(session$input), function(x) {
    out <- capture_console_output(session$input[[x]])
    cat(paste0("* input$", x), ":", out, "\n")
  })
}

console_reactive_values <- function(rv) {
  lapply(names(rv), function(x) {
    out <- capture_console_output(rv[[x]])
    cat(paste0("* rv$", x), ":", out, "\n")
  })
}

#' Console messages from shiny
#'
#' Constantly observes the shiny session and reactive values (is provided), then
#' prints them to the console.
#'
#' @param session A shiny session
#' @param rv A reactiveValues object
#' @export
shinyconsole <- function(session, rv = NULL) {
  shiny::observe({
    console_header()
    console_inputs(session)
    if (!is.null(rv)) console_reactive_values(rv)
    cat("\n")
  })
}
