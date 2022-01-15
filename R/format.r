#' @export
print.swiss <- function(
  x
) {
  if (nrow(x$pairs) == 0) {
    cat("no matches")
  }else{
    cat(
      paste(
        c(
          "matches:",
          paste(x$pairs$P1, "vs", x$pairs$P2),
          "",
          paste("bye:", ifelse(is.na(x$bye), "none", x$bye)),
          "",
          "use decisions() to see decision log"
        ),
        collapse = "\n"
      )
    )
  }
}

#' Show decision log for match decisions
#'
#' @param x a swiss object, as returned by \code{generate_matches}.
#'
#' @return
#' @export
decisions <- function(
  x
) {
  cat("player stats:\n")
  print(x$stats$player_stats)
  cat("\npairing counts:\n")
  print(x$stats$pairing_counts)
  cat(
    "",
    x$decision_log,
    sep = "\n"
  )
}
