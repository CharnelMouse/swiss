check_matches <- function(
  matches
) {
  matches[
    ,
    .(unique_players = !anyDuplicated(c(P1, P2))),
    by = round
  ][
    ,
    all(unique_players)
  ]
}

add_base_stats <- function(
  dt,
  matches
) {
  suppressWarnings(
    copy(matches)[
      , c("tournament", "player1", "player2") := .("dummy", P1, P2)
    ][
      , victor := ifelse(victor == 1, P1, P2)
    ] %>%
      codexdata:::get_player_win_stats()
  )[
    , tournament := NULL
  ][
    dt, on = "player"
  ][
    ,
    c("last_round", "played", "won", "lost", "byes") :=
      nafill(lapply(.SD, as.integer), fill = 0L),
    .SDcols = c("last_round", "played", "won", "lost", "byes")
  ]
}

add_eliminated <- function(
  dt,
  elimination
) {
  dt[, stopifnot(all(lost <= elimination))]
  dt[, eliminated := lost == elimination][]
}

correct_byes <- function(
  dt,
  matches,
  elimination
) {
  dt[
    eliminated == FALSE,
    byes := max(0L, matches[, suppressWarnings(max(round))]) - played,
  ][] %>%
    setindexv(NULL)
}

pairing_history <- function(
  matches,
  players
) {
  n_players <- length(players)
  pairings <- matrix(
    0L,
    nrow = n_players,
    ncol = n_players,
    dimnames = list(players, players)
  )
  if (nrow(matches) > 0) {
    for (n in 1:nrow(matches)) {
      pairings[matches[n, P1], matches[n, P2]] <-
        pairings[matches[n, P1], matches[n, P2]] + 1L
    }
  }
  pairings
}

add_pairing_history <- function(
  stats,
  matches
) {
  list(
    player_stats = stats,
    pairing_counts = pairing_history(matches, stats$player)
  )
}
