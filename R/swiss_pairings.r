# Swiss pairings

# If number of players is odd, assign a bye to:
# a) fewest byes (normally zero)
# b) played against all other players, or so many that bye is required to avoid
#    a re-match elsewhere
# c) most losses
# d) played against fewest remaining players (to prevent re-matches and keep
#    people in longer) [shouldn't this be most remaining players?]
# e) random pick

# Remaining players paired with
# a) opponent paired against fewest times (usually zero)
# b) same number of losses (not uncounted unfinished matches for player not on
#    final elimination)
# c) random pick
# [I don't think this is right, e.g. would be paired with closer number of
# losses before being random pick, priority given to lower-losses players at b)]

# Player to go first is
# a) gone first fewer times in tournament (not counting byes)
# b) if re-match, fewer times gone first against this opponent
# c) random pick

# Uncompleted match handling
# If an uncompleted match is between two players at elimination, use a "winner
# of X" proxy for pairing in the next round, Considered to have combined
# opponent/bye history of both players.

# Let's assume the "happy path" first, i.e. no uncompleted matches in previous rounds
# Early drops are handled by removing them from input for generate_matches
# We need
# - a list of players
# - a data.frame of previous match participants and results
# - "X" elimination (can be Inf, is 3 for forum survival Swiss)

#' Collect player statistics
#'
#' @param matches a data.table, containing match results from previous rounds.
#'   Present columns are round (integer), P1 (character), P2 (character), victor
#'   (integer, 1 or 2).
#' @param players a character vector, containing names of players to collect
#'   statistics for.
#' @param elimination an integer, indicating the number of losses after which a
#'   player is eliminated. Eliminated players are not considered when deciding
#'   on byes or pairings.
#'
#' @return a list of two elements: \code{player_stats}, giving win/loss/bye
#'   records for each player; \code{pairing_counts}, giving counts of each
#'   player matchup, with player one on the row and player two on the column.
#' @export
player_stats <- function(
  matches,
  players,
  elimination
) {
  stopifnot(check_matches(matches))
  data.table(player = players) %>%
    add_base_stats(matches) %>%
    add_eliminated(elimination) %>%
    correct_byes(matches, elimination) %>%
    add_pairing_history(matches)
}

simplify <- function(
  new_match_info
) {
  list(
    stats = new_match_info$stats,
    pairs = new_match_info$pairs,
    bye = new_match_info$bye,
    decision_log = c(
      "# bye decision",
      new_match_info$bye_decision,
      "",
      "# pair decision",
      new_match_info$pair_decision,
      "",
      "# turn order decision",
      new_match_info$turn_decisions
    )
  )
}

#' Generate new round of matches
#'
#' @param stats player statistics, as generated by \code{\link{player_stats}}.
#' @param players a character vector, giving players to consider in generating
#'   new rounds. Players that are present in \code{stats}, but absent in
#'   \code{players}, are considered to have dropped out of the tournament early.
#'   Any players in \code{players} that are eliminated are automatically removed
#'   from consideration.
#' @param seed a random seed, if desired.
#'
#' @return
#' @export
generate_matches <- function(
  stats,
  players,
  seed = NULL
) {
  if (!is.null(seed))
    set.seed(seed)
  used_players <- intersect(
    players,
    stats$player_stats[eliminated == FALSE, player]
  )
  setindex(stats$player_stats, NULL)
  copy(stats) %>%
    add_bye_decision(used_players) %>%
    pair_remaining_players(used_players) %>%
    set_turn_orders() %>%
    simplify() %>%
    `class<-`(c("swiss", "list"))
}
