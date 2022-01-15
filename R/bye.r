# If number of players is odd, assign a bye to:
# a) fewest byes (normally zero)
# b) played against all other players, or so many that bye is required to avoid
#    a re-match elsewhere
# c) most losses
# d) played against most remaining players (to prevent re-matches and keep
#    people in longer)
# e) random pick

no_bye_if_even_player_count <- function(
  stats,
  players
) {
  current_player_stats <- stats$player_stats[player %in% players & eliminated == FALSE]
  if (nrow(current_player_stats) %% 2 == 0)
    finish(
      list(
        stats = stats,
        bye = NA,
        bye_decision = "even number of players, no bye needed"
      )
    )
  else
    continue(
      list(
        stats = stats,
        current_player_stats = current_player_stats
      )
    )
}

fewest_byes <- function(
  lst
) {
  stats <- lst$stats
  current_player_stats <- lst$current_player_stats
  candidates <- current_player_stats[
    byes == min(byes),
    player
  ]
  if (length(candidates) == 1) {
    fewest_decision <- paste("one fewest-byes candidate:", candidates)
    finish(
      list(
        stats = stats,
        bye = NA,
        bye_decision = fewest_decision
      )
    )
  }else{
    fewest_decision <- paste("fewest-byes candidates:", toString(candidates))
    continue(
      list(
        stats = stats,
        current_player_stats = current_player_stats,
        bye = NA,
        bye_decision = fewest_decision,
        bye_candidates = candidates
      )
    )
  }
}

played_all_others <- function(
  lst
) {
  players <- lst$current_player_stats[, player]
  current_pairing_counts <- lst$stats$pairing_counts[players, players]
  mirror_pairs <- current_pairing_counts + t(current_pairing_counts)
  played_all <- which(rowSums(mirror_pairs > 0) == ncol(mirror_pairs) - 1L)
  candidate_played_all <- intersect(lst$bye_candidates, players[played_all])
  if (length(candidate_played_all) == 0) {
    new_decision <- c(
      lst$bye_decision,
      "no one's played all remaining players"
    )
    continue(`[[<-`(lst, "bye_decision", new_decision))
  }else{
    if (length(candidate_played_all) == 1) {
      new_decision <- c(
        lst$bye_decision,
        paste(
          "one candidate has played all others:",
          candidate_played_all
        )
      )
      finish(list(
        stats = lst$stats,
        bye = candidate_played_all,
        bye_decision = new_decision
      ))
    }else{
      new_decision <- c(
        lst$bye_decision,
        paste(
          "candidates that have played all others:",
          toString(candidate_played_all)
        )
      )
      continue(list(
        stats = lst$stats,
        current_player_stats = lst$current_player_stats,
        bye = NA,
        bye_decision = new_decision,
        bye_candidates = candidate_played_all
      ))
    }
  }
}

avoid_rematch <- function(
  lst
) {
  new_decision <- c(
    lst$bye_decision,
    "bye to avoid rematches not implemented"
  )
  continue(`[[<-`(lst, "bye_decision", new_decision))
}

most_losses <- function(
  lst
) {
  candidates <- lst$stats$player_stats[
    player %in% lst$bye_candidates
  ][
    lost == max(lost),
    player
  ]
  if (length(candidates) == 1) {
    finish(list(
      stats = lst$stats,
      bye = candidates,
      bye_decision = c(
        lst$bye_decision,
        paste("one most-losses candidate:", candidates)
      )
    ))
  }else{
    continue(list(
      stats = lst$stats,
      current_player_stats = lst$current_player_stats,
      bye = NA,
      bye_decision = c(
        lst$bye_decision,
        paste("most-losses candidates:", toString(candidates))
      ),
      bye_candidates = candidates
    ))
  }
}

played_most_remaining <- function(
  lst
) {
  # currently not implemented, since requires adding pairing history to stats
  new_decision <- c(
    lst$bye_decision,
    "remaining pairing history step not implemented"
  )
  continue(`[[<-`(lst, "bye_decision", new_decision))

  players <- lst$current_player_stats[, player]
  pairing_counts <- lst$stats$pairing_counts
  n_played <- rowSums(
    (pairing_counts[lst$bye_candidates, players] > 0) |
    t(pairing_counts[players, lst$bye_candidates] > 0)
  )
  candidate_played_most <- lst$bye_candidates[n_played == max(n_played)]
  if (length(candidate_played_most) == 1) {
    new_decision <- c(
      lst$bye_decision,
      paste(
        "one candidate that has played the most other remaining players:",
        candidate_played_most
      )
    )
    finish(list(
      stats = lst$stats,
      bye = candidate_played_most,
      bye_decision = new_decision
    ))
  }else{
    new_decision <- c(
      lst$bye_decision,
      paste(
        "candidates that have played the most remaining players:",
        toString(candidate_played_most)
      )
    )
    continue(list(
      stats = lst$stats,
      current_player_stats = lst$current_player_stats,
      bye = NA,
      bye_decision = new_decision,
      bye_candidates = candidate_played_most
    ))
  }
}

random_bye <- function(
  lst
) {
  candidate <- sample(lst$bye_candidates, 1)
  finish(list(
    stats = lst$stats,
    bye = candidate,
    bye_decision = c(
      lst$bye_decision,
      paste("random pick:", candidate)
    )
  ))
}

add_bye_decision <- function(
  stats,
  players
) {
  stats %>%
    no_bye_if_even_player_count(players) %>%
    bind(fewest_byes) %>%
    bind(played_all_others) %>%
    bind(avoid_rematch) %>%
    bind(most_losses) %>%
    bind(played_most_remaining) %>%
    bind(random_bye) %>%
    finish_bind()
}
