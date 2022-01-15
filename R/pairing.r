# Remaining players paired with
# a) opponent paired against fewest times (usually zero)
# b) same number of losses (not uncounted unfinished matches for player not on
#    final elimination)
# c) random pick
# [I don't think this is right, e.g. would be paired with closer number of
# losses before being random pick, priority given to lower-losses players at b)]

start_pair_tracker_without_nonlisted_and_byed_players <- function(
  stats,
  players
) {
  used_players <- setdiff(players, stats$bye)
  if (length(used_players) <= 1)
    finish(
      c(
        stats,
        list(
          pairs = list(),
          pair_decision = c(
            paste("players to pair:", toString(used_players)),
            "not enough players to pair"
          )
        )
      )
    )
  else
    continue(
      c(
        stats,
        list(
          pairing_tracker = list(
            remaining_player_stats = stats$stats$player_stats[
              .(used_players),
              on = "player"
            ],
            remaining_sym_pairing_counts =
              stats$stats$pairing_counts[used_players, used_players] +
              t(stats$stats$pairing_counts[used_players, used_players]),
            pairs = list(),
            pair_decision = paste("players to pair:", toString(used_players))
          )
        )
      )
    )
}

choose_minimum_loss_first_player <- function(
  stats_with_pairing_tracker
) {
  min_lost_stats <-
    stats_with_pairing_tracker$pairing_tracker$remaining_player_stats[
      lost == min(lost)
    ]
  current_player <- min_lost_stats[, sample(player, 1)]
  list(
    remaining_player_stats =
      stats_with_pairing_tracker$pairing_tracker$remaining_player_stats,
    remaining_sym_pairing_counts =
      stats_with_pairing_tracker$pairing_tracker$remaining_sym_pairing_counts,
    current_player = current_player
  )
}

least_loss_opponents <- function(
  pairing_tracker
) {
  starting_opponents <- setdiff(
    pairing_tracker$remaining_player_stats[, player],
    pairing_tracker$current_player
  )
  counts <- setNames(
    pairing_tracker$remaining_sym_pairing_counts[
      pairing_tracker$current_player,
      starting_opponents
    ],
    starting_opponents
  )
  if (is.null(names(counts))) stop(print(counts))
  opponents <- names(counts[counts == min(counts)])
  new_decision <- paste0(
    "least-played opponents for ",
    pairing_tracker$current_player,
    ": ",
    toString(opponents)
  )
  opponents_with_least_losses <- pairing_tracker$remaining_player_stats[
    player %in% opponents
  ][
    lost == min(lost), player
  ]
  if (length(opponents_with_least_losses) == 1) {
    new_decision <- c(
      new_decision,
      paste0(
        pairing_tracker$current_player,
        " has single least-losses possible opponent: ",
        opponents_with_least_losses
      )
    )
    new_pair <- c(pairing_tracker$current_player, opponents_with_least_losses)
    finish(list(
      new_pair = new_pair,
      new_pair_decision = new_decision
    ))
  }else{
    new_decision <- c(
      new_decision,
      paste0(
        pairing_tracker$current_player,
        " has several least-losses possible opponents: ",
        toString(opponents_with_least_losses)
      )
    )
    involved_players <- c(pairing_tracker$current_player, opponents_with_least_losses)
    continue(list(
      current_player = pairing_tracker$current_player,
      opponents = opponents_with_least_losses,
      remaining_player_stats = pairing_tracker$remaining_player_stats[
        is.element(player, involved_players)
      ],
      remaining_sym_pairing_counts = pairing_tracker$remaining_sym_pairing_counts[
        involved_players, involved_players
      ],
      new_pair_decision = new_decision
    ))
  }
}

random_opponent <- function(
  pairing_info
) {
  current_player <- pairing_info$current_player
  opponents <- pairing_info$opponents
  opponent <- sample(opponents, 1)
  new_pair <- c(current_player, opponent)
  new_pair_indices <- match(new_pair, pairing_info$remaining_player_stats$player)
  new_decision <- c(
    pairing_info$new_pair_decision,
    paste0("random opponent for ", current_player, ": ", opponent)
  )
  finish(list(
    remaining_player_stats = pairing_info$remaining_player_stats[
      !is.element(player, new_pair)
    ],
    remaining_sym_pairing_counts = pairing_info$remaining_sym_pairing_counts[
      -new_pair_indices, -new_pair_indices
    ],
    new_pair = new_pair,
    new_pair_decision = new_decision
  ))
}

push_pairing <- function(
  pairing_info,
  stats_with_tracker
) {
  new_pair_indices <- match(
    pairing_info$new_pair,
    stats_with_tracker$pairing_tracker$remaining_player_stats$player
  )
  c(
    stats_with_tracker[c("stats", "bye", "bye_decision")],
    list(
      pairing_tracker = list(
        remaining_player_stats =
          stats_with_tracker$pairing_tracker$remaining_player_stats[
            -new_pair_indices
          ],
        remaining_sym_pairing_counts =
          stats_with_tracker$pairing_tracker$remaining_sym_pairing_counts[
            -new_pair_indices, -new_pair_indices
          ],
        pairs = c(
          stats_with_tracker$pairing_tracker$pairs,
          list(pairing_info$new_pair)
        ),
        pair_decision = c(
          stats_with_tracker$pairing_tracker$pair_decision,
          pairing_info$new_pair_decision
        )
      )
    )
  )
}

make_single_pairing <- function(
  stats_with_pairing_tracker
) {
  stats_with_pairing_tracker %>%
    choose_minimum_loss_first_player() %>%
    least_loss_opponents() %>%
    bind(random_opponent) %>%
    finish_bind() %>%
    push_pairing(stats_with_pairing_tracker)
}

pair <- function(
  stats_with_pairing_tracker
) {
  if (nrow(stats_with_pairing_tracker$pairing_tracker$remaining_player_stats) == 0) {
    new_decision <- c(
      stats_with_pairing_tracker$pairing_tracker$pair_decision,
      "all players paired"
    )
    finish(
      c(
        stats_with_pairing_tracker[c("stats", "bye", "bye_decision")],
        list(
          pairs = stats_with_pairing_tracker$pairing_tracker$pairs,
          pair_decision = new_decision
        )
      )
    )
  }else{
    make_single_pairing(stats_with_pairing_tracker) %>%
      pair()
  }
}

pair_remaining_players <- function(
  stats,
  players
) {
  stats %>%
    start_pair_tracker_without_nonlisted_and_byed_players(players) %>%
    bind(pair) %>%
    finish_bind()
}
