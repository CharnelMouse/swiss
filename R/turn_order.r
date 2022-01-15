# Player to go first is
# a) gone first fewer times in tournament (not counting byes)
# b) if re-match, fewer times gone first against this opponent
# c) random pick

fewer_global_firsts_goes_first <- function(
  pair,
  pairing_counts
) {
  global_firsts <- rowSums(pairing_counts[pair, ])
  if (global_firsts[1] == global_firsts[2])
    continue(list(
      pair = pair,
      decisions = paste(
        pair[1],
        "and",
        pair[2],
        "have gone first the same number of times"
      )
    ))
  else{
    new_pair <- pair[order(global_firsts)]
    finish(list(
      turn_order = data.table(P1 = new_pair[1], P2 = new_pair[2]),
      decisions = paste(
        new_pair[1],
        "goes first: has gone first fewer times than",
        new_pair[2]
      )
    ))
  }
}

fewer_matchup_firsts_goes_first <- function(
  turn_order,
  pairing_counts
) {
  local_firsts <- rowSums(pairing_counts[turn_order$pair, turn_order$pair])
  if (local_firsts[1] == local_firsts[2])
    continue(list(
      pair = turn_order$pair,
      decisions = c(
        turn_order$decisions,
        paste(
          turn_order$pair[1],
          "and",
          turn_order$pair[2],
          "have gone first against each other the same number of times"
        )
      )
    ))
  else{
    new_pair <- turn_order$pair[order(local_firsts)]
    finish(list(
      turn_order = data.table(P1 = new_pair[1], P2 = new_pair[2]),
      decisions = c(
        turn_order$decisions,
        paste(
          new_pair[1],
          "goes first: has gone first against",
          new_pair[2],
          "fewer times than vice versa"
        )
      )
    ))
  }
}

random_turn_order <- function(
  turn_order_info,
  pairing_counts
) {
  new_pair <- sample(turn_order_info$pair)
  finish(
    list(
      turn_order = data.table(P1 = new_pair[1], P2 = new_pair[2]),
      decisions = c(
        turn_order_info$decisions,
        paste("turn order randomised to", toString(new_pair))
      )
    )
  )
}

choose_order <- function(
  pair,
  pairing_counts
) {
  pair %>%
    fewer_global_firsts_goes_first(pairing_counts) %>%
    bind(fewer_matchup_firsts_goes_first, pairing_counts) %>%
    bind(random_turn_order) %>%
    finish_bind()
}

turn_decision <- function(
  prev,
  pair,
  pairing_counts
) {
  turn_order <- choose_order(pair, pairing_counts)
  list(
    pairs = rbind(
      prev$pairs,
      turn_order$turn_order
    ),
    turn_decisions = c(
      prev$turn_decisions,
      turn_order$decisions
    )
  )
}

set_turn_orders <- function(
  stats_with_pairings
) {
  turn_orders <- Reduce(
    function(x, y) turn_decision(x, y, stats_with_pairings$stats$pairing_counts),
    stats_with_pairings$pairs,
    init = list(
      pairs = data.table(P1 = character(), P2 = character()),
      turn_decisions = character()
    )
  )
  c(
    stats_with_pairings[c("stats", "bye", "bye_decision", "pair_decision")],
    turn_orders
  )
}
