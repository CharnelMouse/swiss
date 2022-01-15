context("stats")

test_that(
  "can take empty matches",
  {
    sample_players <- c(
      "bansa",
      "charnel_mouse",
      "codexnewb",
      "FrozenStorm",
      "zhavier"
    )
    sample_matches <- data.table(
      round = integer(),
      P1 = character(),
      P2 = character(),
      victor = integer()
    )
    sample_elimination <- 3L
    sample_stats <- player_stats(sample_matches, sample_players, sample_elimination)
    expect_identical(
      sample_stats,
      list(
        player_stats = data.table(
          player = sample_players,
          last_round = c(0L, 0L, 0L, 0L, 0L),
          played = c(0L, 0L, 0L, 0L, 0L),
          won = c(0L, 0L, 0L, 0L, 0L),
          lost = c(0L, 0L, 0L, 0L, 0L),
          byes = c(0L, 0L, 0L, 0L, 0L),
          eliminated = FALSE
        ),
        pairing_counts = matrix(
          as.integer(c(
            0, 0, 0, 0, 0,
            0, 0, 0, 0, 0,
            0, 0, 0, 0, 0,
            0, 0, 0, 0, 0,
            0, 0, 0, 0, 0
          )),
          byrow = TRUE,
          nrow = 5,
          dimnames = list(sample_players, sample_players)
        )
      )
    )
  }
)

test_that(
  "can generate matches for non-empty match history",
  {
    sample_players <- c(
      "bansa",
      "charnel_mouse",
      "codexnewb",
      "FrozenStorm",
      "zhavier"
    )
    sample_matches <- data.table(
      round = 1L,
      P1 = c("charnel_mouse", "FrozenStorm"),
      P2 = c("codexnewb", "zhavier"),
      victor = c(2L, 1L)
    )
    sample_elimination <- 3L
    sample_stats <- player_stats(sample_matches, sample_players, sample_elimination)
    expect_identical(
      sample_stats,
      list(
        player_stats = data.table(
          player = sample_players,
          last_round = c(0L, 1L, 1L, 1L, 1L),
          played = c(0L, 1L, 1L, 1L, 1L),
          won = c(0L, 0L, 1L, 1L, 0L),
          lost = c(0L, 1L, 0L, 0L, 1L),
          byes = c(1L, 0L, 0L, 0L, 0L),
          eliminated = FALSE
        ),
        pairing_counts = matrix(
          as.integer(c(
            0, 0, 0, 0, 0,
            0, 0, 1, 0, 0,
            0, 0, 0, 0, 0,
            0, 0, 0, 0, 1,
            0, 0, 0, 0, 0
          )),
          byrow = TRUE,
          nrow = 5,
          dimnames = list(sample_players, sample_players)
        )
      )
    )
    without_drop <- generate_matches(sample_stats, sample_players, 2021-03-20)
    expect_identical(nrow(without_drop$pairs), 2L)
    expect_false(is.na(without_drop$bye))
    with_drop <- generate_matches(sample_stats, setdiff(sample_players, "charnel_mouse"), 2021-03-20)
    expect_identical(nrow(with_drop$pairs), 2L)
    expect_true(is.na(with_drop$bye))
    many_drop <- generate_matches(sample_stats, c("codexnewb", "FrozenStorm"), 2021-03-20)
    expect_false(any(grepl(
      "have gone first the same number of times$",
      many_drop$decision_log
    )))
  }
)
