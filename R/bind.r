continue <- function(
  state
) {
  list(
    status = "continue",
    state = state
  )
}

finish <- function(
  state
) {
  list(
    status = "finish",
    state = state
  )
}

bind <- function(
  lst,
  fn,
  ...
) {
  switch(
    lst$status,
    finish = lst,
    continue = fn(lst$state, ...)
  )
}

finish_bind <- function(
  lst
) {
  switch(
    lst$status,
    finish = lst$state,
    continue = {warning("bind not finished"); lst$state}
  )
}
