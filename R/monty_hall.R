#' @title Simulate Monty Hall Problem
#'
#' @description simulates one Monty Hall Problem game with the ability to vary
#'   the participant's choice to switch doors probabilistically.
#'
#' @param p probability of switching doors
#'
#' @details Steve Selvin wrote a letter to the American Statistician in 1975
#'   describing a problem based on the game show Let's Make a Deal, (Selvin
#'   1975a), dubbing it the "Monty Hall problem" in a subsequent letter (Selvin
#'   1975b). The problem is mathematically equivalent to the Three Prisoners
#'   Problem described in Martin Gardner's "Mathematical Games" column in
#'   Scientific American in 1959 (Gardner 1959) and the Three Shells Problem
#'   described in Gardner's book Aha Gotcha (Gardner 1982).
#'
#'   The same problem was restated in a 1990 letter by Craig Whitaker to Marilyn
#'   vos Savant's "Ask Marilyn" column in Parade:
#'
#'   Suppose you're on a game show, and you're given the choice of three doors:
#'   Behind one door is a car; behind the others, goats. You pick a door, say
#'   No. 1, and the host, who knows what's behind the doors, opens another door,
#'   say No. 3, which has a goat. He then says to you, "Do you want to pick door
#'   No. 2?" Is it to your advantage to switch your choice? (Whitaker, 1990, as
#'   quoted by vos Savant 1990a)
#'
#' @return logical indicating win status
#'
#' @references
#' Gardner, M. (1959). Mathematical games. \emph{Scientific American, 201}(4), 174-184.
#'
#' Gardner, M. (1982). \emph{Aha! Gotcha: Paradoxes to puzzle and delight}. San Francisco: Freeman.
#'
#' Slevin, S. (1975a). A problem in probability. \emph{American Statistician, 29}(1), 67.
#'
#' Sleven, S. (1975b). On the Monty Hall problem, \emph{American Statistician, 29}(3), 134.
#'
#' vos Savant, M. (1990, September 9). Ask Marilyn. \emph{Parade Magazine}, 16.
#'
#' Whitaker, C. F. (1990, September 9). Ask Marilyn. \emph{Parade Magazine}, 16.
#' @export
#'
#' @examples
#' ## always stay with first choice
#' sims_never <- replicate(10000, sim_monty_hall(p = 0))
#' hist(as.numeric(sims_never))
#' mean(sims_never)
#'
#' ## always switch to other door
#' sims_always <- replicate(10000, sim_monty_hall(p = 1))
#' hist(as.numeric(sims_always))
#' mean(sims_always)
#'
#' ## switch doors with probability p = 0.5
#' sims_even <- replicate(10000, sim_monty_hall(p = 0.5))
#' hist(as.numeric(sims_even))
#' mean(sims_even)
sim_monty_hall <- function(p = 0) {
  all_doors <- LETTERS[1:3]
  winning_door <- sample(all_doors, 1)
  chosen_door <- sample(all_doors, 1)
  opened_door <- sample(setdiff(all_doors, c(winning_door, chosen_door)), 1)
  if(stats::runif(1) < p)
    chosen_door <- setdiff(all_doors, c(chosen_door, opened_door))
  return(winning_door == chosen_door)
}
