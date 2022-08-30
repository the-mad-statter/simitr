#' Secretary Problem
#'
#' @description The secretary problem demonstrates a scenario involving optimal
#' stopping theory that is studied extensively in the fields of applied
#' probability, statistics, and decision theory. It is also known as the
#' marriage problem, the sultan's dowry problem, the fussy suitor problem,
#' the googol game, and the best choice problem.
#'
#' The basic form of the problem is the following: imagine an administrator who
#' wants to hire the best secretary out of n rankable applicants for a
#' position. The applicants are interviewed one by one in random order. A
#' decision about each particular applicant is to be made immediately after
#' the interview. Once rejected, an applicant cannot be recalled. During the
#' interview, the administrator gains information sufficient to rank the
#' applicant among all applicants interviewed so far, but is unaware of the
#' quality of yet unseen applicants. The question is about the optimal strategy
#' (stopping rule) to maximize the probability of selecting the best applicant.
#' If the decision can be deferred to the end, this can be solved by the simple
#' maximum selection algorithm of tracking the running maximum (and who
#' achieved it), and selecting the overall maximum at the end. The difficulty
#' is that the decision must be made immediately.
#'
#' The optimal policy for the problem is a stopping rule. Under it
#' the reviewer rejects the first r - 1 applicants and then selects the first
#' subsequent applicant that is better than the best applicant among the r - 1
#' applicants.
#'
#' @param r index to start choosing at (only observe first r - 1)
#' @param n total sample size
#'
#' @return the probability that the best applicant is selected for a given r.
#' @export
#'
#' @examples
#' ## 100 applicants
#' r <- 2:100
#' n <- 100
#' ps <- ped_secretary(r, n)
#' max_p_i <- which.max(ps)
#' r[max_p_i]
#' ps[max_p_i]
#'
#' @references
#' https://en.wikipedia.org/wiki/Secretary_problem
ped_secretary <- function(r, n) {
  purrr::map2_dbl(r, n, ~ (.x - 1) / .y * sum(1 / (.x:.y - 1)))
}
