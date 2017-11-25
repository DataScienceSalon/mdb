#------------------------------------------------------------------------------#
#                         Sample Size Calculations                             #
#------------------------------------------------------------------------------#
#' sampleSize
#'
#' \code{sampleSize} Computes minimum sample size given log imdb votes effect
#' size and genre proportions.
#'
#' @param movies Data frame containing sample movie data
#' @param genres Data frame containing genre proportion information
#'
#' @return List containing sample size analyses for continuous outcome
#' and proportions.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family analysis functions
#' @export
sampleSize <- function(movies, genres) {

  # Compute sample size required for desire effect size
  mdb <- movies %>% mutate(imdb_num_votes_log = log(imdb_num_votes))
  e <- 1/10 * (max(mdb$imdb_num_votes_log) - min(mdb$imdb_num_votes_log))
  ssVotes <- getCSampleSize(data = mdb$imdb_num_votes_log, e = e)

  # Compute sample sizes required for each genre
  genres <- genres %>% mutate(ss = getPSampleSize(proportion))

  # Summarize counts by genre from sample
  sp <- movies %>% group_by(genre) %>% summarize(N = n())

  # Merge population and sample data
  genres <- merge(genres, sp)

  # Compute Difference
  genres <- genres %>% mutate(diff = N - ss)
  colnames(genres) <- c("Genre", "p", "Min N", "N", "Diff")

  ss <- list(
    votes = ssVotes,
    genres = genres
  )

  return(ss)
}
