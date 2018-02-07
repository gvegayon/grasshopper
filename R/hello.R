
#' The grasshopper problem
#'
#' @param nsim Integer scalar.
#' @param xmax Integer scalar.
#' @param ymax Integer scalar.
#' @param area Integer scalar.
#' @param len Integer scalar.
#' @param subsim Integer scalar.
#' @param ring Numeric scalar. Size of the inner circle, if any. If set to 0
#' then the grasshopper jumps between its spot and `len` uniformly, if greater
#' than 1 then the grasshopper always jumps `len`.
#' @param ncpus Integer scalar.
#'
#' @return A list
#' @export
#'
#' @examples
#' 1+1
grasshopper <- function(
  nsim = 10000,
  xmax = 100,
  ymax = 100,
  area = 100,
  len  = 3,
  subsim = 5000,
  ring = 0.5,
  ncpus = parallel::detectCores()
) {

  cl  <- parallel::makeCluster(ncpus)
  on.exit(parallel::stopCluster(cl))

  # Loading the package
  invisible(parallel::clusterEvalQ(cl, library(grasshopper)))

  # Making the call
  ans <- parallel::parLapply(
    cl, 1L:nsim,
    function(i, xmax, ymax, area, len, subsim, ring) {

      # Simulating grass
      s <- sim_grass_joint(xmax, ymax, area)

      # Returning
      list(
        s$positions,
        grasshopper_stat(
          grass     = s$grass,
          positions = s$positions,
          nsim      = subsim,
          length    = len,
          ring      = ring
        )
      )

    }, xmax = xmax, ymax = ymax, area = area, len = len, subsim = subsim, ring = ring)

  # Sorting output
  ans <- list(
    positions     = lapply(ans, "[[", 1L),
    probabilities = sapply(ans, "[[", 2L)
  )

  # Which one was the best?
  ans$sol <- which.max(ans$probabilities)
  ans$params <-
    list(
      ymax = ymax,
      xmax = xmax,
      len  = len,
      area = area
    )

  # return
  structure(
    ans,
    class = "grasshopper_sim"
  )
}

#' @export
#' @param x An object of class `grasshopper_sim`
#' @param y ignored.
#' @param id Id of the grass to plot.
#' @param ... Ignored.
#' @rdname grasshopper
plot.grasshopper_sim <- function(x, y = NULL, id = x$sol, ...) {

  # Retrieving positions
  pos <- x$positions[[id]] + 1L
  pos <- pos - min(pos) + 2L
  m <- max(pos) + 2L

  mat <- matrix(0L, ncol = m, nrow=m)
  mat[pos] <- 1L

  image(mat, col = c("white", "darkgreen"))

  legend(
    "topleft",
    legend = sprintf("Pr() = %.4f", x$probabilities[id]),
    bty = "n"
    )

  invisible(mat)
}

#' Plot the best and the worse
#' @param x An object of class `grasshopper_sim`
#' @param top Integer scalar
#' @param bottom Integer scalar
#' @export
plot_seq <- function(x, top=4, bottom=4) {

  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))

  par(mfrow = c(2, max(top, bottom)), mai = rep(0, 4), oma=rep(0, 4), xaxt = "n", yaxt="n")
  r <- order(x$probabilities, decreasing = TRUE)
  r <- c(head(r, top), tail(r, bottom))

  for (i in r)
    plot(x, id = i)

  invisible(x)


}




# len <- 10
# pos <- c(50, 50)
#
# set.seed(1222)
# dat <- lapply(1:1000, function(x) grasshopper:::hop(pos, len, ring = .5))
# dat <- do.call(rbind, dat)
#
# plot(dat, xlim = c(1, 100), ylim=c(1, 100), pch=20, cex=.5,
#      col = adjustcolor("steelblue", .5))
# rect(pos[1] - len, pos[2] - len, len + pos[1],len + pos[2])
