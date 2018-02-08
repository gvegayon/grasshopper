
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
#' @return A list
#' @export
#'
#' @examples
#' 1+1
grasshopper <- function(
  nsim = 10000,
  area = 100,
  len  = 3,
  subsim = 5000,
  nchanges = 5,
  ring = 0.99,
  verb = TRUE
) {

  # Initialize the algorithm
  probs   <- vector("double", nsim)
  configs <- vector("list", nsim)
  U       <- runif(nsim)

  # Generating the seed grass
  dat0 <- sim_grass_joint(area, area, area)

  probs[1L] <- grasshopper_stat(
    grass     = dat0$grass,
    positions = dat0$positions,
    nsim      = subsim,
    length    = len,
    ring      = ring
  )

  configs[[1L]] <- dat0$positions
  curbest <- 1L
  dat <- dat0

  for (i in 2L:nsim) {

    # Mutating the grass
    dat <- mutate_grass(dat$grass, dat$positions, nchanges = nchanges)

    # Computing probs and mutating
    probs[i] <- grasshopper_stat(
      grass     = dat$grass,
      positions = dat$positions,
      nsim      = subsim,
      length    = len,
      ring      = ring
    )

    configs[[i]] <- dat$positions

    # Hastings ratio
    if (probs[curbest] < probs[i]) {

      # Updating the ids
      curbest <- i
      dat0    <- dat

      if (verb) {
        message(sprintf("The new best has a prob: %.4f (iter #%i)", probs[i], i))
        image(dat$grass, col = c("brown", "green"))
      }


    } else {

      # Getting back to the current best
      dat <- dat0

    }

  }

  # return
  structure(
    list(
      probabilities = probs,
      positions = configs
    ),
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



#
# len <- 30
# pos <- c(50, 50)
#
# set.seed(1222)
# dat <- lapply(1:1000, function(x) grasshopper:::hop(pos, len, ring = .99))
# dat <- do.call(rbind, dat)
#
# plot(dat, xlim = c(1, 100), ylim=c(1, 100), pch=20, cex=.5,
#      col = adjustcolor("steelblue", .5))
# rect(pos[1] - len, pos[2] - len, len + pos[1],len + pos[2])
