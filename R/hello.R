
#' The grasshopper problem
#'
#' @param nsim Integer scalar.
#' @param xmax Integer scalar.
#' @param ymax Integer scalar.
#' @param area Integer scalar.
#' @param len Integer scalar.
#' @param subsim Integer scalar.
#' @return A list
#' @export
#'
#' @examples
#' 1+1
grasshopper <- function(
  grass = NULL,
  nsim = 10000,
  area = 100,
  len  = 3,
  subsim = 5000,
  temp = 100,
  verb = TRUE,
  continuous = FALSE
) {

  # Initialize the algorithm
  probs   <- vector("double", nsim)
  configs <- vector("list", nsim)

  # Generating the seed grass
  if (length(grass)) {
    dat0 <- grass
  } else {
    canvas <- floor(sqrt(area*6))
    dat0 <- if (continuous)
      sim_grass_joint(canvas, canvas, area)
    else
      sim_grass(canvas, canvas, area)
  }


  probs[1L] <- grasshopper_stat(
    grass     = dat0$grass,
    positions = dat0$positions,
    nsim      = subsim,
    length    = len,
    deterministic = TRUE
  )

  configs[[1L]] <- dat0$positions
  curbest <- 1L
  dat <- dat0

  message(sprintf("The new best has a prob: %.4f (iter #%i)", probs[1], 1))
  image(dat$grass, col = c("brown", "green"))

  # Simulated annealing part
  nchanges <- ceiling(
    (area*2)*((1/1:nsim)^(1/temp) - (1/nsim)^(1/temp))/
      (1 - (1/nsim)^(1/temp))
    ) + floor(area*.1)

  Temp <- temp*.999^(1:nsim)

  U <- runif(nsim) #*paccept + .5*(1 - paccept)

  for (i in 2L:nsim) {

    # Mutating the grass
    dat <- mutate_grass(dat$grass, dat$positions, nchanges = nchanges[i])

    # Computing probs and mutating
    probs[i] <- grasshopper_stat(
      grass     = dat$grass,
      positions = dat$positions,
      nsim      = subsim,
      length    = len,
      deterministic = TRUE
      # counts    = subsim

    )

    configs[[i]] <- dat$positions

    # Hastings ratio
    d <- probs[i] - probs[curbest]

    if ((d > 0)) { #  || (U[i] > exp(d/Temp[i]))

      # Updating the ids
      curbest <- i
      dat0    <- dat

    } else {

      # Getting back to the current best
      dat <- dat0

    }

    if (verb & !(i %% 200)) {
      message(sprintf("Prob: %.4f at iter %i of %i. Current temp: %i. Energy:%.2f", probs[curbest], curbest, i, nchanges[i], exp(d/Temp[i])))
      image(dat$grass, col = c("brown", "green"))
    }

  }

  # return
  structure(
    list(
      probabilities = probs,
      positions = configs,
      grass = dat0
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
#
# len <- 10
# pos <- c(10, 10)
#
# set.seed(1222)
# dat <- lapply(1:1000, function(x) grasshopper:::hop(pos, len, theta=8))
# dat <- do.call(rbind, dat)
#
# plot(dat, xlim = c(1, 10), ylim=c(1, 10), pch=20, cex=.5,
#      col = adjustcolor("steelblue", .5))
# rect(pos[1] - len, pos[2] - len, len + pos[1],len + pos[2])
