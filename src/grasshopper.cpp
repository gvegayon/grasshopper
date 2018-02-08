#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector hop(
    NumericVector & pos,
    double length,
    int xmax = 100,
    int ymax = 100,
    double ring = 0.0
) {

  // What is the limit to jump
  NumericVector y = rnorm(2u, 0, 1);

  // Normalizing
  y = y/sqrt(sum(pow(y, 2.0)));

  // Rescaling
  if (ring < 1.0) {
    double s = pow(length, 2);
    NumericVector u = pow(runif(2u, s*fmin(ring, 1.0), s), 0.5);
    y = y * u;
  }


  y = floor(pos + y);

  if ((y[0] >= 0 && y[0] < xmax) && (y[1] >= 0 && y[1] < ymax))
    return y;
  else
    return NumericVector::create(-1, -1);
}


// [[Rcpp::export]]
List sim_grass(
    int xmax = 100,
    int ymax = 100,
    int area = 100
) {

  IntegerMatrix m(xmax, ymax);
  IntegerMatrix pos(area, 2);
  IntegerVector s = sample(xmax*ymax, area) - 1;

  for (int i = 0; i < s.size(); i++) {
    *(m.begin() + s.at(i)) = 1;

    // row and column
    pos.at(i, 0) = s.at(i) % ymax;
    pos.at(i, 1) = floor(s.at(i) / ymax);
  }


  return List::create(
    _["grass"] = m,
    _["positions"] = pos
  );

}

// [[Rcpp::export]]
int rindex() {

  int i;
  double prob = unif_rand();
  double onethird = 1.0/3.0;

  if (prob < onethird)
    return -1;
  else if (prob < (onethird*2.0))
    return 0;
  else return 1;

}

// [[Rcpp::export]]
List sim_grass_joint(
    int xmax = 100,
    int ymax = 100,
    int area = 100
) {

  IntegerMatrix m(xmax, ymax);
  IntegerMatrix pos(area, 2);

  m.fill(0);

  // Starts at the center
  pos.at(0, 0) = floor(xmax / 2);
  pos.at(0, 1) = floor(ymax / 2);

  m.at(pos.at(0,0), pos.at(0, 1)) = 1;

  // Randomly filling the gaps
  int i = 0, j;
  int x, y;
  double onethird = 1.0/3.0;
  double prob;
  while ((i + 1) < area) {

    // Pick random position
    j = floor(unif_rand()*((double) (i + 1)));

    // Picking next pos
    x = pos.at(j, 1) + rindex();
    y = pos.at(j, 0) + rindex();

    // Checking bounds
    if (x < 0 | y < 0)
      continue;

    if (x >= xmax | y >= ymax)
      continue;

    // Already painted?
    if (m.at(y, x) > 0)
      continue;

    // Storing the info
    pos.at(++i, 0) = y;
    pos.at(i, 1)   = x;

    m.at(y, x) = 1;

  }

  return List::create(
    _["grass"] = m,
    _["positions"] = pos
  );
}

// [[Rcpp::export]]
bool is_bridge(
  int x,
  int y,
  const IntegerMatrix & grass,
  bool recursive = true
) {

  int xmax = grass.ncol();
  int ymax = grass.nrow();

  // Looking at the neighbors
  int nneigh = 0;
  for (int i = -1; i < 2; i++)
    for (int j = -1; j < 2; j++) {

      // Don't care about myself
      if (i == 0 & j == 0)
        continue;

      // Is it in the border?
      if ((x + j) < 0 | (x + j) >= xmax)
        continue;

      if ((y + i) < 0 | (y + i) >= ymax)
        continue;

      // We found a neighbor
      if (grass(y + i, x + j) > 0) {

        // If this is the recursive, let's count their neighbors
        if (recursive) {

          // Am I the only neighbour of my neighbour?
          if (!is_bridge(x+j, y+i, grass, false))
            continue;
           else
            return true;

        } else {
          // How many neighbours
          nneigh++;
        }
      }
    }

  // If only a single neighbour, then it is a bridge
  if (nneigh == 1)
    return true;

  return false;

}

// [[Rcpp::export]]
List mutate_grass(
  const IntegerMatrix & grass,
  const NumericMatrix & positions,
  int nchanges = 1
) {

  IntegerMatrix newgrass     = clone(grass);
  NumericMatrix newpositions = clone(positions);

  int p_less, p_more, n = positions.nrow();
  int xmax = grass.ncol();
  int ymax = grass.nrow();
  int x, y;
  while (nchanges > 0) {

    // Random points
    p_more = floor(unif_rand()*n);
    p_less = floor(unif_rand()*n);

    // Is it the same point
    if (p_less == p_more)
      continue;

    // Picking next pos
    x = newpositions.at(p_more, 1) + rindex();
    y = newpositions.at(p_more, 0) + rindex();

    // Checking bounds
    if (x < 0 | y < 0)
      continue;

    if (x >= xmax | y >= ymax)
      continue;

    // Already painted?
    if (newgrass.at(y, x) > 0)
      continue;

    // Is the point to remove a bridge?
    if (is_bridge(newpositions(p_less, 1), newpositions(p_less, 0), newgrass))
      continue;

    // Is the old point removing the new one?
    if ((x == newpositions(p_less, 1)) | (y == newpositions(p_less, 0)))
      continue;

    // Else, let's change it baby! ---------------------------------------------

    // Removing the point from the grid
    newgrass.at(newpositions(p_less, 0), newpositions(p_less, 1)) = 0;

    // Adding the point to the grid
    newgrass.at(y, x) = 1;

    // Adding the point to the list
    newpositions.at(p_less, 1) = x;
    newpositions.at(p_less, 0) = y;

    --nchanges;

  }

  return List::create(
    _["grass"] = newgrass,
    _["positions"] = newpositions
  );

}

// [[Rcpp::export]]
double grasshopper_stat(
    const IntegerMatrix & grass,
    const NumericMatrix & positions,
    int nsim,
    double length,
    double ring
) {

  // Getting the positions
  IntegerVector p = sample(positions.nrow(), nsim, true) - 1;

  double prob = 0.0;
  NumericVector pos(2);
  int xmax = grass.ncol();
  int ymax = grass.nrow();
  for (int i = 0; i < nsim; i++) {

    // Fecthing and updating position
    pos = positions.row(p[i]);
    pos = hop(pos, length, xmax, ymax, ring);

    // Did he fell outside?
    if (pos[0] == -1)
      continue;

    if (grass(pos[0], pos[1]) > 0)
      prob += 1.0/(double)nsim;

  }

  return prob;

}

