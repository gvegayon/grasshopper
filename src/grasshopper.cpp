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
    prob = unif_rand();
    if (prob < onethird)
      x = pos.at(j, 1);
    else if (prob < onethird*2)
      x = pos.at(j, 1) - 1;
    else
      x = pos.at(j, 1) + 1;

    prob = unif_rand();
    if (prob < onethird)
      y = pos.at(j, 0);
    else if (prob < onethird*2)
      y = pos.at(j, 0) - 1;
    else
      y = pos.at(j, 0) + 1;


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
double grasshopper_stat(
    const IntegerMatrix & grass,
    const NumericMatrix & positions,
    int nsim,
    double length,
    double ring
) {

  // Getting the positions
  IntegerVector p = sample(positions.nrow() - 1u, nsim, true);

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

