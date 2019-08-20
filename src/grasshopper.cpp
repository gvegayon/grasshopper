#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector hop(
    NumericVector & pos,
    double length,
    int xmax = 100,
    int ymax = 100,
    double theta = 10.0
) {

  // What is the limit to jump

  if (theta > 2.0*PI)
    theta = unif_rand()*2.0*PI;

  NumericVector y(2);

  y.at(0) = pos.at(0) + (length + 1) * sin(theta);
  y.at(1) = pos.at(1) + (length + 1) * cos(theta);

  if (y.at(0) > pos.at(0)) y.at(0) = floor(y.at(0));
  else y.at(0) = ceilf(y.at(0));

  if (y.at(1) > pos.at(1)) y.at(1) = floor(y.at(1));
  else y.at(1) = ceilf(y.at(1));

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
  while (--nchanges > 0) {

    // Random points
    p_more = floor(unif_rand()*n);
    p_less = floor(unif_rand()*n);

    // // Is it the same point
    // if (p_less == p_more)
    //   continue;

    // // Picking next pos
    x = newpositions.at(p_more, 1) + rindex();
    y = newpositions.at(p_more, 0) + rindex();
    // x = floor(unif_rand()*xmax/4)*(unif_rand() > 0.5? -1 : 1);
    // y = floor(unif_rand()*ymax/4)*(unif_rand() > 0.5? -1 : 1);

    // Checking bounds
    if (x < 0 | y < 0)
      continue;

    if (x >= xmax | y >= ymax)
      continue;

    // Already painted?
    if (newgrass.at(y, x) > 0)
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
    bool deterministic = true
) {

  double prob = 0.0;
  NumericVector pos(2);
  int xmax = grass.ncol();
  int ymax = grass.nrow();

  // Getting the positions
  if (!deterministic) {
    IntegerVector p = sample(positions.nrow(), nsim, true) - 1;


    for (int i = 0; i < nsim; i++) {

      // Fecthing and updating position
      pos = positions.row(p[i]);
      pos = hop(pos, length, xmax, ymax);

      // Did he fell outside?
      if (pos[0] == -1)
        continue;

      if (grass(pos[0], pos[1]) > 0)
        prob += 1.0/(double)nsim;

    }
  } else {

    // How many positions
    int n = positions.nrow();
    int i=0, counts = nsim;

    while (--counts > 0) {

      // Random hop from the ith position
      pos = positions.row(i);
      pos = hop(pos, length, xmax, ymax);


      // Did he fell outside?
      if (pos[0] == -1)
        continue;

      if (grass(pos[0], pos[1]) > 0)
        prob += 1.0/(double)nsim;

      // Should we restart
      i++;
      if (i >= n)
        i = 0;

    }

  }


  return prob;

}

// [[Rcpp::export]]
bool in_hop(
  int i,
  int j,
  const NumericMatrix & positions,
  double length
) {

  double y = positions(j, 0) - positions(i, 0);
  double x = positions(j, 1) - positions(i, 1);
  double theta = atan2(y, x);

  NumericVector where = positions.row(i);

  where = hop(where, length, 1e5, 1e5, theta);

  return where.at(0) == positions.at(j, 0) &
    where.at(1) == positions.at(j, 1);

}

// [[Rcpp::export]]
double grasshopper_stat_deterministic(
    const IntegerMatrix & grass,
    const NumericMatrix & positions,
    double length,
    int counts = 180
) {

  // Parameters
  double prob = 0.0, probi;
  int n = positions.nrow();
  double a = 2.0*PI*length;
  int xmax = grass.ncol();
  int ymax = grass.nrow();

  NumericVector posi(2);
  NumericVector pos(2);
  for (int i = 0; i < n; ++i) {

    probi = 0.0;
    posi = positions.row(i);
    for (int j = 0; j < counts; ++j) {

      // Getting new position
      pos = hop(posi, length, xmax, ymax, 2.0*PI/(double)counts*(double)j);

      // Is it outside?
      if (pos[0] < 0)
        continue;

      if (grass.at(pos[1], pos[0]) > 0)
        probi += 1.0/(double) counts;

    }

    prob += probi/((double)n);

  }

  return prob;
}

