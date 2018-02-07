// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// hop
NumericVector hop(NumericVector& pos, double length, int xmax, int ymax, double ring);
RcppExport SEXP _grasshopper_hop(SEXP posSEXP, SEXP lengthSEXP, SEXP xmaxSEXP, SEXP ymaxSEXP, SEXP ringSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector& >::type pos(posSEXP);
    Rcpp::traits::input_parameter< double >::type length(lengthSEXP);
    Rcpp::traits::input_parameter< int >::type xmax(xmaxSEXP);
    Rcpp::traits::input_parameter< int >::type ymax(ymaxSEXP);
    Rcpp::traits::input_parameter< double >::type ring(ringSEXP);
    rcpp_result_gen = Rcpp::wrap(hop(pos, length, xmax, ymax, ring));
    return rcpp_result_gen;
END_RCPP
}
// sim_grass
List sim_grass(int xmax, int ymax, int area);
RcppExport SEXP _grasshopper_sim_grass(SEXP xmaxSEXP, SEXP ymaxSEXP, SEXP areaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type xmax(xmaxSEXP);
    Rcpp::traits::input_parameter< int >::type ymax(ymaxSEXP);
    Rcpp::traits::input_parameter< int >::type area(areaSEXP);
    rcpp_result_gen = Rcpp::wrap(sim_grass(xmax, ymax, area));
    return rcpp_result_gen;
END_RCPP
}
// sim_grass_joint
List sim_grass_joint(int xmax, int ymax, int area);
RcppExport SEXP _grasshopper_sim_grass_joint(SEXP xmaxSEXP, SEXP ymaxSEXP, SEXP areaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type xmax(xmaxSEXP);
    Rcpp::traits::input_parameter< int >::type ymax(ymaxSEXP);
    Rcpp::traits::input_parameter< int >::type area(areaSEXP);
    rcpp_result_gen = Rcpp::wrap(sim_grass_joint(xmax, ymax, area));
    return rcpp_result_gen;
END_RCPP
}
// grasshopper_stat
double grasshopper_stat(const IntegerMatrix& grass, const NumericMatrix& positions, int nsim, double length, double ring);
RcppExport SEXP _grasshopper_grasshopper_stat(SEXP grassSEXP, SEXP positionsSEXP, SEXP nsimSEXP, SEXP lengthSEXP, SEXP ringSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const IntegerMatrix& >::type grass(grassSEXP);
    Rcpp::traits::input_parameter< const NumericMatrix& >::type positions(positionsSEXP);
    Rcpp::traits::input_parameter< int >::type nsim(nsimSEXP);
    Rcpp::traits::input_parameter< double >::type length(lengthSEXP);
    Rcpp::traits::input_parameter< double >::type ring(ringSEXP);
    rcpp_result_gen = Rcpp::wrap(grasshopper_stat(grass, positions, nsim, length, ring));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_grasshopper_hop", (DL_FUNC) &_grasshopper_hop, 5},
    {"_grasshopper_sim_grass", (DL_FUNC) &_grasshopper_sim_grass, 3},
    {"_grasshopper_sim_grass_joint", (DL_FUNC) &_grasshopper_sim_grass_joint, 3},
    {"_grasshopper_grasshopper_stat", (DL_FUNC) &_grasshopper_grasshopper_stat, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_grasshopper(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}