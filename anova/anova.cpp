// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// unque std::vector<int>
std::vector<int> unique_vec_int(std::vector<int> vec) {
  std::sort(vec.begin(), vec.end());
  std::vector<int> res{vec[0]};
  int counter = 0;
  for(auto i: vec) {
    if(i != res[counter]) {
      res.push_back(i);
      counter++;
    }
  }
  return res;
}

// seperate data into groups
arma::field<arma::vec> split_into_groups(const arma::vec& d, const std::vector<int>& groups) {
    std::vector<int> g = unique_vec_int(groups);
    arma::vec gr(groups.size());
    for(int i = 0; i < groups.size(); i++) gr(i) = static_cast<double>(groups[i]);
    arma::field<arma::vec> res(g.size());
    for(int i = 0; i < g.size(); i++) {
      arma::uvec indices = arma::find(gr == static_cast<double>(g[i]) );
      res(i) = d(indices);
    }
    return res;
}

// calculate SSW = sum of squares within
double ssw(arma::vec& group_means, arma::field<arma::vec>& data_groups) {
  double ssw_res = 0.0;
  for(int i = 0; i < group_means.size(); i++) {
    arma::vec g = data_groups(i);
    for(int j = 0; j < g.size(); j++) {
      ssw_res += (g(j) - group_means(i))*(g(j) - group_means(i));
    }
  }
  return ssw_res;
}

// calculate SSB = sum of squares between or SSE = sum of squares errors/residuals
double ssb(const double grand_mean, arma::vec& group_means, arma::vec& size_groups) {
  double ssb_res = 0.0;
  for(int i = 0; i < group_means.size(); i++) {
    ssb_res += (grand_mean - group_means(i))*(grand_mean - group_means(i))*size_groups(i);
  }
  return ssb_res;
}

// calculate SST = sum of squares total
double sst(const double grand_mean, const arma::vec& data) {
  double sst_res = 0.0;
  for(int i = 0; i < data.size(); i++) {
    sst_res += (data(i) - grand_mean)*(data(i) - grand_mean);
  }
  return sst_res;
}
 
// [[Rcpp::export]]
arma::vec own_anova(const arma::vec data, const std::vector<int> groups) {
  
  const double grand_mean = arma::mean(data);
  arma::field<arma::vec> data_groups = split_into_groups(data, groups);
  arma::vec group_means(data_groups.size());
  for(int i = 0; i < data_groups.size(); i++) group_means(i) = arma::mean(data_groups(i));
  arma::vec group_means_size(data_groups.size());
  for(int i = 0; i < group_means_size.size(); i++) group_means_size(i) = static_cast<double>(data_groups(i).size());
  
  // sum of squares
  const double SSW = ssw(group_means, data_groups);
  const double SSB = ssb(grand_mean, group_means, group_means_size);
  const double SST = sst(grand_mean, data); 
  Rcpp::Rcout << "SST " << SST << " SSW " << SSW << " SSB " << SSB << std::endl;
  
  // degree of freedoms 
  const double df_ssb = static_cast<double>(group_means.size()) - 1.0;
  const double df_ssw = static_cast<double>(data.size() - group_means.size());
  const double df_sst = static_cast<double>(data.size()) - 1.0;
  Rcpp::Rcout << "df ssb " << df_ssb << " df ssw " << df_ssw << " df sst " << df_sst << std::endl;
  
  // calculate mean squares between
  const double MSB = SSB/df_ssb;
  // calculate mean squares within
  const double MSW = SSW/df_ssw;
  Rcpp::Rcout << "MSB " << MSB << " MSW " << MSW << std::endl;
  
  // F value
  const double F = MSB / MSW;
  Rcpp::Rcout << "F " << F << std::endl;
  
  return arma::vec{F, df_ssb, df_ssw};
}

/*** R
data <-iris[, 1]
group <- iris[, 5]
F_DFssb_DFssw <- own_anova(data, group)
F_DFssb_DFssw
pf(F_DFssb_DFssw[1], F_DFssb_DFssw[2], F_DFssb_DFssw[3], lower.tail = FALSE)
broom::tidy(aov(iris$Sepal.Length ~ iris$Species))
*/
