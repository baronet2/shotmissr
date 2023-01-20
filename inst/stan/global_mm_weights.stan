data {
  int <lower=1> num_shots; // Number of shots
  int <lower=1> num_components; // Number of mixture components
  matrix <lower=0> [num_shots, num_components] trunc_pdfs;
}
parameters {
  simplex [num_components] global_weights; // Global mixture component weights
}
model {
  global_weights ~ dirichlet(rep_vector(0.5, num_components)); // Lower to prune more aggressively
  for (i in 1:num_shots) {
    target += log(dot_product(global_weights, trunc_pdfs[i]));
  }
}
