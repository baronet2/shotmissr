data {
  int <lower=1> num_players; // Number of players
  int <lower=1> num_shots; // Number of shots
  int <lower=1> num_components; // Number of mixture components

  int <lower=1, upper=num_players> shot_players [num_shots]; // Player labels

  matrix <lower=0> [num_shots, num_components] trunc_pdfs;

  real <lower=0> alpha;
}
parameters {
  simplex [num_components] global_weights; // Global mixture component weights
  simplex [num_components] player_weights[num_players];
}
model {
  for (i in 1:num_players) {
    player_weights[i] ~ dirichlet(alpha * global_weights);
  }
  for (i in 1:num_shots) {
    target += log(dot_product(player_weights[shot_players[i]], trunc_pdfs[i]));
  }
}
