# Miss It Like Messi: Extracting Value from Off-Target Shots in Soccer

This is a companion repository to the paper "Miss It Like Messi: Extracting Value from Off-Target Shots in Soccer".
Previous version of this work were presented at the [2021 New England Symposium on Statistics in Sport](https://www.youtube.com/watch?v=zQCl1cL-JxA&t=6s)
and the 2022 Canadian Operational Research Society Annual Conference in Vancouver, Canada, where it was selected as the winner of the
Undergraduate Category in the [Student Paper Competition](https://www.cors.ca/?q=content/student-paper-competition).

## Abstract

Measuring soccer shooting skill is a challenging analytics problem due to the scarcity and highly contextual nature
of scoring events. The introduction of more advanced data surrounding soccer shots has given rise to model-based metrics
which better cope with these challenges. Specifically, metrics such as expected goals added, goals above expectation,
and post-shot expected goals all use advanced data to offer an improvement over the classical conversion rate. However,
all metrics developed to date assign a value of zero to off-target shots, which account for almost two-thirds of all
shots, since these shots have no probability of scoring. We posit that there is non-negligible shooting skill signal
contained in the trajectories of off-target shots and propose two shooting skill metrics that incorporate the signal
contained in off-target shots. Specifically, we develop a player-specific generative model for shot trajectories based
on a mixture of truncated bivariate Gaussian distributions. We use this generative model to compute metrics that allow
us to attach non-zero value to off-target shots. We demonstrate that our proposed metrics are more stable than
current state-of-the-art metrics and have increased predictive power.

## Usage

This repository contains the code used to generate the results in our paper.
In particular see, [reproduction/miss_it_like_messi.md](https://github.com/baronet2/shotmissr/blob/main/reproduction/miss_it_like_messi.md).

Our data was acquired through an academic partnership with StatsBomb LLC so cannot be shared publicly.
However, the code may be useful to other analysts working with StatsBomb shooting data.

To import our code as an R package, use:

```r
devtools::install_github("baronet2/shotmissr")
```

## Citation

If you use this work, please cite our paper. We provide the `bibtex` below:

```bibtex
@misc{miss_it_like_messi,
  author =       {Baron, Ethan and Sandholtz, Nathan and Chan, Timothy, and Pleuler, Devin},
  title =        {Miss It Like Messi: Extracting Value from Off-Target Shots in Soccer},
}
```
