## Statistical Regression
Statistical regression methods

- Linear regression using batch gradient descent


To-Do:

- Linear regression using stochastic gradient descent


## Noisy Signal Generation
Generating noise for a clean signal to simulate real-world conditions.

There are two components to the noise applied to the clean signal:

* signal noise
    - Gaussian noise applied to all samples of the signal
* outliers
    - Also defined by a Gaussian distribution - usually with a higher standard deviation.
    - Only '_n_' number of outliers are applied to the samples in the signal.

Uses the [Chart](https://hackage.haskell.org/package/Chart-1.9.1) package for easy plotting.
