# Numerics

## Numeric fields

Numeric summaries are calculated for any column which has at least one
value which parses as numeric (thus, care must be taken to ensure that
only results from fields which are actually numeric are used).

## Mean calculation

The mean of numeric fields is computed using the following recurrence relation [from @Knuth2007, pp. 232]:

$$M_1 = x_1, M_k = M_{k-1} + \frac{x_k - M_{k-1}}{k}$$

This has the advantages of operating in a single pass and accumulating
less error on large datasets than the direct calculation
$\mu = \frac{1}{n} \sum\limits_{i=1}^n x_i$.

A standard method in numerical computing for calculating the mean with
minimal error is to sort the dataset and then perform the sum in
order; this doesn't work for us as it requires multiple passes. An
alternative might be Kahan summation [@Kahan1965], but this raises concerns about
precision lost on large datasets when storing the entire sum in one
64-bit float.

## Standard deviation and variance

The accumulator is computed using the following recurrence relation [@Knuth2007, pp. 232]:

$$S_1 = 0, S_k = S_{k-1} + (x_k - M_{k-1})(x_k - M_k)$$

This quantity related to the variance with
$\sigma^2 = \frac{S_n}{n-1}$. 

## Combining accumulators

Warden computes summaries of chunks of data in parallel, which results
in the need to combine accumulators of subsets of the dataset to
arrive at the final result.

### Mean

The two terms are scaled according to the corresponding number of
observed values and then divided by the total number of observed
elements:

$$\mu_{1:n+m} = \frac{n(\mu_{1:n}) + m(\mu_{n:n+m})}{m + n}$$

#### Derivation

$$\mu_{1:n+m} = \frac{1}{m+n} \sum\limits_{i=1}^{m+n} x_i$$

$$\begin{aligned}(m + n) \mu_{1:n+m} &= \sum\limits_{i=1}^{n+m} x_i \\
                                     &= \sum\limits_{i=1}^n x_i + \sum\limits_{i=n}^{n+m} x_i \\
                                     &= n\mu_{1:n} + m\mu_{n:n+m}\end{aligned}$$

$$\mu_{1:n+m} = \frac{n\mu_{1:n} + m\mu_{n:n+m}}{m + n}$$

### Variance/standard deviation

With respect to variance, many methods are available; some are
decidedly worse than others, but even the best perform badly on
certain types of data. Thus, the correct choice here
depends on the distribution of the data we expect to validate, and as
`warden` is designed to work on almost all real-world datasets there
is no single optimal algorithm, and adapting the algorithm to the
dataset being examined is not practical as the optimal algorithm
depends on (among other properties of the data distribution) its
variance. [@Ling1974]

The accumulator is first converted to population variance. Then the
two subset variances are combined:

$$\sigma_{1:n+m}^2 = \frac{m(\sigma_{1:m}^2 + \mu_{1:m}^2) + n(\sigma_{m:n}^2 + \mu_{m:n}^2)}{m + n} - \mu_{1:n+m}^2$$

The naive floating-point computation of this value was found to be
numerically unstable and was optimised with the help of Herbie
[@panchekha2015] to the following:

$$\sigma_{1:n+m}^2 =
  m \otimes \sigma_{1:m}^2 \oplus m \otimes \mu_{1:m} \otimes \mu_{1:m} \oplus
  n \otimes \sigma_{m:n+m}^2 \oplus n \otimes \mu_{m:n+m} \otimes \mu_{m:n+m}
  \otimes (1 \oslash (m \oplus n))$$

Finally, the combined variance is converted back to an accumulator:

$$S_n = \sigma^2(n - 1)$$

This method remains problematic due to accumulation of floating-point
error, and requires some refinement.

#### Derivation

Derivation of the combination of two subset variances[^whuber]:

From the definitions of mean and variance:

$$\mu_{1:n} = \frac{1}{n} \sum\limits_{i=1}^n x_i$$

$$\sigma_{1:n}^2 = \frac{1}{n} \sum\limits_{i=1}^n (x_i - \mu_{1:n})^2$$

We have:

$$\begin{aligned}(m + n)(\sigma_{1:m+n}^2 + \mu_{1:m+n}^2) &= \sum\limits_{i=1}^{n+m} x_i^2 \\
  &= \sum_{i=1}^n x_i^2 + \sum_{i=n+1}^{n+m} x_i^2 \\
  &= n(\sigma^2_{1:n} + \mu_{1:n}^2) + m(\sigma_{1+n:m+n}^2 + \mu_{1+n:m+n}^2)\end{aligned}$$

Solving:

$$\sigma_{1:m+n}^2 = \frac{n(\sigma_{1:n}^2 + \mu_{1:n}^2) + m(\sigma_{1+n:m+n}^2 + \mu_{1+n:m+n}^2)}{m+n} - \mu_{1:m+n}^2$$

[^whuber]: Originally from
           https://stats.stackexchange.com/questions/43159/how-to-calculate-pooled-variance-of-two-groups-given-known-group-variances-mean
