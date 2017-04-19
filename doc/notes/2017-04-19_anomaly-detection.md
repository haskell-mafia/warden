Whiteboard session with Erik, Navin and Rukshan to ensure we're all on
the same page wrt plans for numerical anomaly detection in warden.

Summary
-------

 - The two anomaly detection problems (with and without time-series
   context) are very different and bear only a trivial resemblance.

 - Sharif will be working on time-series techniques first, after
   finishing a set of primitives which will be common to both (basic
   linear algebra functionality).

 - Multivariate methods are likely to be very expensive, and there may
   not be any benefit gained from treating rows as part of a vector
   space (as opposed to treating each numeric column
   individually). If multivariate methods are explored the first task
   should be to verify the presence of additional structure (e.g.,
   correlations between fields).

 - Rukshan has put together [notes on possible
   approaches](https://github.com/ambiata/warden/blob/249208061dc1bc4d022d554da113bebf88dec769/doc/stats_and_plots.md),
   focusing on univariate methods.

  - The Seasonal Hybrid Extreme Studentised Deviate test (S-H-ESD)
    looks promising for time series data; Sharif has used it with
    promising results, though never in anger.

  - Rukshan will use the R package to validate the method against
    warden's summary data.

 - Haskell isn't a great language for interactive data analysis,
   Sharif needs to bite the bullet and spend some time with the data
   in R.

  - Sharif will write a tool to extract R-friendly CSV versions of the
    samples from the JSON marker files.

 - Navin mentions topological data analysis being brought to bear on
   similar problems; possibly something to explore in the future.
