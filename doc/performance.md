# Notes on performance

 - Rule of thumb: adding a 10000-element reservoir can increase
   check time by up to ~20% on a typical feed (obviously highly
   variable and schema-dependent).
 - When sampling is enabled, the amount of time spent on sampling
   during a check grows roughly logarithmically with the size of the
   reservoir.
