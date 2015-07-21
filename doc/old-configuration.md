Configuration
-------------

*I think we have decided that no fixed configuration is required for now*

Warden needs a description of a view and the checks to run.

A minimal description:

```
[warden]
        format: pdv|cdv|psv|csv|json|etc...
```

This will run the bare minimum set of checks:
 - No corrupt/long lines.
 - Each row tokenizes correctly with a consistent length.
 - (Very) Basic PII checks.

Each format also supports a number of additional checks, for example:

```
[pdv]
        columns: 4
        types:
                string
                int
                string
                string

```

From the information provided, a maximal set of statistics and
cross-checks will be determined for the dataset.
