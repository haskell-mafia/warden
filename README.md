Warden
======

```
Warden
/ˈwɔːd(ə)n/
noun

a person responsible for the supervision of a particular place or activity
or for enforcing the regulations associated with it.
```

Warden is the gatekeeper for data entering our system.


Metadata
--------

The result of warden runs will be stored as metadata in files
shadowing the actual data, or in the metadata store. From this
metadata the following state will be clear:

 - The state of the dataset:
    - passed: all checks complete and ok
    - failed: some checks failed

 - Statistics about this feed. These statistics will be
   enough to do over-time-comparisons etc...

 - List of checks, with results, run against this dataset.

 - Enough to do roll-ups to determine the "health" of a
   feed overall.

 - These should be something we can eventually push to a
   centralized service (brandix?).


Running
-------

Warden is a command line tool, and is run directly against a
local copy of a view.

Where a view is the traditional hive-style,
`year=..../month=../day=../dataset` partitioning.

Warden runs on both the view-level and the file-level. View checks are
run on a complete view, or any nonempty subset of a view; these
consist of row counts, numeric summaries, et cetera and can be
schema-aware. File checks perform very basic sanity checks at the file
level (e.g., does the file have a nonzero size).

Metadata from view checks is to be stored in the metadata store, e.g.,
`s3://ambiata-prod-live-state/$customer/warden/$view`. Metadata from
file checks is to be stored back into the view, with the naming schema
`_${view_filename}.warden`. However, warden does not write to S3
itself - it writes metadata to the local filesystem, which can then be
stored in S3 using other tools.

This means you should be always able to run the following on a
configured view:

```
# basic file checks
warden sanity path/to/view
# full view checks
warden check path/to/view
# infer schema from check metadata
warden infer _warden/$view/$data_dates/$check_date/*.json
```

Future Work
-----------

 - Tighter integration with monitoring tools (sending alerts, feeding into smarter online checking etc...).
 - warden integration for downstream tools so they only accept warden approved feeds.
