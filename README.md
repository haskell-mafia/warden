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
shadowing the actual data. From this metadata the following
state will be clear:

 - The state of the dataset:
    - green: all checks complete and ok
    - red: checks failed
    - yellow: warnings generated due to concerning values
    - white: checks in progress

 - Statistics about this feed. These statistics will be
   enough to do over-time-comparisons etc...

 - List of checks, with results, run against this dataset.

 - Enough to do roll-ups to determine the "health" of a
   feed overall.

 - These should be something we can eventually push to a
   centralized service (brandix?).


Running
-------

Warden is a command line tool, and will be run directly against a
local copy of a view.

Where a view is the traditional hive-style,
`year=..../month=../day=../dataset` partitioning.

Warden will run on all files that require checks. Warden will store
its metadata back into the view.

This means you should be always able to run the following on a
configured view:

```
warden check path/to/view
```

This will validate all outstanding data-sets. There will also be
options to:

 - Do a dry-run and not write back metadata.

 - Only scan a selected subset of data in the view.


Future Work
-----------

 - Fixed schema configuration.
 - Automate schema/check inference based on a sample dataset.
 - Tighter integration with monitoring tools (sending alerts, feeding into smarter online checking etc...).
 - warden integration for downstream tools so they only accept warden approved feeds.
 - Work out a clean (hopefully more general than just warden) way of integrating warden as a job pre-condition into hydra.
