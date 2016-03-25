
SurveyScripts
=============

This directory contains scripts for running a survey of datatypes in
Haskell packages.

It manages data in the `./data` subdirectory, which should generally
be a symlink to some big, local. storage.  Here's a cheat sheet for
what the different steps of processing are.

Within `./data` you will find these directories of inputs and
intermediate results:

 * `0_all_hackage_tarballs`
 * `1_only_newest_versions`
 * `2_untarred`
 * `3_ddef_clusters`

The Makefile drives the different stages of the processing pipeline.
