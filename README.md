Forkk.net Website
=================

This is my personal website and blog.


Building
========

To build the site, run the following commands in the root folder of the source
tree:

    cabal sandbox init
    cabal install -j --dependencies-only
    cabal build
    ./site rebuild

To work on the site, run `./site watch`. This starts a webserver to host the
site for testing at `localhost:8000` and automatically rebuilds any files that
change.
