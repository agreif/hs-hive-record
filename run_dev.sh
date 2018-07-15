#!/bin/sh

which yesod
if test $? -ne 0; then
    stack build yesod-bin
fi

stack clean

PGUSER=hiverec \
    PGPASS=hiverec \
    PGDATABASE=hiverec \
    stack exec -- yesod devel
