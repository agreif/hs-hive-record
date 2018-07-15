#!/bin/sh

uname -a | grep -q Darwin
if test $? -eq 0; then
    # sudo su postgres -c '/opt/local/lib/postgresql95/bin/createuser --createdb --encrypted --no-inherit --login --pwprompt --no-createrole --no-superuser --no-replication hiverec'
    # sudo su postgres -c '/opt/local/lib/postgresql95/bin/createdb --encoding=UTF-8 --owner=hiverec --template=template0 hiverec'
    # sudo su postgres -c '/opt/local/lib/postgresql95/bin/psql -U hiverec hiverec'
    # sudo su postgres -c '/opt/local/lib/postgresql95/bin/dropdb hiverec'

    sudo su postgres -c 'EDITOR=emacs /opt/local/lib/postgresql95/bin/psql -U hiverec hiverec'
fi


uname -a | grep -q Ubuntu
if test $? -eq 0; then
    # sudo su postgres -c 'createuser --createdb --encrypted --no-inherit --login --pwprompt --no-createrole --no-superuser --no-replication hiverec'
    # sudo su postgres -c 'createdb --encoding=UTF-8 --owner=hiverec --template=template0 hiverec'
    sudo su postgres -c 'EDITOR=emacs psql -U hiverec hiverec'
fi


uname -a | grep -q NixOS
if test $? -eq 0; then
  # createuser -U postgres --createdb --encrypted --no-inherit --login --pwprompt --no-createrole --no-superuser --no-replication hiverec
  # createdb -U postgres --encoding=UTF-8 --owner=hiverec --template=template0 hiverec
  psql -U hiverec hiverec
fi
