#! /bin/sh

autoheader
libtoolize --force
aclocal
automake --add-missing
autoconf
