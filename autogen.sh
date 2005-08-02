#! /bin/sh

chmod +x destill.scm
libtoolize --force
autoheader
aclocal
automake --add-missing
autoconf
