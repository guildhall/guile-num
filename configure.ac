AC_INIT
AM_INIT_AUTOMAKE("guile-numerics", "0.4.0")
AC_PREREQ(2.54)
AC_CONFIG_SRCDIR("test_gsl_blas.scm")
AC_CONFIG_HEADERS

AM_CONFIG_HEADER(config.h)
AM_MAINTAINER_MODE
AC_DISABLE_STATIC

AC_ISC_POSIX
AC_PROG_CC
AC_STDC_HEADERS
AC_PROG_CPP
AC_PROG_F77
AC_F77_LIBRARY_LDFLAGS
AC_PROG_LIBTOOL

AC_MSG_CHECKING(for Guile)
guile-config link > /dev/null || {
 echo "configure: cannot find guile-config; is Guile installed?" 1>&2
 exit 1
}
GUILE_VERSION=`guile-config info guileversion`
if test "$GUILE_VERSION" \< 1.6.4; then
  AC_MSG_ERROR([Guile 1.6.4 or newer is required, but you only have $GUILEVERSION.])
fi
GUILE_CFLAGS="`guile-config compile`"
GUILE_LIBS="`guile-config link`"
AC_SUBST(GUILE_CFLAGS)
AC_SUBST(GUILE_LIBS)
AC_MSG_RESULT(yes)


CFLAGS="$CFLAGS `gsl-config --cflags`"
LIBS="$LIBS `gsl-config --libs`"

CFLAGS="$CFLAGS `guile-config compile`"
LIBS="$LIBS `guile-config link`"

dnl See also top_builddir in info node: (libtool)AC_PROG_LIBTOOL
top_builddir_absolute=`pwd`
AC_SUBST(top_builddir_absolute)
top_srcdir_absolute=`(cd $srcdir && pwd)`
AC_SUBST(top_srcdir_absolute)

AC_CONFIG_FILES([Makefile finance/Makefile fftw/Makefile gsl/Makefile
		 lapack/Makefile math/Makefile plot/Makefile
		 signal/Makefile sound/Makefile testfun/Makefile])
AC_CONFIG_FILES([pre-inst-guile], [chmod +x pre-inst-guile])
AC_OUTPUT
