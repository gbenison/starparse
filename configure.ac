
AC_INIT(starparse, 0.9, [gbenison@gmail.com])
AC_CONFIG_SRCDIR([src/parser.y])

AC_PROG_CC
AC_PROG_LIBTOOL
AC_PROG_LEX
AC_PROG_YACC

AM_INIT_AUTOMAKE

ENABLE_GUILE=no  # default value
AC_ARG_ENABLE(guile,
	      AS_HELP_STRING([--enable-guile],
                             [build bindings for guile scheme interpreter (default no)]),
              ENABLE_GUILE=yes, ENABLE_GUILE=no)
if test "x$ENABLE_GUILE" = "xyes";
then
  GUILE_FLAGS
fi
# AC_SUBST(ENABLE_GUILE)
AM_CONDITIONAL(ENABLE_GUILE, test "x$ENABLE_GUILE" = "xyes")

AC_CONFIG_FILES([Makefile
                 src/Makefile
                 example/Makefile])

AC_OUTPUT



