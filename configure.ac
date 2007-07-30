
AC_INIT(starparse, 0.9, [gbenison@gmail.com])
AC_CONFIG_SRCDIR([parser.y])

AC_PROG_CC
AC_PROG_LIBTOOL
AC_PROG_LEX
AC_PROG_YACC

AM_INIT_AUTOMAKE

AC_ARG_ENABLE(guile,
	      AS_HELP_STRING([--enable-guile],
                             [build bindings for guile scheme interpreter]),
              USE_GUILE=$enable_guile,
              USE_GUILE=yes)  # default value

if test "x$USE_GUILE" = "xyes";
then
  GUILE_FLAGS
fi

AM_CONDITIONAL(ENABLE_GUILE, test "x$USE_GUILE" = "xyes")

AC_CONFIG_FILES([Makefile
                 test/Makefile
                 example/Makefile])

AC_OUTPUT



