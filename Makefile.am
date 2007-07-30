
SUBDIRS = example test

lib_LTLIBRARIES=libstarparse.la

libstarparse_la_SOURCES =\
	parser.y lexer.l starparse.h

libstarparse_la_CFLAGS =
libstarparse_la_LDFLAGS = 

if ENABLE_GUILE
libstarparse_la_SOURCES += guile-binding.c
libstarparse_la_CFLAGS += @GUILE_CFLAGS@
libstarparse_la_LDFLAGS += @GUILE_LDFLAGS@ -lpthread
guiledatadir=$(datadir)/guile/site
dist_guiledata_DATA = modules/starparse.scm
endif

greg:
	echo @GUILE_LDFLAGS@
