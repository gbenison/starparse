
SUBDIRS = . src example test

if ENABLE_GUILE
guiledatadir=$(datadir)/guile/site
dist_guiledata_DATA = modules/starparse.scm
endif

pkgconfigdir=$(libdir)/pkgconfig
pkgconfig_DATA=starparse.pc

bin_SCRIPTS=starparse-env
