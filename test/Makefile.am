
TESTS_ENVIRONMENT = GUILE_LOAD_PATH=$(top_srcdir)/modules:$GUILE_LOAD_PATH

TESTS = \
    file-not-found.test \
    file-not-found-xfail.test \
    malformed.test \
    malformed-xfail.test \
    single-quote.test

XFAIL_TESTS = \
    file-not-found-xfail.test \
    malformed-xfail.test

EXTRA_DIST = $(TESTS) $(XFAIL_TESTS) single-quote.str malformed.str
