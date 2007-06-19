/*
 * guile-1.6 compatible equivalents for functions found only
 * in guile-1.8
 */

#if (SCM_MINOR_VERSION == 6)

size_t str_len;
#define scm_to_locale_string(x)   gh_scm2newstr((x), &str_len)
#define scm_from_locale_string(x) gh_str02scm(x)
#define scm_from_locale_symbol(x) gh_symbol2scm(x)
#define scm_from_long(x)          gh_long2scm(x)
#define scm_from_double(x)        gh_double2scm(x)

#endif /* (SCM_MINOR_VERSION == 6) */

