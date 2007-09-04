/*
 *  Copyright (C) 2007 Greg Benison
 * 
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 * 
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 * 
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#include <errno.h>
#include <libguile.h>
#include "guile-compat.h"

static SCM ship_item_cb;

static void
guile_cb(char* name, char* value)
{
  SCM value_scm;
  char* tail;

  /* try as an integer */
  long int result = strtol(value, &tail, 0);
  if (!(errno) && (*tail == '\0'))
    value_scm = scm_from_long(result);
  else
    {
      /* try as a float */
      double result = strtod(value, &tail);
      if (*tail == '\0')
	value_scm = scm_from_double(result);
      else
	/* finally, a string */
	value_scm = scm_from_locale_string(value);
    }
  
  scm_call_2(ship_item_cb, scm_from_locale_symbol(name), value_scm);

}


#define FUNC_NAME "star-parse"
static SCM star_parse_guile (SCM fname_scm, SCM filter_string_scm, SCM ship_item_scm)
{
  char* filter_string = NULL;
  char* fname = "-";

  if (SCM_NFALSEP(filter_string_scm))
    filter_string = scm_to_locale_string(filter_string_scm);

  if (SCM_NFALSEP(fname_scm))
    fname = scm_to_locale_string(fname_scm);
  
  ship_item_cb = ship_item_scm;

  starparse(fname, filter_string, guile_cb);
  
  if (filter_string)
    free(filter_string);

}
#undef FUNC_NAME

void
starparse_init()
{
  scm_c_define_gsubr ("star-parse", 3, 0, 0, star_parse_guile);
}
