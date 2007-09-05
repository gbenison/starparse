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
 * grammar definition from: J. Chem. Inf. Comput. Sci. 1994, 34, 505-508
 *
 */


%{
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <assert.h>
#include <regex.h>
#include "starparse.h"
#define YYSTYPE char*

  extern int yylineno;

  char*
    current_line()
    {
      #define MSG_MAX_LENGTH 128
      static char msg[MSG_MAX_LENGTH];
      snprintf(msg, MSG_MAX_LENGTH, "line %d", yylineno);
      return msg;
    }

  int loop_nest_lvl = 0;

  starparse_error_handler_t starparse_error;

  struct _loop_struct {
    char* value;
    int nest_lvl;
    struct _loop_struct* next;
    struct _loop_struct* reenter_point;
  };
  typedef struct _loop_struct loop_t;
  loop_t* loop_base = NULL;
  /*  loop_t* loop_reenter = NULL; */
  loop_t* loop_cur = NULL;

  static ship_item_cb_t ship_item_cb = NULL;

  static regex_t re;

  static void
   _ship_item(char* name, char* value)
    {
      if (ship_item_cb)
	if ((regexec(&re, name, 0, NULL, 0)) == 0)
          ship_item_cb(name, value);
    }


  static void
  default_error_handler(char* msg)
    {
      fprintf(stderr, msg);
      exit(1);
    }

void
throw_error(const char* msg1, const char* msg2)
{
  char* fmt = "starparse: %s: %s";
  char msg[strlen(fmt) + strlen(msg1) + strlen(msg2)];
  sprintf(msg, fmt, msg1, msg2);
  starparse_error(msg);
}

#define SHIP_ITEM(_a_, _b_) {_ship_item((_a_), (_b_));}

  static void default_ship_item(char* name, char* value)
    {
      printf ("%s: %s\n", name, value);
    }

  void loop_push(char* str)
    {
      loop_t* new = (loop_t*)malloc(sizeof(loop_t));
      new->value = str;
      new->nest_lvl = loop_nest_lvl;
      new->next = loop_base;
      loop_base = new;
    }

  loop_t* next_in_lvl(loop_t* start)
    {
      int start_lvl = start->nest_lvl;
      start = start->next;
      while (1)
	{
	  if (start == NULL)
	    return NULL;
	  if (start->nest_lvl == start_lvl)
	    return start;
	  start = start->next;
	}
      /* UNREACHABLE */
    }

  void normalize_reenter(loop_t* start, loop_t* cur, loop_t* reenter_point)
    {
      loop_t* next = cur->next;

      cur->reenter_point = reenter_point;

      if (next == NULL)
	return;

      if (next->nest_lvl == cur->nest_lvl)
	normalize_reenter(start, next, reenter_point);
      else if (next->nest_lvl > cur->nest_lvl)
	{
	  loop_t* new_reenter_point = next_in_lvl(cur);
	  if (new_reenter_point != NULL)
	    normalize_reenter(new_reenter_point, new_reenter_point, reenter_point);
	  else
	    new_reenter_point = start;
	  normalize_reenter(next, next, new_reenter_point);
	}
    }

  void circularize(loop_t* start, loop_t* cur)
    {
      loop_t* next = cur->next;
      
      if (next == NULL)
	cur->next = start;
      else
	{
	  if (next->nest_lvl == cur->nest_lvl)
	    circularize(start, next);
	  else
	    if (next->nest_lvl < start->nest_lvl)
	      cur->next = start;
	    else
	      if (next->nest_lvl > start->nest_lvl)
		{
		  circularize(next, next);
		  if (next->reenter_point != start)
		    circularize(start, next->reenter_point);
		}
	}
    }

  void loop_compile()
    {
      /* reverse loop order */
      loop_t* tmp = NULL;
      while (loop_base != NULL)
	{
	  loop_t* next = loop_base->next;
	  loop_base->next = tmp;
	  tmp = loop_base;
	  loop_base = next;
	}
      loop_base = tmp;

      normalize_reenter(loop_base, loop_base, NULL);
      circularize(loop_base, loop_base);

      loop_cur = loop_base;
    }

  char* loop_grab_tag()
    {
      if (loop_cur == NULL)
	throw_error("misformatted loop", current_line());
      assert(loop_cur != NULL);
      char* result = loop_cur->value;
      loop_cur = loop_cur->next;
      return result;
    }

  void loop_pop_level()
    {
      loop_cur = loop_cur->reenter_point;
    }

  void yyerror (char const *s)
    {
      fprintf (stderr, "%s\n", s);
    }

%}

%debug
%defines

%token SAVE_HEADING
%token SAVE_FRAME_STOP
%token DATA_HEADING
%token GLOBAL_HEADING
%token DATA_NAME
%token LOOP_START
%token LOOP_STOP
%token DATA_ITEM
%token TEXT_STRING

%%

star_file:  /* empty */
            | star_file data_block 
            | star_file global_block

data_block: DATA_HEADING data_block_body

global_block: GLOBAL_HEADING global_block_body

data_block_body: /* empty */
                 | data_block_body data
                 | data_block_body save_frame

global_block_body: /* empty */
                   | global_block_body data

data:  DATA_NAME data_value {SHIP_ITEM($1, $2);}
       | error { throw_error("syntax error", current_line()); }
       | data_loop

save_frame: SAVE_HEADING save_frame_body SAVE_FRAME_STOP

save_frame_body: /* empty */
                 | save_frame_body data

loop_start_tag: LOOP_START {++loop_nest_lvl;}

data_loop:  loop_start_tag data_loop_definition_top data_loop_values { loop_base = NULL; loop_cur = NULL; loop_nest_lvl = 0; }

data_loop_definition_top: data_loop_definition { loop_compile(); }

data_loop_definition: /* empty */
                  | data_loop_definition data_loop_field 

data_loop_field: DATA_NAME {loop_push(yylval);}
                 | nested_loop

nested_loop: loop_start_tag data_loop_definition LOOP_STOP {--loop_nest_lvl;}
             | loop_start_tag data_loop_definition {--loop_nest_lvl;}

data_loop_values: /* empty */
                  | data_loop_values data_loop_item

data_loop_item: data_value {SHIP_ITEM(loop_grab_tag(), yylval);}
               | LOOP_STOP {loop_pop_level();}

data_value: DATA_ITEM
            | TEXT_STRING

%%

extern FILE* yyin;
extern int yy_flex_debug;

void
starparse(const char* fname, const char* filter, ship_item_cb_t ship_item, starparse_error_handler_t error_handler)
{
  if (getenv("DEBUG_LEXER"))
      yy_flex_debug = 1;
  else
      yy_flex_debug = 0;
  if (getenv("DEBUG_PARSER"))
      yydebug = 1;
  else
      yydebug = 0;
  if (error_handler == NULL)
    starparse_error = default_error_handler;
  else
    starparse_error = error_handler;
  if (strcmp(fname, "-") != 0)
    {
      yyin = fopen(fname, "r");
      if (!yyin)
	throw_error(fname, strerror(errno));
      assert(yyin);
    }
  ship_item_cb = ship_item;

  const char* re_string = filter ? filter : ".*";
  regcomp(&re, re_string, REG_EXTENDED);

  yyparse();

  fclose(yyin);
}




