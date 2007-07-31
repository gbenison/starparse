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

%x double_quote_string
%x single_quote_string
%x semicolon_string

%option noyywrap
%option yylineno

%{
#define YYSTYPE char*
#include <stdlib.h>
#include "parser.h"
char* _string_buf = NULL;
char* _string_buf_ptr = NULL;
int string_max = 0;
void push_char(char c)
{
   int offset = _string_buf_ptr - _string_buf;
   if (offset >= string_max)
   {
     if (string_max == 0) string_max = 16;
     string_max *= 2;
     _string_buf = realloc(_string_buf, string_max);
     _string_buf_ptr = _string_buf + offset;
   }
   *_string_buf_ptr++ = c;
}
char* string_buf() {if (_string_buf_ptr) *_string_buf_ptr = '\0'; return _string_buf;}
char* reset_string_buf() {_string_buf_ptr = _string_buf;}
%}

WHITESPACE           [ \t\n]
NONWHITESPACE        [^ \t\n]

%%

#.*\n                         /* gobble comments */

loop_                         return LOOP_START;
stop_                         return LOOP_STOP;

save_                         return SAVE_FRAME_STOP;
save_{NONWHITESPACE}+         yylval = strdup(yytext + 5); return SAVE_HEADING;
data_{NONWHITESPACE}+         yylval = strdup(yytext + 5); return DATA_HEADING;
_{NONWHITESPACE}+             yylval = strdup(yytext + 1); return DATA_NAME;

[^_\"\'; \t\n]{NONWHITESPACE}*  yylval = strdup(yytext); return DATA_ITEM;

\'      reset_string_buf(); BEGIN(single_quote_string);
<single_quote_string>\\(.|\n)     push_char(yytext[1]);
<single_quote_string>[^\\\n\']+  {
          char *yptr = yytext;
          while ( *yptr )
                  push_char(*yptr++);
        }

<single_quote_string>\'   {
        BEGIN(INITIAL);
        push_char('\0');
        yylval = strdup(string_buf());
        return TEXT_STRING;
     }

\"      reset_string_buf(); BEGIN(double_quote_string);

<double_quote_string>\"        { /* saw closing quote - all done */
        BEGIN(INITIAL);
        push_char('\0');
        /* return string constant token type and
         * value to parser
         */
        yylval = strdup(string_buf());
        return TEXT_STRING;
        }

<double_quote_string>\n        {
        /* FIXME error - unterminated string constant */
        }

<double_quote_string>\\[0-7]{1,3} {
        /* octal escape sequence */
        int result;

        (void) sscanf( yytext + 1, "%o", &result );

        if ( result > 0xff )
                /* error, constant is out-of-bounds */

        push_char(result);
        }

<double_quote_string>\\[0-9]+ {
        /* generate error - bad escape sequence; something
         * like '\48' or '\0777777'
         */
        }

<double_quote_string>\\n  push_char('\n');
<double_quote_string>\\t  push_char('\t');
<double_quote_string>\\r  push_char('\r');
<double_quote_string>\\b  push_char('\b');
<double_quote_string>\\f  push_char('\f');

<double_quote_string>\\(.|\n)    push_char(yytext[1]);

<double_quote_string>[^\\\n\"]+        {
        char *yptr = yytext;

        while ( *yptr )
                push_char(*yptr++);
        }

\n;                            { 
                                 BEGIN(semicolon_string);
                                 reset_string_buf();
                                 int i;
                                 for (i = 2; i < strlen(yytext); ++i)
                                    push_char(yytext[i]);
                               }
<semicolon_string>[^;]         {push_char(yytext[0]);}
<semicolon_string>\n;[ \n]*\n   { 
                                 BEGIN(INITIAL);
                                 yylval = strdup(string_buf());
                                 return TEXT_STRING;
                               }


{WHITESPACE} /* gobble */
.            printf ("@"); /* gobble unmatched chars */ 


