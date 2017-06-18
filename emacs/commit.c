/*
  Copyright (c) 2005-2013 uim Project https://github.com/uim/uim

  All rights reserved.

  Redistribution and use in source and binary forms, with or
  without modification, are permitted provided that the
  following conditions are met:

  1. Redistributions of source code must retain the above
     copyright notice, this list of conditions and the
     following disclaimer.
  2. Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the
     following disclaimer in the documentation and/or other
     materials provided with the distribution.
  3. Neither the name of authors nor the names of its
     contributors may be used to endorse or promote products
     derived from this software without specific prior written
     permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
  CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "commit.h"

char *
add_commit_string(char *comstr, const char *str)
{
  int buflen;

  debug_printf(DEBUG_NOTE,
			   "add_commit_string str: %s\n", str); 

  buflen = (comstr != (char *)NULL) ? strlen(comstr) : 0;
  
  if (str != (const char *)NULL) {

	debug_printf(DEBUG_NOTE,
				 "add_commit_string comstr: len:%d (%p)\n", 
				 buflen + strlen(str) + 1, comstr); 

	comstr = uim_realloc(comstr, buflen + strlen(str) + 1);
	comstr[buflen] = '\0';

	debug_printf(DEBUG_NOTE,
				 "add_commit_string comstr: %s (%p)\n", comstr, comstr); 

	strlcat(comstr, str, buflen + strlen(str) + 1);
  }

  debug_printf(DEBUG_NOTE,
			   "add_commit_string comstr: %s (%p)\n", comstr, comstr); 

  return comstr;
}


int
show_commit_string(char *comstr)
{
  if (!comstr) return -1;

  a_printf("( s ");
  output_with_escape(comstr);
  a_printf(" ) ");
  return 1;
}


void
reset_commit_string(char *comstr)
{
  debug_printf(DEBUG_NOTE,
			   "reset_commit_string comstr: %p\n", comstr); 

  free(comstr);
}

