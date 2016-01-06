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

#include "preedit.h"

preedit *
create_preedit()
{
  preedit *pe;
  pe = uim_malloc(sizeof(preedit));
  pe->valid = 0;
  pe->head = pe->tail = NULL;

  return pe;
}

void
add_preedit(preedit *pe, int attr, const char *str)
{
  preedit_buffer *pb;

  pe->valid = 1;

  pb = uim_malloc(sizeof(preedit_buffer));

  if (pe->head == NULL) {
	pe->head = pb;
	pe->tail = pb;
  } else {
	pe->tail->next = pb;
	pe->tail = pb;
  }

  if (strlen(str) > 0) {
	pb->str = uim_strdup(str);
	pe->length += strlen(str);
  } else {
	pb->str = NULL;
  }

  pb->attr = attr;
  pb->next = NULL;
}


void
clear_preedit(preedit *pe)
{
  preedit_buffer *p, *ptmp;

  pe->valid = 0;

  p = pe->head;

  while (p) {
	ptmp = p;
	p = p->next;
	free(ptmp->str);
	free(ptmp);
  }

  pe->head = pe->tail = NULL;
  pe->length = 0;
}



int
show_preedit(preedit *pe)
{
  preedit_buffer *p;

  p = pe->head;

  if (p == NULL || pe->length == 0) {
	a_printf(" ( e ) ");
	return 0;
  }

  a_printf("( p ");

  while (p) {
	a_printf(" ( ");
	if (p->attr & UPreeditAttr_Reverse)
	  a_putchar('r');
	if (p->attr & UPreeditAttr_UnderLine) 
	  a_putchar('u');
	if (p->attr & UPreeditAttr_Cursor) 
	  a_putchar('c');
	if (p->attr & UPreeditAttr_Separator)
	  a_putchar('s');

	a_putchar('t');
	a_putchar(' ');

	output_with_escape(p->str);

	a_printf(" ) ");

	p = p->next;
  }
  a_printf(" ) ");

  return 1;
}



int
show_preedit_force(preedit *pe)
{
  if (! show_preedit(pe))
	a_printf("( p (t \"\") ) ");

  return 1;
}
