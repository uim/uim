/*
  Copyright (c) 2005-2010 uim Project http://code.google.com/p/uim/

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

#include "candidate.h"

candidate_info *
create_candidate()
{
  candidate_info *cand;

  cand = (candidate_info *)malloc(sizeof(candidate_info));
  cand->valid = 0;

  return cand;
}

int
new_candidate(uim_context context, candidate_info *cand, int num, int limit)
{
  int i;
  uim_candidate u_cand;

  if (cand->valid) clear_candidate(cand);

  cand->valid = 1;

  cand->index = -1;
  cand->disp_limit = limit;
  cand->num = num;

  cand->cand_array = (candidate *)malloc(sizeof(candidate) * num);

  /* get candidates from context */
  for (i = 0; i < num; i ++) {
	u_cand = uim_get_candidate(context, i, limit ? i % limit : i);
	cand->cand_array[i].str = strdup(uim_candidate_get_cand_str(u_cand));
	cand->cand_array[i].label = strdup(uim_candidate_get_heading_label(u_cand));
	uim_candidate_free(u_cand);
  }

  return 1;
}


int
show_candidate(candidate_info *cand)
{
  int i;
  int page;
  int index;

  if (cand->num == 0) {
	a_printf("( e )");
	return 0;
  }

  index = cand->index;
  if (cand->disp_limit)
    page = index / cand->disp_limit;
  else
    page = 0;

  a_printf("( c ");

  a_printf(" ( %d . %d ) ",  cand->index + 1,  cand->num);

  for (i = cand->disp_limit * page;
	   i < (cand->disp_limit ? cand->disp_limit * (page + 1) : cand->num);
	   i ++) {

	if (i >= cand->num) break;

	if (i == index)
	  a_printf("( t ");
	else
	  a_printf("( nil ");

	output_with_escape(cand->cand_array[i].label);

	a_putchar(' ');

	output_with_escape(cand->cand_array[i].str);

	a_printf(" ) ");
  }

  a_printf(") ");

  return 1;
}



void
clear_candidate(candidate_info *cand)
{
  int i;

  if (cand->valid) {

	cand->valid = 0;

	for (i = 0; i < cand->num; i ++) {
	  free(cand->cand_array[i].str);
	  free(cand->cand_array[i].label);
	}

	free(cand->cand_array);
  }
}



void
shift_candidate_page(uim_context context, candidate_info *cand, int direction)
{
  int index;
  int is_first = 0, is_last = 0;

  debug_printf(DEBUG_NOTE, 
			   "candidate_shift_page_cb (direction: %d)\n", direction);

  if (!cand->disp_limit)
    return;

  index = cand->index;
  
  is_first = (index < cand->disp_limit);

  /*
   * ((cand.num - 1) / cand.disp_limit) + 1   : total pages
   * (index / cand.disp_limit) + 1            : current page number
   */
  is_last = ((((cand->num - 1) / cand->disp_limit) + 1) 
			 == ((index / cand->disp_limit) + 1));
  
  if (direction) { /* forward */

	if (is_last)
	  index = index % cand->disp_limit;
	else
	  index += cand->disp_limit;

  } else { /* backward */

	if (is_first)
	  index = (cand->num / cand->disp_limit) * cand->disp_limit + index;
    else
	  index -= cand->disp_limit;
  }

  if (index < 0) index = 0;
  
  if (index >= cand->num) index = cand->num - 1;

  cand->index = index;

  uim_set_candidate_index(context, index);
}
