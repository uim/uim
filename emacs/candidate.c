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

#include "candidate.h"

candidate_info *
create_candidate()
{
  candidate_info *cand;

  cand = uim_malloc(sizeof(candidate_info));
  cand->valid = 0;

  return cand;
}

#if UIM_EL_USE_NEW_PAGE_HANDLING
static int
set_page_candidates(uim_context context, candidate_info *cand)
{
  int i, nr_in_page, start;

  start = cand->page_index * cand->disp_limit;
  if (cand->disp_limit && ((cand->num - start) > cand->disp_limit))
    nr_in_page = cand->disp_limit;
  else
    nr_in_page = cand->num - start;

  for (i = start; i < (start + nr_in_page); i++) {
    uim_candidate u_cand;

    u_cand = uim_get_candidate(context, i, cand->disp_limit ?
					   i % cand->disp_limit :
					   i);
    free(cand->cand_array[i].str);
    free(cand->cand_array[i].label);
    cand->cand_array[i].str = uim_strdup(uim_candidate_get_cand_str(u_cand));
    cand->cand_array[i].label = uim_strdup(uim_candidate_get_heading_label(u_cand));
    uim_candidate_free(u_cand);
  }

  return 1;
}
#endif

int
new_candidate(uim_context context, candidate_info *cand, int num, int limit)
{
  int i;
#if !UIM_EL_USE_NEW_PAGE_HANDLING
  uim_candidate u_cand;
#endif

  if (cand->valid) clear_candidate(cand);

  cand->valid = 1;

  cand->index = -1;
  cand->disp_limit = limit;
  cand->num = num;
#if UIM_EL_USE_NEW_PAGE_HANDLING
  cand->page_index = 0;
#endif

  cand->cand_array = uim_malloc(sizeof(candidate) * num);

#if !UIM_EL_USE_NEW_PAGE_HANDLING
  /* get candidates from context */
  for (i = 0; i < num; i ++) {
	u_cand = uim_get_candidate(context, i, limit ? i % limit : i);
	cand->cand_array[i].str = uim_strdup(uim_candidate_get_cand_str(u_cand));
	cand->cand_array[i].label = uim_strdup(uim_candidate_get_heading_label(u_cand));
	uim_candidate_free(u_cand);
  }
#else
  for (i = 0; i < num; i++) {
	cand->cand_array[i].str = NULL;
	cand->cand_array[i].label = NULL;
  }
  set_page_candidates(context, cand);
#endif

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

#if UIM_EL_USE_NEW_PAGE_HANDLING
void
select_candidate(uim_context context, candidate_info *cand, int index)
{
  int new_page;

  new_page = cand->disp_limit ? index / cand->disp_limit : 0;

  cand->index = index;

  if (new_page != cand->page_index) {
    cand->page_index = new_page;
    set_page_candidates(context, cand);
  }
}
#endif

void
shift_candidate_page(uim_context context, candidate_info *cand, int direction)
{
  int index;
#if UIM_EL_USE_NEW_PAGE_HANDLING
  int page, nr_pages;
#endif
  int is_first = 0, is_last = 0;

  debug_printf(DEBUG_NOTE, 
			   "candidate_shift_page_cb (direction: %d)\n", direction);

  if (!cand->disp_limit)
    return;

  index = cand->index;
#if UIM_EL_USE_NEW_PAGE_HANDLING
  page = cand->page_index;
#endif
  
#if !UIM_EL_USE_NEW_PAGE_HANDLING
  is_first = (index < cand->disp_limit);

  /*
   * ((cand.num - 1) / cand.disp_limit) + 1   : total pages
   * (index / cand.disp_limit)                : current page number
   */
  is_last = ((((cand->num - 1) / cand->disp_limit) + 1) 
			 == ((index / cand->disp_limit) + 1));
#else
  is_first = (page == 0);
  nr_pages = ((cand->num - 1) / cand->disp_limit) + 1;
  is_last = (nr_pages == page);
#endif

  if (direction) { /* forward */

	if (is_last) {
	  index = index % cand->disp_limit;
#if UIM_EL_USE_NEW_PAGE_HANDLING
	  cand->page_index = 0;
#endif
	}
	else {
	  index += cand->disp_limit;
#if UIM_EL_USE_NEW_PAGE_HANDLING
	  cand->page_index++;
#endif
	}

  } else { /* backward */

	if (is_first) {
	  index = (cand->num / cand->disp_limit) * cand->disp_limit + index;
#if UIM_EL_USE_NEW_PAGE_HANDLING
	  cand->page_index = nr_pages - 1;
#endif
	}
    else {
	  index -= cand->disp_limit;
#if UIM_EL_USE_NEW_PAGE_HANDLING
	  cand->page_index--;
#endif
    }
  }

  if (index < 0) index = 0;
  
  if (index >= cand->num) index = cand->num - 1;

  cand->index = index;

#if UIM_EL_USE_NEW_PAGE_HANDLING
  set_page_candidates(context, cand);
#endif
  uim_set_candidate_index(context, index);
}
