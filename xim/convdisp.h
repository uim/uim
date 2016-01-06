/*

  Copyright (c) 2003-2013 uim Project https://github.com/uim/uim

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.
  3. Neither the name of authors nor the names of its contributors
     may be used to endorse or promote products derived from this software
     without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
  SUCH DAMAGE.
*/

// -*- C++ -*-
#ifndef UIM_XIM_CONVDISP_H
#define UIM_XIM_CONVDISP_H

#include "ximserver.h"
#include <vector>

class icxatr;

class Convdisp {
public:
    Convdisp(InputContext *, icxatr *);
    virtual ~Convdisp();
    void set_pe(pe_stat *);
    uString get_pe();
    void set_focus();
    void unset_focus();
    InputContext *get_context();
    int get_caret_pos();
    void update_caret_state();
    virtual void update_preedit() = 0;
    virtual void clear_preedit() = 0;
    virtual void update_icxatr() = 0;
    virtual void move_candwin() = 0;
    virtual void set_im_lang(const char *im_lang);
    virtual void set_locale_name(const char *locale);
    virtual const char *get_locale_name();
    virtual bool use_xft() = 0;

protected:
    // Owner of mKkContext is XimIC. This is set at the time of
    // construct and is destructed before deletion of convdisp by
    // XimIC.
    InputContext *mKkContext;
    icxatr *m_atr;
    pe_stat *m_pe; // Initially NULL.
    const char *mIMLang;
    const char *mEncoding;
    const char *mLocaleName;
};

Convdisp *create_convdisp(int style, InputContext *, icxatr *, Connection *);
XFontSet get_font_set(const char *name, const char *locale);

#endif
/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 */
