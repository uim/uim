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
#ifndef UIM_QT4_IMMODULE_QUIMINPUTCONTEXT_COMPOSE_H
#define UIM_QT4_IMMODULE_QUIMINPUTCONTEXT_COMPOSE_H

#include <X11/X.h>
#undef Above
#undef Below
#undef CursorShape
#undef FocusIn
#undef FocusOut
#undef FontChange
#undef KeyPress
#undef KeyRelease
#undef None

class QKeyEvent;

typedef struct _DefTree {
    struct _DefTree *next;                /* another Key definition */
    struct _DefTree *succession;        /* successive Key Sequence */
                                        /* Key definitions */
    unsigned modifier_mask;
    unsigned modifier;
    KeySym keysym;                        /* leaf only */
    char *mb;
    char *utf8;                                /* make from mb */
    KeySym ks;
} DefTree;

#include <QtCore/QtGlobal>

#if QT_VERSION < 0x050000
class QUimInputContext;
#else
class QUimPlatformInputContext;
#endif
class Compose {
public:
#if QT_VERSION < 0x050000
    Compose(DefTree *, QUimInputContext *);
#else
    Compose(DefTree *, QUimPlatformInputContext *);
#endif
    ~Compose();
    bool handle_qkey(const QKeyEvent *event);
    void reset();
private:
    bool handleKey(KeySym xkeysym, int xstate, bool is_push);
#if QT_VERSION < 0x050000
    QUimInputContext *m_ic;
#else
    QUimPlatformInputContext *m_ic;
#endif
    DefTree *m_top;
    DefTree *m_context;
    DefTree *m_composed;
};

#endif
/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 */
