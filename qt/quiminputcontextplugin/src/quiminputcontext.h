/*

Copyright (c) 2003,2004 uim Project http://uim.freedesktop.org/

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

THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
SUCH DAMAGE.

*/
#ifndef _QUIMINPUT_CONTEXT_H_
#define _QUIMINPUT_CONTEXT_H_

#include <qinputcontext.h>
#include <qptrlist.h>

#include <uim/uim.h>
#include <uim/uim-util.h>
#include <uim/uim-helper.h>

class CandidateWindow;
class QUimHelperManager;

class PreeditSegment
{
public:
    PreeditSegment( int p_attr, const QString &p_str )
    {
        attr = p_attr;
        str = p_str;
    }

    int attr;
    QString str;
};

class QUimInputContext : public QInputContext
{
    Q_OBJECT
public:
    QUimInputContext( const char *imname = 0, const char *lang = 0 );
    ~QUimInputContext();

    virtual QString identifierName();
    virtual QString language();

    virtual bool filterEvent( const QEvent *event );
    virtual void reset();
    virtual void setFocus();
    virtual void unsetFocus();
    virtual void setMicroFocus( int x, int y, int w, int h, QFont *f = 0 );
    virtual void mouseHandler( int x, QEvent::Type type,
                               Qt::ButtonState button, Qt::ButtonState state );
    virtual bool isPreeditRelocationEnabled();

    uim_context uimContext() { return m_uc; }

    static QUimInputContext *focusedIC();

    void commitString( const QString& str );

    void readIMConf();

protected:
    uim_context createUimContext( const char *imname );
    virtual bool isPreeditPreservationEnabled();  // not a QInputContext func

    void createUimInfo();
private:
    QString getPreeditString();
    int getPreeditCursorPosition();
    int getPreeditSelectionLength();

    /* callbacks for uim */
    static void commit_cb( void *ptr, const char *str );
    //preedit
    static void clear_cb( void *ptr );
    static void pushback_cb( void *ptr, int attr, const char *str );
    static void update_cb( void *ptr );
    //candidate
    static void cand_activate_cb( void *ptr, int nr, int displayLimit );
    static void cand_select_cb( void *ptr, int index );
    static void cand_shift_page_cb( void* ptr, int index );
    static void cand_deactivate_cb( void *ptr );
    /* real functions for callbacks (correspond order) */
    //preedit
    void clearPreedit();
    void pushbackPreeditString( int attr, const QString& str );
    void updatePreedit();
    //candidate
    void candidateActivate( int nr, int displayLimit );
    void candidateSelect( int index );
    void candidateDeactivate();

protected:
    QString m_imname;
    QString m_lang;
    uim_context m_uc;
    bool candwinIsActive;

    QPtrList<PreeditSegment> psegs;
    QString preeditString;

    CandidateWindow *cwin;
    static QUimHelperManager *m_HelperManager;
};

struct UIMInfo
{
    const char *lang;
    const char *name;
    const char *short_desc;
};

#endif /* Not def: _QUIMINPUT_CONTEXT_H_ */
