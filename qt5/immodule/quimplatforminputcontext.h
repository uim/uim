/*

  Copyright (c) 2012-2013 uim Project https://github.com/uim/uim

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

#ifndef UIM_QT5_IMMODULE_QUIMPLATFORMINPUTCONTEXT_H
#define UIM_QT5_IMMODULE_QUIMPLATFORMINPUTCONTEXT_H

#include <QtGui/QInputMethodEvent>

#include <qpa/qplatforminputcontext.h>

#include <uim.h>

class CandidateWindowProxy;
class QUimTextUtil;
class QUimHelperManager;

struct PreeditSegment
{
    PreeditSegment(int attr, const QString &str) {
        this->attr = attr;
        this->str = str;
    }
    int attr;
    QString str;
};

class QUimPlatformInputContext : public QPlatformInputContext
{
    Q_OBJECT

public:
    explicit QUimPlatformInputContext(const char *imname = 0);
    ~QUimPlatformInputContext();


    virtual void commit();
    virtual bool filterEvent(const QEvent *event);
    virtual void hideInputPanel();
    virtual Qt::LayoutDirection inputDirection() const;
    virtual void invokeAction(QInputMethod::Action action, int cursorPosition);
    virtual bool isAnimating() const { return m_isAnimating; }
    virtual bool isInputPanelVisible() const;
    virtual bool isValid() const;
    virtual QRectF keyboardRect() const;
    virtual QLocale locale() const;
    virtual void reset();
    virtual void showInputPanel();
    virtual void update(Qt::InputMethodQueries);

    uim_context uimContext() { return m_uc; }

    void commitString(const QString& str);

    void updatePosition();
    void updateStyle();

    QUimTextUtil *textUtil() { return m_textUtil; }

    QString getPreeditString();
    int getPreeditCursorPosition();

    void saveContext();
    void restoreContext();

    void updateIndicator(const QString &str);

    void setCandwinActive() { candwinIsActive = true; }

private:
    uim_context createUimContext(const char *imname);
    void createCandidateWindow();
    void setFocusObject(QObject *object);
    void setFocus();
    void unsetFocus();

    QList<QInputMethodEvent::Attribute> getPreeditAttrs();

    void update();

    /// callbacks for uim
    static void commit_cb(void *ptr, const char *str);
    // preedit
    static void clear_cb(void *ptr);
    static void pushback_cb(void *ptr, int attr, const char *str);
    static void update_cb(void *ptr);
    // candidate
    static void cand_activate_cb(void *ptr, int nr, int displayLimit);
    static void cand_select_cb(void *ptr, int index);
    static void cand_shift_page_cb(void* ptr, int index);
    static void cand_deactivate_cb(void *ptr);
    // imsw
    static void switch_app_global_im_cb(void *ptr, const char *str);
    static void switch_system_global_im_cb(void *ptr, const char *str);
    // delay
    static void cand_activate_with_delay_cb(void *ptr, int delay);
    /// real functions for callbacks (correspond order)
    // preedit
    void clearPreedit();
    void pushbackPreeditString(int attr, const QString& str);
    void updatePreedit();
    // imsw
    void switch_app_global_im(const char *str);
    void switch_system_global_im(const char *str);

    QUimTextUtil *m_textUtil;
    bool candwinIsActive;
    bool m_isAnimating;

    uim_context m_uc;
    QList<PreeditSegment> preeditSegments;
    CandidateWindowProxy *proxy;

    static QUimHelperManager *m_helperManager;
};

#endif /* Not def: UIM_QT5_IMMODULE_QUIMPLATFORMINPUTCONTEXT_H */
