/*
Copyright (C) 2004 Kazuki Ohta <mover@hct.zaq.ne.jp>
*/
#ifndef _QUIMINPUT_CONTEXT_H_
#define _QUIMINPUT_CONTEXT_H_

#include <qinputcontext.h>
#include <qevent.h>

#include <uim/uim.h>
#include <uim/uim-helper.h>
#include <uim/uim-util.h>

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
    virtual void update();
    virtual void setFocus();
    virtual void unsetFocus();
    virtual void mouseHandler( int x, QMouseEvent *event );
    virtual bool isComposing() const { return m_isComposing; }

    virtual bool isPreeditRelocationEnabled();

    uim_context uimContext() { return m_uc; }

    static QUimInputContext *focusedIC();

    void readIMConf();

protected:
    uim_context createUimContext( const char *imname );
    virtual bool isPreeditPreservationEnabled();  // not a QInputContext func

    void createUimInfo();
private:
    void setMicroFocus( int x, int y, int w, int h, QFont *f = 0 );
    QString getPreeditString();
    int getPreeditCursorPosition();
    int getPreeditSelectionLength();
    QList<QInputMethodEvent::Attribute> getPreeditAttrs();

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
    void commitString( const QString& str );
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
    bool m_isComposing;

    QList<PreeditSegment*> psegs;

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
