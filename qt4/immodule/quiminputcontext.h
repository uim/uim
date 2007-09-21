/*
Copyright (C) 2004 Kazuki Ohta <mover@hct.zaq.ne.jp>
*/
#ifndef _QUIMINPUT_CONTEXT_H_
#define _QUIMINPUT_CONTEXT_H_

#include <qinputcontext.h>
#include <qevent.h>

#ifdef Q_WS_X11
#define UIM_QT_USE_JAPANESE_KANA_KEYBOARD_HACK 1
#endif

#include <uim/uim.h>
#include <uim/uim-helper.h>
#include <uim/uim-util.h>

class CandidateWindow;
class QUimHelperManager;
#ifdef Q_WS_X11
typedef struct _DefTree DefTree;
class Compose;
#endif
class QUimTextUtil;

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

#ifdef Q_WS_X11
    virtual bool x11FilterEvent( QWidget *keywidget, XEvent *event );
#endif
    virtual bool filterEvent( const QEvent *event );
    virtual void reset();
    virtual void update();
    virtual void mouseHandler( int x, QMouseEvent *event );
    virtual bool isComposing() const { return m_isComposing; }
    virtual void setFocusWidget( QWidget *w );

    virtual bool isPreeditRelocationEnabled();

    uim_context uimContext() { return m_uc; }

    static QUimInputContext *focusedIC();
    static void reloadUim();

    void commitString( const QString& str );

    void readIMConf();

    QUimTextUtil *textUtil() { return mTextUtil; }

    QString getPreeditString();
    int getPreeditCursorPosition();

    void saveContext();
    void restoreContext();

protected:
    uim_context createUimContext( const char *imname );
    virtual bool isPreeditPreservationEnabled();  // not a QInputContext func
    virtual void setFocus();    // not a QInputContext func
    virtual void unsetFocus();  // not a QInputContext func

private:
    void setMicroFocus( int x, int y, int w, int h, QFont *f = 0 );
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
    //imsw
    static void switch_app_global_im_cb( void *ptr, const char *str );
    static void switch_system_global_im_cb( void *ptr, const char *str );
    /* real functions for callbacks (correspond order) */
    //preedit
    void clearPreedit();
    void pushbackPreeditString( int attr, const QString& str );
    void updatePreedit();
    //candidate
    void candidateActivate( int nr, int displayLimit );
    void candidateSelect( int index );
    void candidateDeactivate();
    //imsw
    void switch_app_global_im( const char *str );
    void switch_system_global_im( const char *str );

#ifdef Q_WS_X11
    // for X11 Compose
    static DefTree *mTreeTop;
    static void create_compose_tree( void );
    static char *get_compose_filename( void );
    static char *TransFileName( char *name );
    static void ParseComposeStringFile( FILE *fp );
    static void FreeComposeTree( DefTree *top );
    static int parse_compose_line( FILE *fp, char **tokenbuf, size_t *buflen );
    static int get_mb_string( char *buf, unsigned int ks );
    static const char *get_encoding( void );
    static char *get_lang_region( void );

    Compose *mCompose;
#endif
    QUimTextUtil *mTextUtil;

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

#endif /* Not def: _QUIMINPUT_CONTEXT_H_ */
