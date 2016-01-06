/*

Copyright (c) 2006-2013 uim Project https://github.com/uim/uim

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

#include "qtextutil.h"

#include <cstdlib>

#include <QtGui/QClipboard>
#if QT_VERSION < 0x050000
# include <QtGui/QApplication>
# include <QtGui/QLineEdit>
# include <QtGui/QTextEdit>
# ifdef ENABLE_QT4_QT3SUPPORT
#  include <Qt3Support/Q3TextEdit>
# endif
#else
# include <QtWidgets/QApplication>
# include <QtWidgets/QLineEdit>
# include <QtWidgets/QTextEdit>
#endif

#if QT_VERSION < 0x050000
# include "quiminputcontext.h"
#else
# include "quimplatforminputcontext.h"
#endif

QUimTextUtil::QUimTextUtil( QObject *parent )
        : QObject( parent )
{
#if QT_VERSION < 0x050000
    mIc = static_cast<QUimInputContext *>( parent );
#else
    mIc = static_cast<QUimPlatformInputContext *>( parent );
#endif
    mPreeditSaved = false;
}

QUimTextUtil::~QUimTextUtil()
{
}

int
QUimTextUtil::acquire_text_cb( void *ptr, enum UTextArea text_id,
                               enum UTextOrigin origin,
                               int former_req_len, int latter_req_len,
                               char **former, char **latter )
{
    int err;
#if QT_VERSION < 0x050000
    QUimInputContext *ic = static_cast<QUimInputContext *>( ptr );
#else
    QUimPlatformInputContext *ic
        = static_cast<QUimPlatformInputContext *>( ptr );
#endif
    QUimTextUtil *tu = ic->textUtil();

    switch ( text_id ) {
    case UTextArea_Primary:
        err = tu->acquirePrimaryText( origin, former_req_len, latter_req_len,
                                      former, latter );
        break;
    case UTextArea_Selection:
        err = tu->acquireSelectionText( origin, former_req_len, latter_req_len,
                                        former, latter );
        break;
    case UTextArea_Clipboard:
        err = tu->acquireClipboardText( origin, former_req_len, latter_req_len,
                                        former, latter );
        break;
    case UTextArea_Unspecified:
    default:
        err = -1;
    }

    return err;
}

int
QUimTextUtil::delete_text_cb( void *ptr, enum UTextArea text_id,
                              enum UTextOrigin origin,
                              int former_req_len, int latter_req_len )
{
    int err;
#if QT_VERSION < 0x050000
    QUimInputContext *ic = static_cast<QUimInputContext *>( ptr );
#else
    QUimPlatformInputContext *ic
        = static_cast<QUimPlatformInputContext *>( ptr );
#endif
    QUimTextUtil *tu = ic->textUtil();

    switch ( text_id ) {
    case UTextArea_Primary:
        err = tu->deletePrimaryText( origin, former_req_len, latter_req_len );
        break;
    case UTextArea_Selection:
        err = tu->deleteSelectionText( origin, former_req_len, latter_req_len );
        break;
    case UTextArea_Clipboard:
    case UTextArea_Unspecified:
    default:
        err = -1;
        break;
    }

    return err;
}

int
QUimTextUtil::acquirePrimaryText( enum UTextOrigin origin,
                                  int former_req_len, int latter_req_len,
                                  char **former, char **latter )
{
    int err;
#if defined(Q_WS_X11)
    mWidget = QApplication::focusWidget();
#else
    return -1;
#endif

    if ( qobject_cast<QLineEdit *>( mWidget ) )
        err = acquirePrimaryTextInQLineEdit( origin, former_req_len,
                                             latter_req_len, former, latter );
    else if ( qobject_cast<QTextEdit *>( mWidget ) )
        err = acquirePrimaryTextInQTextEdit( origin, former_req_len,
                                             latter_req_len, former, latter );
#ifdef ENABLE_QT4_QT3SUPPORT
    else if ( qobject_cast<Q3TextEdit *>( mWidget ) )
        err = acquirePrimaryTextInQ3TextEdit( origin, former_req_len,
                                              latter_req_len, former, latter );
#endif
    else
        // FIXME other widgets?
        err = -1;

    return err;
}

int
QUimTextUtil::acquirePrimaryTextInQLineEdit( enum UTextOrigin origin,
                                             int former_req_len,
                                             int latter_req_len,
                                             char **former, char **latter )
{
    QLineEdit *edit = static_cast<QLineEdit *>( mWidget );
    QString text, former_text, latter_text;
    int cursor_index, len, precedence_len, following_len, offset;
    int preedit_len, preedit_cursor_pos;

    preedit_len = mIc->getPreeditString().length();
    preedit_cursor_pos = mIc->getPreeditCursorPosition();

    text = edit->text(); // excluding preedit string
    len = text.length();
    cursor_index = edit->cursorPosition() + preedit_len;

    precedence_len = cursor_index - preedit_cursor_pos;
    following_len = len - precedence_len;

    switch ( origin ) {
    case UTextOrigin_Cursor:
        offset = 0;
        if ( former_req_len >= 0 ) {
            if ( precedence_len > former_req_len )
              offset = precedence_len - former_req_len;
        } else {
            if (! ( ~former_req_len & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) )
                return -1;
        }
        *former = strdup(
            text.mid( offset, precedence_len - offset ).toUtf8().data() );

        offset = 0;
        if ( latter_req_len >= 0 ) {
            if ( following_len > latter_req_len )
                offset = following_len - latter_req_len;
        } else {
            if (! ( ~latter_req_len & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) ) {
                free( *former );
                return -1;
            }
        }
        *latter = strdup( text.mid( precedence_len + preedit_len,
            following_len - offset ).toUtf8().data() );
        break;

    case UTextOrigin_Beginning:
        *former = 0;
        if ( latter_req_len >= 0 ) {
            if ( precedence_len >= latter_req_len )
                text = text.left( latter_req_len );
            else {
                former_text = text.left( precedence_len );
                if ( following_len >= ( latter_req_len - precedence_len ) )
                    latter_text = text.mid( precedence_len + preedit_len, ( latter_req_len - precedence_len ) );
                else
                    latter_text = text.mid( precedence_len + preedit_len, following_len );
                text = former_text + latter_text;
            }
        } else {
            if (! ( ~latter_req_len & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) )
                return -1;

            former_text = text.left( precedence_len );
            latter_text = text.mid( precedence_len + preedit_len, following_len );
            text = former_text + latter_text;
        }
        *latter = strdup( text.toUtf8().data() );
        break;

    case UTextOrigin_End:
        if ( former_req_len >= 0 ) {
            if ( following_len >= former_req_len )
                text = text.right( former_req_len );
            else {
                    latter_text = text.right( following_len );
                if ( precedence_len >= ( former_req_len - following_len ) )
                    former_text = text.mid( precedence_len - ( former_req_len - following_len ), former_req_len - following_len );
                else
                    former_text = text.left( precedence_len );
                text = former_text + latter_text;
            }
        } else {
            if (! ( ~former_req_len & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) )
                return -1;

            former_text = text.left( precedence_len );
            latter_text = text.right( following_len );
            text = former_text + latter_text;
        }
        *former = strdup( text.toUtf8().data() );
        *latter = 0;
        break;

    case UTextOrigin_Unspecified:
    default:
        return -1;
    }

    return 0;
}

int
QUimTextUtil::acquirePrimaryTextInQTextEdit( enum UTextOrigin origin,
                                             int former_req_len,
                                             int latter_req_len,
                                             char **former, char **latter )
{
    QTextEdit *edit = static_cast<QTextEdit *>( mWidget );
    QString text = edit->toPlainText(); // excluding preedit string
    int len = text.length();

    int preedit_len = mIc->getPreeditString().length();
    int preedit_cursor_pos = mIc->getPreeditCursorPosition();

    int cursor_index = edit->textCursor().position() + preedit_len;
    int precedence_len = cursor_index - preedit_cursor_pos;
    int following_len = len - precedence_len;

    QString former_text;
    QString latter_text;
    switch ( origin ) {
    case UTextOrigin_Cursor:
        {
            int offset = 0;
            if ( former_req_len >= 0 ) {
                if ( precedence_len > former_req_len )
                  offset = precedence_len - former_req_len;
            } else {
                if (! ( ~former_req_len
                        & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) )
                    return -1;
            }
            *former = strdup(
                text.mid( offset, precedence_len - offset ).toUtf8().data() );

            offset = 0;
            if ( latter_req_len >= 0 ) {
                if ( following_len > latter_req_len )
                    offset = following_len - latter_req_len;
            } else {
                if (! ( ~latter_req_len
                        & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) ) {
                    free( *former );
                    return -1;
                }
            }
            *latter = strdup( text.mid( precedence_len + preedit_len,
                following_len - offset ).toUtf8().data() );
            break;
        }

    case UTextOrigin_Beginning:
        *former = 0;
        if ( latter_req_len >= 0 ) {
            if ( precedence_len >= latter_req_len )
                text = text.left( latter_req_len );
            else {
                former_text = text.left( precedence_len );
                if ( following_len >= ( latter_req_len - precedence_len ) )
                    latter_text = text.mid( precedence_len + preedit_len,
                            ( latter_req_len - precedence_len ) );
                else
                    latter_text = text.mid( precedence_len + preedit_len,
                            following_len );
                text = former_text + latter_text;
            }
        } else {
            if (! ( ~latter_req_len
                    & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) )
                return -1;

            former_text = text.left( precedence_len );
            latter_text = text.mid( precedence_len + preedit_len,
                    following_len );
            text = former_text + latter_text;
        }
        *latter = strdup( text.toUtf8().data() );
        break;

    case UTextOrigin_End:
        if ( former_req_len >= 0 ) {
            if ( following_len >= former_req_len )
                text = text.right( former_req_len );
            else {
                    latter_text = text.right( following_len );
                if ( precedence_len >= ( former_req_len - following_len ) )
                    former_text = text.mid( precedence_len
                            - ( former_req_len - following_len ),
                            former_req_len - following_len );
                else
                    former_text = text.left( precedence_len );
                text = former_text + latter_text;
            }
        } else {
            if (! ( ~former_req_len
                    & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) )
                return -1;

            former_text = text.left( precedence_len );
            latter_text = text.right( following_len );
            text = former_text + latter_text;
        }
        *former = strdup( text.toUtf8().data() );
        *latter = 0;
        break;

    case UTextOrigin_Unspecified:
    default:
        return -1;
    }

    return 0;
}

#ifdef ENABLE_QT4_QT3SUPPORT
int
QUimTextUtil::acquirePrimaryTextInQ3TextEdit( enum UTextOrigin origin,
                                              int former_req_len,
                                              int latter_req_len,
                                              char **former, char **latter )
{
    Q3TextEdit *edit = static_cast<Q3TextEdit *>( mWidget );
    QString text;

    int start_para, start_index, end_para, end_index, para, index;
    int n_para;
    int preedit_len, preedit_cursor_pos;
    int sel_start_para, sel_start_index, sel_end_para, sel_end_index;
    Qt::TextFormat format;

    format = edit->textFormat();
    edit->setTextFormat( Qt::PlainText );

    edit->getCursorPosition( &para, &index ); // including preedit string

    // keep current selection
    edit->getSelection( &sel_start_para, &sel_start_index, &sel_end_para,
                        &sel_end_index, 0 );

    preedit_len = mIc->getPreeditString().length();
    preedit_cursor_pos = mIc->getPreeditCursorPosition();
    n_para = edit->paragraphs();

    switch ( origin ) {
    case UTextOrigin_Cursor:
        start_index = index - preedit_cursor_pos;
        start_para = para;
        end_index = start_index + preedit_len;
        end_para = para;

        if ( former_req_len >= 0 ) {
            for ( int i = 0; i < former_req_len; i++ )
                Q3TextEditPositionBackward( &start_para, &start_index );
        } else {
            if ( former_req_len == UTextExtent_Line )
                start_index = 0;
            else if ( former_req_len == UTextExtent_Full ) {
                start_para = 0;
                start_index = 0;
            } else {
                edit->setTextFormat( format );
                return -1;
            }
        }
        edit->setSelection( start_para, start_index, para, index - preedit_cursor_pos, 0 );
        *former = strdup( edit->selectedText().toUtf8().data() );

        if ( latter_req_len >= 0 ) {
            for ( int i = 0; i < latter_req_len; i++ )
                Q3TextEditPositionForward( &end_para, &end_index );
        } else {
            if ( latter_req_len == UTextExtent_Line ) {
                end_index = edit->paragraphLength( end_para );
            } else if ( latter_req_len == UTextExtent_Full ) {
                end_para = n_para - 1;
                end_index = edit->paragraphLength( end_para );
            } else {
                edit->setTextFormat( format );
                return -1;
            }
        }
        edit->setSelection( para, index - preedit_cursor_pos + preedit_len,
                            end_para, end_index, 0 );
        *latter = strdup( edit->selectedText().toUtf8().data() );
        break;

    case UTextOrigin_Beginning:
        *former = 0;

        start_para = 0;
        start_index = 0;
        end_para = start_para;
        end_index = start_index;

        if ( latter_req_len >= 0 ) {
            for ( int i = 0; i < latter_req_len; i++ )
                Q3TextEditPositionForward( &end_para, &end_index );
        } else {
            if ( latter_req_len == UTextExtent_Line )
                end_index = edit->paragraphLength( end_para );
            else if ( latter_req_len == UTextExtent_Full ) {
                end_para = n_para - 1;
                end_index = edit->paragraphLength( end_para );
            } else {
                edit->setTextFormat( format );
                return -1;
            }
        }
        if ( end_para < para || ( end_para == para && end_index <= ( index - preedit_cursor_pos ) ) ) {
            edit->setSelection( start_para, start_index, end_para, end_index, 0 );
            text = edit->selectedText();
        } else {
            edit->setSelection( start_para, start_index, para, index - preedit_cursor_pos, 0 );
            text = edit->selectedText();
            edit->setSelection( para, index - preedit_cursor_pos + preedit_len, end_para, end_index, 0 );
            text += edit->selectedText();
        }
        *latter = strdup( text.toUtf8().data() );
        break;

    case UTextOrigin_End:

        end_para = n_para - 1;
        end_index = edit->paragraphLength( end_para );
        start_para = end_para;
        start_index = end_index;

        if ( former_req_len >= 0 ) {
            for ( int i = 0; i < former_req_len; i++ )
                Q3TextEditPositionBackward( &start_para, &start_index );
        } else {
            if ( former_req_len == UTextExtent_Line )
                start_index = 0;
            else if ( former_req_len == UTextExtent_Full ) {
                start_para = 0;
                start_index = 0;
            } else {
                edit->setTextFormat( format );
                return -1;
            }
        }
        if ( start_para > para || ( start_para == para && start_index >= ( index - preedit_cursor_pos + preedit_len ) ) ) {
            edit->setSelection( start_para, start_index, end_para, end_index, 0 );
            text = edit->selectedText();
        } else {

            edit->setSelection( start_para, start_index, para, index - preedit_cursor_pos, 0 );
            text = edit->selectedText();

            edit->setSelection( para, index - preedit_cursor_pos + preedit_len, end_para, end_index, 0 );
            text += edit->selectedText();
        }
        *former = strdup( text.toUtf8().data() );
        *latter = 0;
        break;

    case UTextOrigin_Unspecified:
    default:
        edit->setTextFormat( format );
        return -1;
    }

    if ( sel_start_para != -1 && sel_start_index != -1 && sel_end_para != -1 &&
         sel_end_index != -1 )
        edit->setSelection( sel_start_index, sel_start_index, sel_end_para, sel_end_index, 0 );
    else
        edit->removeSelection( 0 );

    edit->setCursorPosition( para, index );

    edit->setTextFormat( format );
    return 0; 
}
#endif

int
QUimTextUtil::acquireSelectionText( enum UTextOrigin origin,
                                    int former_req_len, int latter_req_len,
                                    char **former, char **latter )
{
    int err;
#if defined(Q_WS_X11)
    mWidget = QApplication::focusWidget();
#else
    return -1;
#endif

    if ( qobject_cast<QLineEdit *>( mWidget ) )
        err = acquireSelectionTextInQLineEdit( origin, former_req_len,
                                               latter_req_len, former, latter );
    else if ( qobject_cast<QTextEdit *>( mWidget ) )
        err = acquireSelectionTextInQTextEdit( origin, former_req_len,
                                               latter_req_len, former, latter );
#ifdef ENABLE_QT4_QT3SUPPORT
    else if ( qobject_cast<Q3TextEdit *>( mWidget ) )
        err = acquireSelectionTextInQ3TextEdit( origin, former_req_len,
                                                latter_req_len, former,
                                                latter );
#endif
    else
        // FIXME other widgets?
        err = -1;

    return err;
}

int
QUimTextUtil::acquireSelectionTextInQLineEdit( enum UTextOrigin origin,
                                               int former_req_len,
                                               int latter_req_len,
                                               char **former, char **latter )
{
    QLineEdit *edit = static_cast<QLineEdit *>( mWidget );
    QString text;
    int len, offset, start, current;
    bool cursor_at_beginning = false;

    if ( ! edit->hasSelectedText() )
        return -1;

    current = edit->cursorPosition();
    start = edit->selectionStart();

    if ( current == start )
        cursor_at_beginning = true;

    text = edit->selectedText();
    len = text.length();

    if ( origin == UTextOrigin_Beginning ||
         ( origin == UTextOrigin_Cursor && cursor_at_beginning ) ) {
        *former = 0;
        offset = 0;
        if ( latter_req_len >= 0 ) {
            if ( len > latter_req_len )
                offset = len - latter_req_len;
        } else {
            if (! ( ~latter_req_len & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) )
                return -1;
        }
        *latter = strdup( text.left( len - offset ).toUtf8().data() );
    } else if ( origin == UTextOrigin_End ||
                ( origin == UTextOrigin_Cursor && !cursor_at_beginning ) ) {
        offset = 0;
        if ( former_req_len >= 0 ) {
            if ( len > former_req_len )
                offset = len - former_req_len;
        } else {
            if (! ( ~former_req_len & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) )
                return -1;
        }
        *former = strdup( text.mid( offset, len - offset ).toUtf8().data() );
        *latter = 0;
    } else {
        return -1;
    }

    return 0;
}

int
QUimTextUtil::acquireSelectionTextInQTextEdit( enum UTextOrigin origin,
                                               int former_req_len,
                                               int latter_req_len,
                                               char **former, char **latter )
{
    QTextEdit *edit = static_cast<QTextEdit *>( mWidget );
    QTextCursor cursor = edit->textCursor();
    if ( ! cursor.hasSelection() )
        return -1;

    bool cursor_at_beginning = false;
    int current = cursor.position();
    int start = cursor.selectionStart();
    if ( current == start )
        cursor_at_beginning = true;

    QString text = cursor.selectedText();
    int len = text.length();
    int offset;
    if ( origin == UTextOrigin_Beginning ||
         ( origin == UTextOrigin_Cursor && cursor_at_beginning ) ) {
        *former = 0;
        offset = 0;
        if ( latter_req_len >= 0 ) {
            if ( len > latter_req_len )
                offset = len - latter_req_len;
        } else {
            if (! ( ~latter_req_len
                    & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) )
                return -1;
        }
        *latter = strdup( text.left( len - offset ).toUtf8().data() );
    } else if ( origin == UTextOrigin_End ||
                ( origin == UTextOrigin_Cursor && !cursor_at_beginning ) ) {
        offset = 0;
        if ( former_req_len >= 0 ) {
            if ( len > former_req_len )
                offset = len - former_req_len;
        } else {
            if (! ( ~former_req_len
                    & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) )
                return -1;
        }
        *former = strdup( text.mid( offset, len - offset ).toUtf8().data() );
        *latter = 0;
    } else {
        return -1;
    }

    return 0;
}

#ifdef ENABLE_QT4_QT3SUPPORT
int
QUimTextUtil::acquireSelectionTextInQ3TextEdit( enum UTextOrigin origin,
                                                int former_req_len,
                                                int latter_req_len,
                                                char **former, char **latter )
{
    Q3TextEdit *edit = static_cast<Q3TextEdit *>( mWidget );
    QString text;
    int len, offset, newline;
    int start_para, start_index, end_para, end_index;
    int para, index;
    bool cursor_at_beginning = false;
    Qt::TextFormat format;

    if ( ! edit->hasSelectedText() )
        return -1;

    format = edit->textFormat();
    edit->setTextFormat( Qt::PlainText );

    edit->getCursorPosition( &para, &index );
    edit->getSelection(&start_para, &start_index, &end_para, &end_index, 0 );

    if ( para == start_para && index == start_index )
        cursor_at_beginning = true;

    text = edit->selectedText();
    len = text.length();

    if ( origin == UTextOrigin_Beginning ||
         ( origin == UTextOrigin_Cursor && cursor_at_beginning ) ) {
        *former = 0;
        offset = 0;
        if ( latter_req_len >= 0 ) {
            if ( len > latter_req_len )
                offset = len - latter_req_len;
        } else {
            if (! ( ~latter_req_len & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) ) {
                edit->setTextFormat( format );
                return -1;
            }

            if ( latter_req_len == UTextExtent_Line && ( ( newline = text.indexOf( '\n' ) ) != -1 ) )
                offset = len - newline;
        }
        *latter = strdup( text.left( len - offset ).toUtf8().data() );
    } else if ( origin == UTextOrigin_End ||
                ( origin == UTextOrigin_Cursor && !cursor_at_beginning ) ) {
        offset = 0;
        if ( former_req_len >= 0 ) {
            if ( len > former_req_len )
                offset = len - former_req_len;
        } else {
            if (! ( ~former_req_len & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) ) {
                edit->setTextFormat( format );
                return -1;
            }

            if ( former_req_len == UTextExtent_Line && ( ( newline = text.lastIndexOf( '\n' ) ) != -1 ) )
                offset = newline + 1;
        }
        *former = strdup( text.mid( offset, len - offset ).toUtf8().data() );
        *latter = 0;
    } else {
        edit->setTextFormat( format );
        return -1;
    }

    edit->setTextFormat( format );
    return 0;
}
#endif

int
QUimTextUtil::acquireClipboardText( enum UTextOrigin origin,
                                    int former_req_len, int latter_req_len,
                                    char **former, char **latter )
{
    QClipboard *cb = QApplication::clipboard();
    QString text = cb->text( QClipboard::Clipboard );
    int len, offset, newline;

    if ( text.isNull() )
        return -1;

    len = text.length(); 

    /* Cursor position is assumed to be at the end */
    switch ( origin ) {
    case UTextOrigin_Cursor:
    case UTextOrigin_End:
        offset = 0;
        if ( former_req_len >= 0 ) {
            if ( former_req_len < len )
                offset = len - former_req_len;
        } else {
            if (! ( ~former_req_len & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) )
                return -1;

            if ( former_req_len == UTextExtent_Line
                && ( ( newline = text.lastIndexOf( '\n' ) ) != -1 ) )
                offset = newline + 1;
        }
        *former = strdup( text.mid( offset, len - offset ).toUtf8().data() );
        *latter = 0;
        break;

    case UTextOrigin_Beginning:
        *former = 0;
        offset = 0;
        if ( latter_req_len >= 0 ) {
            if ( latter_req_len < len )
                offset = len - latter_req_len;
        } else {
            if (! ( ~latter_req_len & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) )
                return -1;

            if ( latter_req_len == UTextExtent_Line
                && ( ( newline = text.indexOf( '\n' ) ) != -1 ) )
                offset = len - newline;
        }
        *latter = strdup( text.left( len - offset ).toUtf8().data() );
        break;

    case UTextOrigin_Unspecified:
    default:
        return -1;
    }

    return 0;
}

int
QUimTextUtil::deletePrimaryText( enum UTextOrigin origin, int former_req_len,
                                 int latter_req_len )
{
    int err;
#if defined(Q_WS_X11)
    mWidget = QApplication::focusWidget();
#else
    return -1;
#endif

    if ( qobject_cast<QLineEdit *>( mWidget ) )
        err = deletePrimaryTextInQLineEdit( origin, former_req_len,
                                            latter_req_len );
    else if ( qobject_cast<QTextEdit *>( mWidget ) )
        err = deletePrimaryTextInQTextEdit( origin, former_req_len,
                                             latter_req_len );
#ifdef ENABLE_QT4_QT3SUPPORT
    else if ( qobject_cast<Q3TextEdit *>( mWidget ) )
        err = deletePrimaryTextInQ3TextEdit( origin, former_req_len,
                                             latter_req_len );
#endif
    else
        // FIXME other widgets?
        err = -1;

    return err;
}

int
QUimTextUtil::deletePrimaryTextInQLineEdit( enum UTextOrigin origin,
                                            int former_req_len,
                                            int latter_req_len )
{
    QLineEdit *edit = static_cast<QLineEdit *>( mWidget );
    QString text;
    int len, precedence_len, following_len;
    int preedit_len;
    int former_del_start;
    int latter_del_end;

    preedit_len = mIc->getPreeditString().length();

    text = edit->text(); // excluding preedit string
    len = text.length();
    precedence_len = edit->cursorPosition();
    following_len = len - precedence_len;

    switch ( origin ) {
    case UTextOrigin_Cursor:
        former_del_start = 0;
        if ( former_req_len >= 0 ) {
            if ( precedence_len > former_req_len )
                former_del_start = precedence_len - former_req_len;
        } else {
            if (! ( ~former_req_len & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) )
                return -1;
        }
        latter_del_end = len + preedit_len;
        if ( latter_req_len >= 0 ) {
            if ( following_len > latter_req_len )
                latter_del_end = precedence_len + preedit_len + latter_req_len;
        } else {
            if (! ( ~latter_req_len & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) )
                return -1;
        }
        break;

    case UTextOrigin_Beginning:
        former_del_start = 0;
        latter_del_end = precedence_len + preedit_len;
        if ( latter_req_len >= 0 ) {
            if ( precedence_len < latter_req_len ) {
                if ( following_len >= ( latter_req_len - precedence_len ) )
                    latter_del_end = preedit_len + latter_req_len;
                else
                    latter_del_end = len + preedit_len;
            }
        } else {
            if (! ( ~latter_req_len & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) )
                return -1;
            latter_del_end = len + preedit_len;
        }
        break;

    case UTextOrigin_End:
        former_del_start = precedence_len;
        latter_del_end = len + preedit_len;
        if ( former_req_len < 0 ) {
            if (! ( ~former_req_len & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) )
                return -1;

            former_del_start = 0;
        }
        break;

    case UTextOrigin_Unspecified:
    default:
        return -1;
    }

    edit->setText( text.left( former_del_start ) + text.right( len - latter_del_end + preedit_len ) );
    edit->setCursorPosition( former_del_start );

    return 0;
}

int
QUimTextUtil::deletePrimaryTextInQTextEdit( enum UTextOrigin origin,
                                            int former_req_len,
                                            int latter_req_len )
{
    QTextEdit *edit = static_cast<QTextEdit *>( mWidget );
    QString text = edit->toPlainText(); // excluding preedit string
    int len = text.length();

    int preedit_len = mIc->getPreeditString().length();

    QTextCursor cursor = edit->textCursor();
    int precedence_len = cursor.position();
    int following_len = len - precedence_len;

    int former_del_start;
    int latter_del_end;
    switch ( origin ) {
    case UTextOrigin_Cursor:
        former_del_start = 0;
        if ( former_req_len >= 0 ) {
            if ( precedence_len > former_req_len )
                former_del_start = precedence_len - former_req_len;
        } else {
            if (! ( ~former_req_len
                    & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) )
                return -1;
        }
        latter_del_end = len + preedit_len;
        if ( latter_req_len >= 0 ) {
            if ( following_len > latter_req_len )
                latter_del_end = precedence_len + preedit_len + latter_req_len;
        } else {
            if (! ( ~latter_req_len
                    & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) )
                return -1;
        }
        break;

    case UTextOrigin_Beginning:
        former_del_start = 0;
        latter_del_end = precedence_len + preedit_len;
        if ( latter_req_len >= 0 ) {
            if ( precedence_len < latter_req_len ) {
                if ( following_len >= ( latter_req_len - precedence_len ) )
                    latter_del_end = preedit_len + latter_req_len;
                else
                    latter_del_end = len + preedit_len;
            }
        } else {
            if (! ( ~latter_req_len
                    & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) )
                return -1;
            latter_del_end = len + preedit_len;
        }
        break;

    case UTextOrigin_End:
        former_del_start = precedence_len;
        latter_del_end = len + preedit_len;
        if ( former_req_len < 0 ) {
            if (! ( ~former_req_len
                    & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) )
                return -1;

            former_del_start = 0;
        }
        break;

    case UTextOrigin_Unspecified:
    default:
        return -1;
    }

    // don't call setText() to avoid flicker unlike QLineEdit
    int end_index = latter_del_end - preedit_len;
    if ( precedence_len != end_index ) {
        cursor.setPosition( precedence_len );
        cursor.setPosition( end_index, QTextCursor::KeepAnchor );
        edit->setTextCursor( cursor );
        cursor.deleteChar();
    }
    if ( precedence_len != former_del_start ) {
        cursor.setPosition( precedence_len );
        cursor.setPosition( former_del_start, QTextCursor::KeepAnchor );
        edit->setTextCursor( cursor );
        cursor.deleteChar();
    }

    return 0;
}

#ifdef ENABLE_QT4_QT3SUPPORT
int
QUimTextUtil::deletePrimaryTextInQ3TextEdit( enum UTextOrigin origin,
                                             int former_req_len,
                                             int latter_req_len )
{
    Q3TextEdit *edit = static_cast<Q3TextEdit *>( mWidget );
    int start_para, start_index, end_para, end_index, para, index;
    int n_para;

    savePreedit();

    edit->getCursorPosition( &para, &index );
    n_para = edit->paragraphs();

    switch ( origin ) {
    case UTextOrigin_Cursor:
        start_index = index;
        start_para = para;
        end_index = start_index;
        end_para = para;

        if ( former_req_len >= 0 ) {
            for ( int i = 0; i < former_req_len; i++ )
                Q3TextEditPositionBackward( &start_para, &start_index );
        } else {
            if ( former_req_len == UTextExtent_Line ) {
                start_index = 0;
            } else if ( former_req_len == UTextExtent_Full ) {
                start_para = 0;
                start_index = 0;
            } else {
                restorePreedit();
                return -1;
            }
        }
        if ( latter_req_len >= 0 ) {
            for ( int i = 0; i < latter_req_len; i++ )
                Q3TextEditPositionForward( &end_para, &end_index );
        } else {
            if ( latter_req_len == UTextExtent_Line ) {
                end_index = edit->paragraphLength( end_para );
            } else if ( latter_req_len == UTextExtent_Full ) {
                end_para = n_para - 1;
                end_index = edit->paragraphLength( end_para );
            } else {
                restorePreedit();
                return -1;
            }
        }
        break;

    case UTextOrigin_Beginning:
        start_para = 0;
        start_index = 0;
        end_para = start_para;
        end_index = start_index;

        if ( latter_req_len >= 0 ) {
            for ( int i = 0; i < latter_req_len; i++ )
                Q3TextEditPositionForward( &end_para, &end_index );
        } else {
            if ( latter_req_len == UTextExtent_Line ) {
                end_index = edit->paragraphLength( end_para );
            } else if ( latter_req_len == UTextExtent_Full ) {
                end_para = n_para - 1;
                end_index = edit->paragraphLength( end_para );
            } else {
                restorePreedit();
                return -1;
            }
        }
        break;

    case UTextOrigin_End:
        end_para = n_para - 1;
        end_index = edit->paragraphLength( end_para );
        start_para = end_para;
        start_index = end_index;

        if ( former_req_len >= 0 ) {
            for ( int i = 0; i < former_req_len; i++ )
                Q3TextEditPositionBackward( &start_para, &start_index );
        } else {
            if ( former_req_len == UTextExtent_Line )
                start_index = 0;
            else if ( former_req_len == UTextExtent_Full ) {
                start_para = 0;
                start_index = 0;
            } else {
                restorePreedit();
                return -1;
            }
        }
        break;

    case UTextOrigin_Unspecified:
    default:
        restorePreedit();
        return -1;
    }
    edit->setSelection( start_para, start_index, end_para, end_index, 1 );
    edit->removeSelectedText( 1 );
    edit->setCursorPosition( start_para, start_index );
    restorePreedit();

    return 0; 
}
#endif

int
QUimTextUtil::deleteSelectionText( enum UTextOrigin origin,
                                   int former_req_len, int latter_req_len )
{
    int err;
#if defined(Q_WS_X11)
    mWidget = QApplication::focusWidget();
#else
    return -1;
#endif

    if ( qobject_cast<QLineEdit *>( mWidget ) )
        err = deleteSelectionTextInQLineEdit( origin, former_req_len,
                                              latter_req_len );
    else if ( qobject_cast<QTextEdit *>( mWidget ) )
        err = deleteSelectionTextInQTextEdit( origin, former_req_len,
                                              latter_req_len );
#ifdef ENABLE_QT4_QT3SUPPORT
    else if ( qobject_cast<Q3TextEdit *>( mWidget ) )
        err = deleteSelectionTextInQ3TextEdit( origin, former_req_len,
                                               latter_req_len );
#endif
    else
        // FIXME other widgets?
        err = -1;

    return err;
}

int
QUimTextUtil::deleteSelectionTextInQLineEdit( enum UTextOrigin origin,
                                              int former_req_len,
                                              int latter_req_len )
{
    QLineEdit *edit = static_cast<QLineEdit *>( mWidget );
    QString text;
    int len, start, end, current;
    bool cursor_at_beginning = false;

    if ( ! edit->hasSelectedText() )
        return -1;

    current = edit->cursorPosition();
    start = edit->selectionStart();
    if ( current == start )
        cursor_at_beginning = true;

    text = edit->selectedText();
    len = text.length();
    end = start + len;

    if ( origin == UTextOrigin_Beginning ||
         ( origin == UTextOrigin_Cursor && cursor_at_beginning ) ) {
        if ( latter_req_len >= 0 ) {
            if ( len > latter_req_len )
                end = start + latter_req_len;
        } else {
            if (! ( ~latter_req_len & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) )
                return -1;
        }
    } else if ( origin == UTextOrigin_End ||
                ( origin == UTextOrigin_Cursor && !cursor_at_beginning ) ) {
        if ( former_req_len >= 0 ) {
            if ( len > former_req_len )
                start = end - former_req_len;
        } else {
            if (! ( ~former_req_len & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) )
                return -1;
        }
    } else {
        return -1;
    }
    edit->setSelection( start, end - start );
    edit->del();

    return 0;
}

int
QUimTextUtil::deleteSelectionTextInQTextEdit( enum UTextOrigin origin,
                                              int former_req_len,
                                              int latter_req_len )
{
    QTextEdit *edit = static_cast<QTextEdit *>( mWidget );
    QTextCursor cursor = edit->textCursor();
    if ( ! cursor.hasSelection() )
        return -1;

    bool cursor_at_beginning = false;
    int current = cursor.position();
    int start = cursor.selectionStart();
    if ( current == start )
        cursor_at_beginning = true;

    QString text = cursor.selectedText();
    int len = text.length();
    int end = start + len;
    if ( origin == UTextOrigin_Beginning ||
         ( origin == UTextOrigin_Cursor && cursor_at_beginning ) ) {
        if ( latter_req_len >= 0 ) {
            if ( len > latter_req_len )
                end = start + latter_req_len;
        } else {
            if (! ( ~latter_req_len
                    & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) )
                return -1;
        }
    } else if ( origin == UTextOrigin_End ||
                ( origin == UTextOrigin_Cursor && !cursor_at_beginning ) ) {
        if ( former_req_len >= 0 ) {
            if ( len > former_req_len )
                start = end - former_req_len;
        } else {
            if (! ( ~former_req_len
                    & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) )
                return -1;
        }
    } else {
        return -1;
    }
    cursor.setPosition( start );
    cursor.setPosition( end, QTextCursor::KeepAnchor );
    edit->setTextCursor( cursor );
    cursor.deleteChar();

    return 0;
}

#ifdef ENABLE_QT4_QT3SUPPORT
int
QUimTextUtil::deleteSelectionTextInQ3TextEdit( enum UTextOrigin origin,
                                               int former_req_len,
                                               int latter_req_len )
{
    Q3TextEdit *edit = static_cast<Q3TextEdit *>( mWidget );
    QString text;
    int len, newline;
    int para, index;
    int sel_para_from, sel_index_from, sel_para_to, sel_index_to;
    int start_para, start_index, end_para, end_index;
    bool cursor_at_beginning = false;

    if ( ! edit->hasSelectedText() )
        return -1;

    edit->getCursorPosition( &para, &index );
    edit->getSelection( &sel_para_from, &sel_index_from, &sel_para_to, &sel_index_to, 0 );

    if ( para == sel_para_from && index == sel_index_from )
        cursor_at_beginning = true;

    text = edit->selectedText();
    len = text.length();

    start_para = sel_para_from;
    start_index = sel_index_from;
    end_para = sel_para_to;
    end_index = sel_index_to;

    if ( origin == UTextOrigin_Beginning ||
         ( origin == UTextOrigin_Cursor && cursor_at_beginning ) ) {
        edit->setCursorPosition( sel_para_from, sel_index_from );
        if ( latter_req_len >= 0 ) {
            if ( len > latter_req_len ) {
                end_para = sel_para_from;
                end_index = sel_index_from;
                for ( int i = 0; i < latter_req_len; i++)
                    Q3TextEditPositionForward( &end_para, &end_index );
            }
        } else {
            if (! ( ~latter_req_len & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) )
                return -1;

            if ( latter_req_len == UTextExtent_Line && ( ( newline = text.indexOf('\n') ) != -1 ) ) {
                end_para = sel_para_from;
                end_index = sel_index_from + newline;
            }
        }
    } else if ( origin == UTextOrigin_End ||
                ( origin == UTextOrigin_Cursor && !cursor_at_beginning ) ) {
        if ( former_req_len >= 0 ) {
            if ( len > former_req_len ) {
                start_para = sel_para_to;
                start_index = sel_index_to;
                for ( int i = 0; i < former_req_len; i++)
                    Q3TextEditPositionBackward( &start_para, &start_index );
            }
        } else {
            if (! ( ~former_req_len & ( ~UTextExtent_Line | ~UTextExtent_Full ) ) )
                return -1;

            if ( former_req_len == UTextExtent_Line && ( ( newline = text.lastIndexOf( '\n' ) ) != -1 ) ) {
                start_para = sel_para_to;
                start_index = 0;
            }
        }
    } else {
        return -1;
    }
    edit->setSelection( start_para, start_index, end_para, end_index, 1 );
    edit->removeSelectedText( 1 );

    return 0;
}

void
QUimTextUtil::Q3TextEditPositionBackward( int *cursor_para, int *cursor_index )
{
    Q3TextEdit *edit = static_cast<Q3TextEdit *>( mWidget );
    int preedit_len, preedit_cursor_pos;
    int para, index;
    int current_para, current_index;

    current_para = *cursor_para;
    current_index = *cursor_index;

    if ( ! mPreeditSaved ) {
        preedit_len = mIc->getPreeditString().length();
        preedit_cursor_pos = mIc->getPreeditCursorPosition();
    } else {
        preedit_len = 0;
        preedit_cursor_pos = 0;
    }
    edit->getCursorPosition( &para, &index );

    if ( current_para == para && current_index > ( index - preedit_cursor_pos ) && ( current_index <= ( index - preedit_cursor_pos + preedit_len ) ) )
        current_index = index - preedit_cursor_pos;

    if ( current_index > 0 )
        current_index--;
    else {
        if ( current_para > 0 ) {
            current_para--;
            current_index = edit->paragraphLength( current_para );
        }
    }

    *cursor_para = current_para;
    *cursor_index = current_index;
}

void
QUimTextUtil::Q3TextEditPositionForward( int *cursor_para, int *cursor_index )
{
    Q3TextEdit *edit = static_cast<Q3TextEdit *>( mWidget );
    int n_para = edit->paragraphs();
    int preedit_len, preedit_cursor_pos;
    int current_para_len;
    int para, index;
    int current_para, current_index;

    current_para = *cursor_para;
    current_index = *cursor_index;

    current_para_len = edit->paragraphLength( current_para );
    if ( ! mPreeditSaved ) {
        preedit_len = mIc->getPreeditString().length();
        preedit_cursor_pos = mIc->getPreeditCursorPosition();
    } else {
        preedit_len = 0;
        preedit_cursor_pos = 0;
    }
    edit->getCursorPosition( &para, &index );

    if ( current_para == para && current_index >= ( index - preedit_cursor_pos ) && current_index < ( index - preedit_cursor_pos + preedit_len ) )
        current_index = index - preedit_cursor_pos + preedit_len;

    if ( current_para == n_para - 1 ) {
        if ( current_index < current_para_len )
            current_index++;
    } else {
        if ( current_index < current_para_len )
            current_index++;
        else {
            current_para++;
            current_index = 0;
        }
    }

    *cursor_para = current_para;
    *cursor_index = current_index;
}
#endif

void QUimTextUtil::savePreedit()
{
    mIc->saveContext();
    mPreeditSaved = true;
}

void QUimTextUtil::restorePreedit()
{
    mIc->restoreContext();
    mPreeditSaved = false;
}
