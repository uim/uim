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
#ifndef UIM_TQT_IMMODULE_QTEXT_UTIL_H
#define UIM_TQT_IMMODULE_QTEXT_UTIL_H

class QUimInputContext;

class QUimTextUtil : TQObject
{
    TQ_OBJECT

public:
    QUimTextUtil( TQObject *parent = 0 );
    ~QUimTextUtil();

    static int acquire_text_cb( void *ptr, enum UTextArea text_id,
                                enum UTextOrigin origin,
                                int former_req_len, int latter_req_len,
                                char **former, char **latter );
    static int delete_text_cb( void *ptr, enum UTextArea text_id,
                               enum UTextOrigin origin,
                               int former_req_len, int latter_req_len );

private:
    int acquirePrimaryText( enum UTextOrigin origin,
                            int former_req_len, int latter_req_len,
                            char **former, char **latter );
    int acquirePrimaryTextInQLineEdit( enum UTextOrigin origin,
                                       int former_req_len, int latter_req_len,
                                       char **former, char **latter );
    int acquirePrimaryTextInQTextEdit( enum UTextOrigin origin,
                                       int former_req_len, int latter_req_len,
                                       char **former, char **latter );

    int acquireSelectionText( enum UTextOrigin origin,
                              int former_req_len, int latter_req_len,
                              char **former, char **latter );
    int acquireSelectionTextInQLineEdit( enum UTextOrigin origin,
                                         int former_req_len,
					 int latter_req_len,
                                         char **former, char **latter );
    int acquireSelectionTextInQTextEdit( enum UTextOrigin origin,
                                         int former_req_len,
					 int latter_req_len,
                                         char **former, char **latter );

    int acquireClipboardText( enum UTextOrigin origin,
                              int former_req_len, int latter_req_len,
                              char **former, char **latter );

    int deletePrimaryText( enum UTextOrigin origin,
                           int former_req_len, int latter_req_len );
    int deletePrimaryTextInQLineEdit( enum UTextOrigin origin,
                                      int former_req_len, int latter_req_len );
    int deletePrimaryTextInQTextEdit( enum UTextOrigin origin,
                                      int former_req_len, int latter_req_len );

    int deleteSelectionText( enum UTextOrigin origin,
                             int former_req_len, int latter_req_len );
    int deleteSelectionTextInQLineEdit( enum UTextOrigin origin,
                                       int former_req_len, int latter_req_len );
    int deleteSelectionTextInQTextEdit( enum UTextOrigin origin,
                                       int former_req_len, int latter_req_len );

    void QTextEditPositionBackward( int *para, int *index );

    void QTextEditPositionForward( int *para, int *index );

    void savePreedit();
    void restorePreedit();

    TQWidget *mWidget;
    QUimInputContext *mIc;
    bool mPreeditSaved;
};

#endif
