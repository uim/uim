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
#include <config.h>

#include <qvaluelist.h>

#include "uim.h"

#include "quiminfomanager.h"


QUimInfoManager::QUimInfoManager()
{
    initUimInfo();
}

QUimInfoManager::~QUimInfoManager()
{
}

QValueList<uimInfo>
QUimInfoManager::getUimInfo()
{
    return info;
}

void
QUimInfoManager::initUimInfo()
{
    info.clear();

    uim_context tmp_uc = uim_create_context( NULL, "UTF-8", NULL, NULL, NULL, NULL );
    struct uimInfo ui;
    int nr = uim_get_nr_im( tmp_uc );
    for ( int i = 0; i < nr; i++ )
    {
        ui.name = uim_get_im_name( tmp_uc, i );
        ui.lang = uim_get_im_language (tmp_uc, i );
        ui.short_desc = uim_get_im_short_desc( tmp_uc, i );

        info.append( ui );
    }
    uim_release_context( tmp_uc );
}

QString
QUimInfoManager::imLang( const QString &imname )
{
    int i, n;

    n = info.count();
    for (i = 0; i < n; i++) {
        if ( info[i].name == imname )
            return info[i].lang;
    }

    return "";
}
