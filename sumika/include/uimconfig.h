/*
 *  $Id:$
 *  Copyright (c) 2003,2004 Masahito Omote <omote@utyuuzin.net>
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *  3. Neither the name of authors nor the names of its contributors
 *     may be used to endorse or promote products derived from this software
 *     without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 *  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 *  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 *  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 *  SUCH DAMAGE.
 */

#ifndef __UIM_CONFIG_H__
#define __UIM_CONFIG_H__

struct _uim_config_global {
    char *default_im;
    char *cand_win_pos;
    int   enable_im_switch;
};

struct _uim_config_anthy {
    int  dummy;
};

struct _uim_config_canna {
    char *cannaserver;
};

struct _uim_config_prime {
    int  dummy;
};

struct _uim_config_skk {
    char *skk_style;
    char *skk_dic_filename;
    char *skk_userdic_filename;
    char *skk_uim_userdic_filename;
    int   recursive_learning;
};

struct _uim_config {
    struct _uim_config_global global;
    struct _uim_config_anthy  anthy;
    struct _uim_config_canna  canna;
    struct _uim_config_prime  prime;
    struct _uim_config_skk    skk;
};

int init_uim_config  (struct _uim_config *);
int write_uim_config (struct _uim_config *);
#endif /* __UIM_CONFIG_H__ */
