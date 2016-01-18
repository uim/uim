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

#include <config.h>

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <locale.h>
#include <errno.h>

#include "uim/uim.h"

#include "quiminputcontext.h"
#include "quiminputcontext_compose.h"

#include <qtextcodec.h>

#include <X11/keysym.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#define COMPOSE_FILE	"Compose"
#define COMPOSE_DIR_FILE	"X11/locale/compose.dir"
#define XLOCALE_DIR	"X11/locale"
#define FALLBACK_XLIB_DIR	"/usr/X11R6/lib"

#define XLC_BUFSIZE	256
#define iscomment(ch)	((ch) == '#' || (ch) == '\0')

static int parse_line(char *line, char **argv, int argsize);
static unsigned int KeySymToUcs4(KeySym keysym);

Compose::Compose(DefTree *top, QUimInputContext *ic)
{
    m_ic = ic;
    m_composed = NULL;
    m_top = top;
    m_context = top;
}

Compose::~Compose()
{
}

bool Compose::handle_qkey(QKeyEvent *event)
{
    int type = event->type();
    int qkey = event->key();
    int qstate = event->state();

    unsigned int xkeysym, xstate;
    bool press = (type == QEvent::KeyPress) ? true : false;
    
    xstate = 0;
    if (qstate & Qt::ShiftButton)
	xstate |= ShiftMask;
    if (qstate & Qt::ControlButton)
	xstate |= ControlMask;
    if (qstate & Qt::AltButton)
	xstate |= Mod1Mask; // XXX
    if (qstate & Qt::MetaButton)
	xstate |= Mod1Mask; // XXX

    if (qkey >= 0x20 && qkey <= 0xff) {
        if (isascii(qkey) && isprint(qkey)) {
	    int ascii = event->ascii();
	    if (isalpha(ascii))
	    	xkeysym = ascii;
	    else
		if ((qstate & Qt::ControlButton) &&
		    (ascii >= 0x01 && ascii <= 0x1a))
		    if (qstate & Qt::ShiftButton)
			xkeysym = ascii + 0x40;
		    else
			xkeysym = ascii + 0x60;
		else
	    	    xkeysym = qkey;
	} else {
            xkeysym = qkey;
	}
    } else if (qkey >= Qt::Key_Dead_Grave && qkey <= Qt::Key_Dead_Horn) {
        xkeysym = qkey + 0xec00;
    } else {
        switch (qkey) {
        case Qt::Key_Escape: xkeysym = XK_Escape; break;
        case Qt::Key_Tab: xkeysym = XK_Tab; break;
        case Qt::Key_BackSpace: xkeysym = XK_BackSpace; break;
        case Qt::Key_Return: xkeysym = XK_Return; break;
        case Qt::Key_Insert: xkeysym = XK_Insert; break;
        case Qt::Key_Delete: xkeysym = XK_Delete; break;
        case Qt::Key_Pause: xkeysym = XK_Pause; break;
        case Qt::Key_Print: xkeysym = XK_Print; break;
        case Qt::Key_SysReq: xkeysym = XK_Sys_Req; break;
        case Qt::Key_Clear: xkeysym = XK_Clear; break;
        case Qt::Key_Home: xkeysym = XK_Home; break;
        case Qt::Key_End: xkeysym = XK_End; break;
        case Qt::Key_Left: xkeysym = XK_Left; break;
        case Qt::Key_Up: xkeysym = XK_Up; break;
        case Qt::Key_Right: xkeysym = XK_Right; break;
        case Qt::Key_Down: xkeysym = XK_Down; break;
        case Qt::Key_Prior: xkeysym = XK_Prior; break;
        case Qt::Key_Next: xkeysym = XK_Next; break;
        case Qt::Key_Shift: xkeysym = XK_Shift_L; break;
        case Qt::Key_Control: xkeysym = XK_Control_L; break;
        case Qt::Key_Meta: xkeysym = XK_Meta_L; break;
        case Qt::Key_Alt: xkeysym = XK_Alt_L; break;
        case Qt::Key_CapsLock: xkeysym = XK_Caps_Lock; break;
        case Qt::Key_NumLock: xkeysym = XK_Num_Lock; break;
        case Qt::Key_ScrollLock: xkeysym = XK_Scroll_Lock; break;
        case Qt::Key_F1: xkeysym = XK_F1; break;
        case Qt::Key_F2: xkeysym = XK_F2; break;
        case Qt::Key_F3: xkeysym = XK_F3; break;
        case Qt::Key_F4: xkeysym = XK_F4; break;
        case Qt::Key_F5: xkeysym = XK_F5; break;
        case Qt::Key_F6: xkeysym = XK_F6; break;
        case Qt::Key_F7: xkeysym = XK_F7; break;
        case Qt::Key_F8: xkeysym = XK_F8; break;
        case Qt::Key_F9: xkeysym = XK_F9; break;
        case Qt::Key_F10: xkeysym = XK_F10; break;
        case Qt::Key_F11: xkeysym = XK_F11; break;
        case Qt::Key_F12: xkeysym = XK_F12; break;
        case Qt::Key_F13: xkeysym = XK_F13; break;
        case Qt::Key_F14: xkeysym = XK_F14; break;
        case Qt::Key_F15: xkeysym = XK_F15; break;
        case Qt::Key_F16: xkeysym = XK_F16; break;
        case Qt::Key_F17: xkeysym = XK_F17; break;
        case Qt::Key_F18: xkeysym = XK_F18; break;
        case Qt::Key_F19: xkeysym = XK_F19; break;
        case Qt::Key_F20: xkeysym = XK_F20; break;
        case Qt::Key_F21: xkeysym = XK_F21; break;
        case Qt::Key_F22: xkeysym = XK_F22; break;
        case Qt::Key_F23: xkeysym = XK_F23; break;
        case Qt::Key_F24: xkeysym = XK_F24; break;
        case Qt::Key_F25: xkeysym = XK_F25; break;
        case Qt::Key_F26: xkeysym = XK_F26; break;
        case Qt::Key_F27: xkeysym = XK_F27; break;
        case Qt::Key_F28: xkeysym = XK_F28; break;
        case Qt::Key_F29: xkeysym = XK_F29; break;
        case Qt::Key_F30: xkeysym = XK_F30; break;
        case Qt::Key_F31: xkeysym = XK_F31; break;
        case Qt::Key_F32: xkeysym = XK_F32; break;
        case Qt::Key_F33: xkeysym = XK_F33; break;
        case Qt::Key_F34: xkeysym = XK_F34; break;
        case Qt::Key_F35: xkeysym = XK_F35; break;
        case Qt::Key_Super_L: xkeysym = XK_Super_L; break;
        case Qt::Key_Super_R: xkeysym = XK_Super_R; break;
        case Qt::Key_Menu: xkeysym = XK_Menu; break;
        case Qt::Key_Hyper_L: xkeysym = XK_Hyper_L; break;
        case Qt::Key_Hyper_R: xkeysym = XK_Hyper_R; break;
        case Qt::Key_Help: xkeysym = XK_Help; break;
        case Qt::Key_Multi_key: xkeysym = XK_Multi_key; break;
        case Qt::Key_Codeinput: xkeysym = XK_Codeinput; break;
        case Qt::Key_SingleCandidate: xkeysym = XK_SingleCandidate; break;
        case Qt::Key_PreviousCandidate: xkeysym = XK_PreviousCandidate; break;
        case Qt::Key_Mode_switch: xkeysym = XK_Mode_switch; break;
        case Qt::Key_Kanji: xkeysym = XK_Kanji; break;
        case Qt::Key_Muhenkan: xkeysym = XK_Muhenkan; break;
        case Qt::Key_Henkan: xkeysym = XK_Henkan_Mode; break;
        case Qt::Key_Romaji: xkeysym = XK_Romaji; break;
        case Qt::Key_Hiragana: xkeysym = XK_Hiragana; break;
        case Qt::Key_Katakana: xkeysym = XK_Katakana; break;
        case Qt::Key_Hiragana_Katakana: xkeysym = XK_Hiragana_Katakana; break;
        case Qt::Key_Zenkaku: xkeysym = XK_Zenkaku; break;
        case Qt::Key_Hankaku: xkeysym = XK_Hankaku; break;
        case Qt::Key_Zenkaku_Hankaku: xkeysym = XK_Zenkaku_Hankaku; break;
        case Qt::Key_Touroku: xkeysym = XK_Touroku; break;
        case Qt::Key_Massyo: xkeysym = XK_Massyo; break;
        case Qt::Key_Kana_Lock: xkeysym = XK_Kana_Lock; break;
        case Qt::Key_Kana_Shift: xkeysym = XK_Kana_Shift; break;
        case Qt::Key_Eisu_Shift: xkeysym = XK_Eisu_Shift; break;
        case Qt::Key_Eisu_toggle: xkeysym = XK_Eisu_toggle; break;

        case Qt::Key_Hangul: xkeysym = XK_Hangul; break;
        case Qt::Key_Hangul_Start: xkeysym = XK_Hangul_Start; break;
        case Qt::Key_Hangul_End: xkeysym = XK_Hangul_End; break;
        case Qt::Key_Hangul_Jamo: xkeysym = XK_Hangul_Jamo; break;
        case Qt::Key_Hangul_Romaja: xkeysym = XK_Hangul_Romaja; break;
        case Qt::Key_Hangul_Jeonja: xkeysym = XK_Hangul_Jeonja; break;
        case Qt::Key_Hangul_Banja: xkeysym = XK_Hangul_Banja; break;
        case Qt::Key_Hangul_PreHanja: xkeysym = XK_Hangul_PreHanja; break;
        case Qt::Key_Hangul_PostHanja: xkeysym = XK_Hangul_PostHanja; break;
        case Qt::Key_Hangul_Special: xkeysym = XK_Hangul_Special; break;
        default: xkeysym = qkey; break;
        }
    }

    return handleKey(xkeysym, xstate, press);
}

bool Compose::handleKey(KeySym xkeysym, int xkeystate, bool is_push)
{
    DefTree *p;

    if ((is_push == false)  || m_top == NULL)
	return false;

    if (IsModifierKey(xkeysym))
	return false;

    for (p = m_context; p ; p = p->next) {
	if (((xkeystate & p->modifier_mask) == p->modifier) &&
		(xkeysym == p->keysym)) {
	    break;
	}
    }

    if (p) { // Matched
	if (p->succession) { // Intermediate
	    m_context = p->succession;
	    return true;
	} else { // Terminate (reached to leaf)
	    m_composed = p;
	    // commit string here
	    m_ic->commitString(QString::fromUtf8(m_composed->utf8));
	    // initialize internal state for next key sequence
	    m_context = m_top;
	    return true;
	}
    } else { // Unmatched
	if (m_context == m_top)
	    return false;
	// Error (Sequence Unmatch occurred)
	m_context = m_top;
	return true;
    }
}

void Compose::reset()
{
    m_context = m_top;
    m_composed = NULL;
}

static int
nextch(FILE *fp, int *lastch)
{
    int c;

    if (*lastch != 0) {
	c = *lastch;
	*lastch = 0;
    } else {
	c = getc(fp);
	if (c == '\\') {
	    c = getc(fp);
	    if (c == '\n') {
		c = getc(fp);
	    } else {
		ungetc(c, fp);
		c = '\\';
	    }
	}
    }
    return(c);
}

static void
putbackch(int c, int *lastch)
{
    *lastch = c;
}


#define ENDOFFILE 0
#define ENDOFLINE 1
#define COLON 2
#define LESS 3
#define GREATER 4
#define EXCLAM 5
#define TILDE 6
#define STRING 7
#define KEY 8
#define ERROR 9


#ifndef isalnum
#define isalnum(c)      \
    (('0' <= (c) && (c) <= '9')  || \
     ('A' <= (c) && (c) <= 'Z')  || \
     ('a' <= (c) && (c) <= 'z'))
#endif

static int
nexttoken(FILE *fp, char **tokenbuf, int *lastch, size_t *buflen)
{
    int c;
    int token;
    char *p;
    int i, j;
    size_t len = 0;

    while ((c = nextch(fp, lastch)) == ' ' || c == '\t') {
    }
    switch (c) {
    case EOF:
	token = ENDOFFILE;
	break;
    case '\n':
	token = ENDOFLINE;
	break;
    case '<':
	token = LESS;
	break;
    case '>':
	token = GREATER;
	break;
    case ':':
	token = COLON;
	break;
    case '!':
	token = EXCLAM;
	break;
    case '~':
	token = TILDE;
	break;
    case '"':
	p = *tokenbuf;
	while ((c = nextch(fp, lastch)) != '"') {
	    if (len >= *buflen - 1) {
		    *buflen += BUFSIZ;
		    *tokenbuf = (char *)realloc(*tokenbuf, *buflen);
		    p = *tokenbuf + len;
	    }
	    if (c == '\n' || c == EOF) {
		putbackch(c, lastch);
		token = ERROR;
		goto string_error;
	    } else if (c == '\\') {
		c = nextch(fp, lastch);
		switch (c) {
		case '\\':
		case '"':
		    *p++ = (char)c;
		    len++;
		    break;
		case 'n':
		    *p++ = '\n';
		    len++;
		    break;
		case 'r':
		    *p++ = '\r';
		    len++;
		    break;
		case 't':
		    *p++ = '\t';
		    len++;
		    break;
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		    i = c - '0';
		    c = nextch(fp, lastch);
		    for (j = 0; j < 2 && c >= '0' && c <= '7'; j++) {
			i <<= 3;
			i += c - '0';
			c = nextch(fp, lastch);
		    }
		    putbackch(c, lastch);
		    *p++ = (char)i;
		    len++;
		    break;
		case 'X':
		case 'x':
		    i = 0;
		    for (j = 0; j < 2; j++) {
			c = nextch(fp, lastch);
			i <<= 4;
			if (c >= '0' && c <= '9') {
			    i += c - '0';
			} else if (c >= 'A' && c <= 'F') {
			    i += c - 'A' + 10;
			} else if (c >= 'a' && c <= 'f') {
			    i += c - 'a' + 10;
			} else {
			    putbackch(c, lastch);
			    i >>= 4;
			    break;
			}
		    }
		    if (j == 0) {
			token = ERROR;
			goto string_error;
		    }
		    *p++ = (char)i;
		    len++;
		    break;
		case EOF:
		    putbackch(c, lastch);
		    token = ERROR;
		    goto string_error;
		default:
		    *p++ = (char)c;
		    len++;
		    break;
		}
	    } else {
		*p++ = (char)c;
		len++;
	    }
	}
	*p = '\0';
	token = STRING;
	break;
    case '#':
	while ((c = nextch(fp, lastch)) != '\n' && c != EOF) {
	}
	if (c == '\n') {
	    token = ENDOFLINE;
	} else {
	    token = ENDOFFILE;
	}
	break;
    default:
	if (isalnum(c) || c == '_' || c == '-') {
	    if (len >= *buflen - 1) {
		*buflen += BUFSIZ;
		*tokenbuf = (char *)realloc(*tokenbuf,  *buflen);
	    }
	    p = *tokenbuf;
	    *p++ = (char)c;
	    len++;
	    c = nextch(fp, lastch);
	    while (isalnum(c) || c == '_' || c == '-') {
	        if (len >= *buflen - 1) {
			*buflen += BUFSIZ;
			*tokenbuf = (char *)realloc(*tokenbuf,  *buflen);
			p = *tokenbuf + len;
		}
		*p++ = (char)c;
		len++;
		c = nextch(fp, lastch);
	    }
	    *p = '\0';
	    putbackch(c, lastch);
	    token = KEY;
	} else {
	    token = ERROR;
	}
	break;
    }
string_error:
    return(token);
}

static long
modmask(char *name)
{
    long mask;

    struct _modtbl {
	const char *name;
	long mask;
    };
    struct _modtbl *p;

    static struct _modtbl tbl[] = {
	{ "Ctrl",       ControlMask     },
	{ "Lock",       LockMask	},
	{ "Caps",       LockMask	},
	{ "Shift",      ShiftMask       },
	{ "Alt",	Mod1Mask	},
	{ "Meta",       Mod1Mask	},
	{ NULL,	 0	       }};

    p = tbl;
    mask = 0;
    for (p = tbl; p->name != NULL; p++) {
	if (strcmp(name, p->name) == 0) {
	    mask = p->mask;
	    break;
	}
    }
    return(mask);
}

int
QUimInputContext::TransFileName(char *transname, const char *name, size_t len)
{
    char *home = NULL;
    char lcCompose[MAXPATHLEN];
    const char *i = name;
    char *j;
    char ret[MAXPATHLEN];

    j = ret;
    i = name;
    lcCompose[0] = ret[0] = '\0';
    while (*i && j - ret < MAXPATHLEN - 1) {
	if (*i == '%') {
	    i++;
	    switch (*i) {
	    case '%':
		*j++ = '%';
		break;
	    case 'H':
		home = getenv("HOME");
		if (home) {
		    strlcat(ret, home, sizeof(ret));
		    j += strlen(home);
		}
		break;
	    case 'L':
		get_compose_filename(lcCompose, sizeof(lcCompose));
		if (lcCompose[0] != '\0') {
		    strlcat(ret, lcCompose, sizeof(ret));
		    j += strlen(lcCompose);
		}
		break;
	    }
	    i++;
	} else {
	    *j++ = *i++;
	}
	*j = '\0';
    }
    strlcpy(transname, ret, len);

    return 1;
}

#ifndef MB_LEN_MAX
#define MB_LEN_MAX 6
#endif

const char *
QUimInputContext::get_encoding()
{
    QTextCodec *codec = QTextCodec::codecForLocale();

    return codec->name();
}

int
QUimInputContext::get_lang_region(char *locale, size_t len)
{
    char *p;

    strlcpy(locale, setlocale(LC_CTYPE, NULL), len);
    if (locale[0] == '\0') {
        return 0;
    }

    p = strrchr(locale, '.');
    if (p)
        *p = '\0';

    return 1;
}

int
QUimInputContext::get_mb_string(char *buf, unsigned int ks)
{
    int len;
    const char *mb;
    unsigned int ucs;

    ucs = KeySymToUcs4(ks);
    QString qs = QString(QChar(ucs));
    mb = (const char *)qs.local8Bit();
    if (!mb)
	return 0;
    len = strlen(mb);
    strlcpy(buf, mb, MB_LEN_MAX + 1);

    return len;
}

#define AllMask (ShiftMask | LockMask | ControlMask | Mod1Mask)
#define LOCAL_UTF8_BUFSIZE 256
#define SEQUENCE_MAX    10

int
QUimInputContext::parse_compose_line(FILE *fp, char **tokenbuf, size_t *buflen)
{
    int token;
    unsigned modifier_mask;
    unsigned modifier;
    unsigned tmp;
    KeySym keysym = NoSymbol;
    DefTree **top = &mTreeTop;
    DefTree *p = NULL;
    Bool exclam, tilde;
    KeySym rhs_keysym = 0;
    char *rhs_string_mb;
    int l;
    int lastch = 0;
    char local_mb_buf[MB_LEN_MAX + 1];
    char *rhs_string_utf8;

    struct DefBuffer {
	unsigned modifier_mask;
	unsigned modifier;
	KeySym keysym;
    };

    struct DefBuffer buf[SEQUENCE_MAX];
    int i, n;
    QTextCodec *codec = QTextCodec::codecForLocale();
    QString qs;

    do {
	token = nexttoken(fp, tokenbuf, &lastch, buflen);
    } while (token == ENDOFLINE);

    if (token == ENDOFFILE) {
	return(-1);
    }

    n = 0;
    do {
	if ((token == KEY) && (strcmp("include", *tokenbuf) == 0)) {
	    char filename[MAXPATHLEN];
	    FILE *infp;
	    token = nexttoken(fp, tokenbuf, &lastch, buflen);
	    if (token != KEY && token != STRING)
		goto error;

	    if (!TransFileName(filename, *tokenbuf, sizeof(filename)) || filename[0] == '\0')
		goto error;
	    infp = fopen(filename, "r");
	    if (infp == NULL)
		goto error;
	    ParseComposeStringFile(infp);
	    fclose(infp);
	    return 0;
	} else if ((token == KEY) && (strcmp("None", *tokenbuf) == 0)) {
	    modifier = 0;
	    modifier_mask = AllMask;
	    token = nexttoken(fp, tokenbuf, &lastch, buflen);
	} else {
	    modifier_mask = modifier = 0;
	    exclam = False;
	    if (token == EXCLAM) {
		exclam = True;
		token = nexttoken(fp, tokenbuf, &lastch, buflen);
	    }
	    while (token == TILDE || token == KEY) {
		tilde = False;
		if (token == TILDE) {
		    tilde = True;
		    token = nexttoken(fp, tokenbuf, &lastch, buflen);
		    if (token != KEY)
			goto error;
		}
		tmp = modmask(*tokenbuf);
		if (!tmp) {
		    goto error;
		}
		modifier_mask |= tmp;
		if (tilde) {
		    modifier &= ~tmp;
		} else {
		    modifier |= tmp;
		}
		token = nexttoken(fp, tokenbuf, &lastch, buflen);
	    }
	    if (exclam) {
		modifier_mask = AllMask;

	    }
	}

	if (token != LESS) {
	    goto error;
	}

	token = nexttoken(fp, tokenbuf, &lastch, buflen);
	if (token != KEY) {
	    goto error;
	}

	token = nexttoken(fp, tokenbuf, &lastch, buflen);
	if (token != GREATER) {
	    goto error;
	}

	keysym = XStringToKeysym(*tokenbuf);
	if (keysym == NoSymbol) {
	    goto error;
	}

	buf[n].keysym = keysym;
	buf[n].modifier = modifier;
	buf[n].modifier_mask = modifier_mask;
	n++;
	if (n >= SEQUENCE_MAX)
	    goto error;
	token = nexttoken(fp, tokenbuf, &lastch, buflen);
    } while (token != COLON);

    token = nexttoken(fp, tokenbuf, &lastch, buflen);
    if (token == STRING) {
	if ((rhs_string_mb = (char *)malloc(strlen(*tokenbuf) + 1)) == NULL)
	    goto error;
	strcpy(rhs_string_mb, *tokenbuf);
	token = nexttoken(fp, tokenbuf, &lastch, buflen);
	if (token == KEY) {
	    rhs_keysym = XStringToKeysym(*tokenbuf);
	    if (rhs_keysym == NoSymbol) {
		free(rhs_string_mb);
		goto error;
	    }
	    token = nexttoken(fp, tokenbuf, &lastch, buflen);
	}
	if (token != ENDOFLINE && token != ENDOFFILE) {
	    free(rhs_string_mb);
	    goto error;
	}
    } else if (token == KEY) {
	rhs_keysym = XStringToKeysym(*tokenbuf);
	if (rhs_keysym == NoSymbol) {
	    goto error;
	}
	token = nexttoken(fp, tokenbuf, &lastch, buflen);
	if (token != ENDOFLINE && token != ENDOFFILE) {
	    goto error;
	}

	l = get_mb_string(local_mb_buf, rhs_keysym);
	if (l == 0) {
	    rhs_string_mb = (char *)malloc(1);
	} else {
	    rhs_string_mb = (char *)malloc(l + 1);
	}
	if (rhs_string_mb == NULL) {
	    goto error;
	}
	memcpy(rhs_string_mb, local_mb_buf, l);
	rhs_string_mb[l] = '\0';
    } else {
	goto error;
    }

    qs = codec->toUnicode(rhs_string_mb);
    rhs_string_utf8 = strdup((const char *)qs.utf8());

    for (i = 0; i < n; i++) {
	for (p = *top; p; p = p->next) {
	    if (buf[i].keysym == p->keysym &&
		buf[i].modifier == p->modifier &&
		buf[i].modifier_mask == p->modifier_mask) {
		break;
	    }
	}
	if (p) {
	    top = &p->succession;
	} else {
	    if ((p = (DefTree*)malloc(sizeof(DefTree))) == NULL) {
		free(rhs_string_mb);
		goto error;
	    }
	    p->keysym = buf[i].keysym;
	    p->modifier = buf[i].modifier;
	    p->modifier_mask = buf[i].modifier_mask;
	    p->succession = NULL;
	    p->next = *top;
	    p->mb = NULL;
	    p->utf8 = NULL;
	    p->ks = NoSymbol;
	    *top = p;
	    top = &p->succession;
	}
    }

    free(p->mb);
    p->mb = rhs_string_mb;
    free(p->utf8);
    p->utf8 = rhs_string_utf8;
    p->ks = rhs_keysym;
    return n;
error:
    while (token != ENDOFLINE && token != ENDOFFILE) {
	token = nexttoken(fp, tokenbuf, &lastch, buflen);
    }
    return 0;
}

void
QUimInputContext::FreeComposeTree(DefTree *top)
{
    if (!top)
        return;

    if (top->succession)
        FreeComposeTree(top->succession);
    if (top->next)
        FreeComposeTree(top->next);
    free(top->mb);
    free(top->utf8);
    free(top);
}

void
QUimInputContext::ParseComposeStringFile(FILE *fp)
{
    char *tbp, *p[1];
    struct stat st;
    size_t buflen = BUFSIZ;

    if (fstat(fileno(fp), &st) != -1 && S_ISREG(st.st_mode) &&
	st.st_size > 0) {

	tbp = (char *)malloc(buflen);
	p[0] = tbp;
	if (tbp != NULL) {
	    while (parse_compose_line(fp, p, &buflen) >= 0) {
	    }
	    free(p[0]);
	}
    }
}

void QUimInputContext::create_compose_tree()
{
    FILE *fp = NULL;
    char name[MAXPATHLEN];
    char lang_region[BUFSIZ];
    const char *encoding;
    char *compose_env;
    int ret;

    name[0] = '\0';
    compose_env = getenv("XCOMPOSEFILE");

    if (compose_env != NULL) {
	strlcpy(name, compose_env, sizeof(name));
    } else {
	char *home = getenv("HOME");
	if (home != NULL) {
	    snprintf(name, sizeof(name), "%s/.XCompose", home);
	    fp = fopen(name, "r");
	    if (fp == NULL)
		name[0] = '\0';
	}
    }

    if (name[0] == '\0' && !get_compose_filename(name, sizeof(name))) {
        if (fp)
            fclose(fp);
	return;
    }

    if (fp == NULL && ((fp = fopen(name, "r")) == NULL))
	return;

    ret = get_lang_region(lang_region, sizeof(lang_region));
    encoding = get_encoding();
    if (!ret || encoding == NULL) {
	fprintf(stderr, "Warning: locale name is NULL\n");
	fclose(fp);
	return;
    }

    ParseComposeStringFile(fp);
    fclose(fp);
}

int QUimInputContext::get_compose_filename(char *filename, size_t len)
{
    char compose_dir_file[MAXPATHLEN], name[MAXPATHLEN];
    char locale[BUFSIZ];
    const char *encoding;
    char lang_region[BUFSIZ];
    int ret;

    FILE *fp;
    char buf[XLC_BUFSIZE];
    const char *xlib_dir = XLIB_DIR ;

    ret = get_lang_region(lang_region, sizeof(lang_region));
    encoding = get_encoding();

    if (!ret || encoding == NULL)
	return 0;

    snprintf(locale, sizeof(locale), "%s.%s", lang_region, encoding);
    snprintf(compose_dir_file, sizeof(compose_dir_file), "%s/%s", XLIB_DIR, COMPOSE_DIR_FILE);

    fp = fopen(compose_dir_file, "r");
    if (fp == NULL) {
	/* retry with fallback file */
	if (strcmp(FALLBACK_XLIB_DIR, XLIB_DIR)) {
	    snprintf(compose_dir_file, sizeof(compose_dir_file), "%s/%s",
			    FALLBACK_XLIB_DIR, COMPOSE_DIR_FILE);
	    fp = fopen(compose_dir_file, "r");
	    if (fp == NULL)
		return 0;
	    xlib_dir = FALLBACK_XLIB_DIR;
	} else
	    return 0;
    }

    name[0] = '\0';
    while (fgets(buf, XLC_BUFSIZE, fp) != NULL) {
	char *p = buf;
	int n;
	char *args[2], *from, *to;
	while ((unsigned char)isspace(*p)) {
	    ++p;
	}
	if (iscomment(*p)) {
	    continue;
	}
	n = parse_line(p, args, 2);
	if (n != 2) {
	    continue;
	}
	from = args[1], to = args[0];
	if (!strcmp(from, locale)) {
	    strlcpy(name, to, sizeof(name));
	    break;
	}
    }
    fclose(fp);

    if (name[0] == '\0')
	return 0;

    snprintf(filename, len, "%s/%s/%s", xlib_dir, XLOCALE_DIR, name);

    return 1;
}

static int
parse_line(char *line, char **argv, int argsize)
{
    int argc = 0;
    char *p = line;

    while (argc < argsize) {
	while ((unsigned char)isspace(*p)) {
	    ++p;
	}
	if (*p == '\0') {
	    break;
	}
	argv[argc++] = p;
	while (*p != ':' && *p != '\n' && *p != '\0') {
	    ++p;
	}
	if (*p == '\0') {
	    break;
	}
	*p++ = '\0';
    }

    return argc;
}

static unsigned short const keysym_to_unicode_1a1_1ff[] = {
	    0x0104, 0x02d8, 0x0141, 0x0000, 0x013d, 0x015a, 0x0000, /* 0x01a0-0x01a7 */
    0x0000, 0x0160, 0x015e, 0x0164, 0x0179, 0x0000, 0x017d, 0x017b, /* 0x01a8-0x01af */
    0x0000, 0x0105, 0x02db, 0x0142, 0x0000, 0x013e, 0x015b, 0x02c7, /* 0x01b0-0x01b7 */
    0x0000, 0x0161, 0x015f, 0x0165, 0x017a, 0x02dd, 0x017e, 0x017c, /* 0x01b8-0x01bf */
    0x0154, 0x0000, 0x0000, 0x0102, 0x0000, 0x0139, 0x0106, 0x0000, /* 0x01c0-0x01c7 */
    0x010c, 0x0000, 0x0118, 0x0000, 0x011a, 0x0000, 0x0000, 0x010e, /* 0x01c8-0x01cf */
    0x0110, 0x0143, 0x0147, 0x0000, 0x0000, 0x0150, 0x0000, 0x0000, /* 0x01d0-0x01d7 */
    0x0158, 0x016e, 0x0000, 0x0170, 0x0000, 0x0000, 0x0162, 0x0000, /* 0x01d8-0x01df */
    0x0155, 0x0000, 0x0000, 0x0103, 0x0000, 0x013a, 0x0107, 0x0000, /* 0x01e0-0x01e7 */
    0x010d, 0x0000, 0x0119, 0x0000, 0x011b, 0x0000, 0x0000, 0x010f, /* 0x01e8-0x01ef */
    0x0111, 0x0144, 0x0148, 0x0000, 0x0000, 0x0151, 0x0000, 0x0000, /* 0x01f0-0x01f7 */
    0x0159, 0x016f, 0x0000, 0x0171, 0x0000, 0x0000, 0x0163, 0x02d9  /* 0x01f8-0x01ff */
};

static unsigned short const keysym_to_unicode_2a1_2fe[] = {
	    0x0126, 0x0000, 0x0000, 0x0000, 0x0000, 0x0124, 0x0000, /* 0x02a0-0x02a7 */
    0x0000, 0x0130, 0x0000, 0x011e, 0x0134, 0x0000, 0x0000, 0x0000, /* 0x02a8-0x02af */
    0x0000, 0x0127, 0x0000, 0x0000, 0x0000, 0x0000, 0x0125, 0x0000, /* 0x02b0-0x02b7 */
    0x0000, 0x0131, 0x0000, 0x011f, 0x0135, 0x0000, 0x0000, 0x0000, /* 0x02b8-0x02bf */
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x010a, 0x0108, 0x0000, /* 0x02c0-0x02c7 */
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, /* 0x02c8-0x02cf */
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0120, 0x0000, 0x0000, /* 0x02d0-0x02d7 */
    0x011c, 0x0000, 0x0000, 0x0000, 0x0000, 0x016c, 0x015c, 0x0000, /* 0x02d8-0x02df */
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x010b, 0x0109, 0x0000, /* 0x02e0-0x02e7 */
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, /* 0x02e8-0x02ef */
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0121, 0x0000, 0x0000, /* 0x02f0-0x02f7 */
    0x011d, 0x0000, 0x0000, 0x0000, 0x0000, 0x016d, 0x015d	  /* 0x02f8-0x02ff */
};

static unsigned short const keysym_to_unicode_3a2_3fe[] = {
		    0x0138, 0x0156, 0x0000, 0x0128, 0x013b, 0x0000, /* 0x03a0-0x03a7 */
    0x0000, 0x0000, 0x0112, 0x0122, 0x0166, 0x0000, 0x0000, 0x0000, /* 0x03a8-0x03af */
    0x0000, 0x0000, 0x0000, 0x0157, 0x0000, 0x0129, 0x013c, 0x0000, /* 0x03b0-0x03b7 */
    0x0000, 0x0000, 0x0113, 0x0123, 0x0167, 0x014a, 0x0000, 0x014b, /* 0x03b8-0x03bf */
    0x0100, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x012e, /* 0x03c0-0x03c7 */
    0x0000, 0x0000, 0x0000, 0x0000, 0x0116, 0x0000, 0x0000, 0x012a, /* 0x03c8-0x03cf */
    0x0000, 0x0145, 0x014c, 0x0136, 0x0000, 0x0000, 0x0000, 0x0000, /* 0x03d0-0x03d7 */
    0x0000, 0x0172, 0x0000, 0x0000, 0x0000, 0x0168, 0x016a, 0x0000, /* 0x03d8-0x03df */
    0x0101, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x012f, /* 0x03e0-0x03e7 */
    0x0000, 0x0000, 0x0000, 0x0000, 0x0117, 0x0000, 0x0000, 0x012b, /* 0x03e8-0x03ef */
    0x0000, 0x0146, 0x014d, 0x0137, 0x0000, 0x0000, 0x0000, 0x0000, /* 0x03f0-0x03f7 */
    0x0000, 0x0173, 0x0000, 0x0000, 0x0000, 0x0169, 0x016b	  /* 0x03f8-0x03ff */
};

static unsigned short const keysym_to_unicode_4a1_4df[] = {
	    0x3002, 0x3008, 0x3009, 0x3001, 0x30fb, 0x30f2, 0x30a1, /* 0x04a0-0x04a7 */
    0x30a3, 0x30a5, 0x30a7, 0x30a9, 0x30e3, 0x30e5, 0x30e7, 0x30c3, /* 0x04a8-0x04af */
    0x30fc, 0x30a2, 0x30a4, 0x30a6, 0x30a8, 0x30aa, 0x30ab, 0x30ad, /* 0x04b0-0x04b7 */
    0x30af, 0x30b1, 0x30b3, 0x30b5, 0x30b7, 0x30b9, 0x30bb, 0x30bd, /* 0x04b8-0x04bf */
    0x30bf, 0x30c1, 0x30c4, 0x30c6, 0x30c8, 0x30ca, 0x30cb, 0x30cc, /* 0x04c0-0x04c7 */
    0x30cd, 0x30ce, 0x30cf, 0x30d2, 0x30d5, 0x30d8, 0x30db, 0x30de, /* 0x04c8-0x04cf */
    0x30df, 0x30e0, 0x30e1, 0x30e2, 0x30e4, 0x30e6, 0x30e8, 0x30e9, /* 0x04d0-0x04d7 */
    0x30ea, 0x30eb, 0x30ec, 0x30ed, 0x30ef, 0x30f3, 0x309b, 0x309c  /* 0x04d8-0x04df */
};

static unsigned short const keysym_to_unicode_590_5fe[] = {
    0x06f0, 0x06f1, 0x06f2, 0x06f3, 0x06f4, 0x06f5, 0x06f6, 0x06f7, /* 0x0590-0x0597 */
    0x06f8, 0x06f9, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, /* 0x0598-0x059f */
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x066a, 0x0670, 0x0679, /* 0x05a0-0x05a7 */
	
    0x067e, 0x0686, 0x0688, 0x0691, 0x060c, 0x0000, 0x06d4, 0x0000, /* 0x05ac-0x05af */
    0x0660, 0x0661, 0x0662, 0x0663, 0x0664, 0x0665, 0x0666, 0x0667, /* 0x05b0-0x05b7 */
    0x0668, 0x0669, 0x0000, 0x061b, 0x0000, 0x0000, 0x0000, 0x061f, /* 0x05b8-0x05bf */
    0x0000, 0x0621, 0x0622, 0x0623, 0x0624, 0x0625, 0x0626, 0x0627, /* 0x05c0-0x05c7 */
    0x0628, 0x0629, 0x062a, 0x062b, 0x062c, 0x062d, 0x062e, 0x062f, /* 0x05c8-0x05cf */
    0x0630, 0x0631, 0x0632, 0x0633, 0x0634, 0x0635, 0x0636, 0x0637, /* 0x05d0-0x05d7 */
    0x0638, 0x0639, 0x063a, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, /* 0x05d8-0x05df */
    0x0640, 0x0641, 0x0642, 0x0643, 0x0644, 0x0645, 0x0646, 0x0647, /* 0x05e0-0x05e7 */
    0x0648, 0x0649, 0x064a, 0x064b, 0x064c, 0x064d, 0x064e, 0x064f, /* 0x05e8-0x05ef */
    0x0650, 0x0651, 0x0652, 0x0653, 0x0654, 0x0655, 0x0698, 0x06a4, /* 0x05f0-0x05f7 */
    0x06a9, 0x06af, 0x06ba, 0x06be, 0x06cc, 0x06d2, 0x06c1	  /* 0x05f8-0x05fe */
};

static unsigned short keysym_to_unicode_680_6ff[] = {
    0x0492, 0x0496, 0x049a, 0x049c, 0x04a2, 0x04ae, 0x04b0, 0x04b2, /* 0x0680-0x0687 */
    0x04b6, 0x04b8, 0x04ba, 0x0000, 0x04d8, 0x04e2, 0x04e8, 0x04ee, /* 0x0688-0x068f */
    0x0493, 0x0497, 0x049b, 0x049d, 0x04a3, 0x04af, 0x04b1, 0x04b3, /* 0x0690-0x0697 */
    0x04b7, 0x04b9, 0x04bb, 0x0000, 0x04d9, 0x04e3, 0x04e9, 0x04ef, /* 0x0698-0x069f */
    0x0000, 0x0452, 0x0453, 0x0451, 0x0454, 0x0455, 0x0456, 0x0457, /* 0x06a0-0x06a7 */
    0x0458, 0x0459, 0x045a, 0x045b, 0x045c, 0x0491, 0x045e, 0x045f, /* 0x06a8-0x06af */
    0x2116, 0x0402, 0x0403, 0x0401, 0x0404, 0x0405, 0x0406, 0x0407, /* 0x06b0-0x06b7 */
    0x0408, 0x0409, 0x040a, 0x040b, 0x040c, 0x0490, 0x040e, 0x040f, /* 0x06b8-0x06bf */
    0x044e, 0x0430, 0x0431, 0x0446, 0x0434, 0x0435, 0x0444, 0x0433, /* 0x06c0-0x06c7 */
    0x0445, 0x0438, 0x0439, 0x043a, 0x043b, 0x043c, 0x043d, 0x043e, /* 0x06c8-0x06cf */
    0x043f, 0x044f, 0x0440, 0x0441, 0x0442, 0x0443, 0x0436, 0x0432, /* 0x06d0-0x06d7 */
    0x044c, 0x044b, 0x0437, 0x0448, 0x044d, 0x0449, 0x0447, 0x044a, /* 0x06d8-0x06df */
    0x042e, 0x0410, 0x0411, 0x0426, 0x0414, 0x0415, 0x0424, 0x0413, /* 0x06e0-0x06e7 */
    0x0425, 0x0418, 0x0419, 0x041a, 0x041b, 0x041c, 0x041d, 0x041e, /* 0x06e8-0x06ef */
    0x041f, 0x042f, 0x0420, 0x0421, 0x0422, 0x0423, 0x0416, 0x0412, /* 0x06f0-0x06f7 */
    0x042c, 0x042b, 0x0417, 0x0428, 0x042d, 0x0429, 0x0427, 0x042a  /* 0x06f8-0x06ff */
};

static unsigned short const keysym_to_unicode_7a1_7f9[] = {
	    0x0386, 0x0388, 0x0389, 0x038a, 0x03aa, 0x0000, 0x038c, /* 0x07a0-0x07a7 */
    0x038e, 0x03ab, 0x0000, 0x038f, 0x0000, 0x0000, 0x0385, 0x2015, /* 0x07a8-0x07af */
    0x0000, 0x03ac, 0x03ad, 0x03ae, 0x03af, 0x03ca, 0x0390, 0x03cc, /* 0x07b0-0x07b7 */
    0x03cd, 0x03cb, 0x03b0, 0x03ce, 0x0000, 0x0000, 0x0000, 0x0000, /* 0x07b8-0x07bf */
    0x0000, 0x0391, 0x0392, 0x0393, 0x0394, 0x0395, 0x0396, 0x0397, /* 0x07c0-0x07c7 */
    0x0398, 0x0399, 0x039a, 0x039b, 0x039c, 0x039d, 0x039e, 0x039f, /* 0x07c8-0x07cf */
    0x03a0, 0x03a1, 0x03a3, 0x0000, 0x03a4, 0x03a5, 0x03a6, 0x03a7, /* 0x07d0-0x07d7 */
    0x03a8, 0x03a9, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, /* 0x07d8-0x07df */
    0x0000, 0x03b1, 0x03b2, 0x03b3, 0x03b4, 0x03b5, 0x03b6, 0x03b7, /* 0x07e0-0x07e7 */
    0x03b8, 0x03b9, 0x03ba, 0x03bb, 0x03bc, 0x03bd, 0x03be, 0x03bf, /* 0x07e8-0x07ef */
    0x03c0, 0x03c1, 0x03c3, 0x03c2, 0x03c4, 0x03c5, 0x03c6, 0x03c7, /* 0x07f0-0x07f7 */
    0x03c8, 0x03c9						  /* 0x07f8-0x07ff */
};

static unsigned short const keysym_to_unicode_8a4_8fe[] = {
				    0x2320, 0x2321, 0x0000, 0x231c, /* 0x08a0-0x08a7 */
    0x231d, 0x231e, 0x231f, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, /* 0x08a8-0x08af */
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, /* 0x08b0-0x08b7 */
    0x0000, 0x0000, 0x0000, 0x0000, 0x2264, 0x2260, 0x2265, 0x222b, /* 0x08b8-0x08bf */
    0x2234, 0x0000, 0x221e, 0x0000, 0x0000, 0x2207, 0x0000, 0x0000, /* 0x08c0-0x08c7 */
    0x2245, 0x2246, 0x0000, 0x0000, 0x0000, 0x0000, 0x22a2, 0x0000, /* 0x08c8-0x08cf */
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x221a, 0x0000, /* 0x08d0-0x08d7 */
    0x0000, 0x0000, 0x2282, 0x2283, 0x2229, 0x222a, 0x2227, 0x2228, /* 0x08d8-0x08df */
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, /* 0x08e0-0x08e7 */
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, /* 0x08e8-0x08ef */
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0192, 0x0000, /* 0x08f0-0x08f7 */
    0x0000, 0x0000, 0x0000, 0x2190, 0x2191, 0x2192, 0x2193	  /* 0x08f8-0x08ff */
};

static unsigned short const keysym_to_unicode_9df_9f8[] = {
							    0x2422, /* 0x09d8-0x09df */
    0x2666, 0x25a6, 0x2409, 0x240c, 0x240d, 0x240a, 0x0000, 0x0000, /* 0x09e0-0x09e7 */
    0x240a, 0x240b, 0x2518, 0x2510, 0x250c, 0x2514, 0x253c, 0x2500, /* 0x09e8-0x09ef */
    0x0000, 0x0000, 0x0000, 0x0000, 0x251c, 0x2524, 0x2534, 0x252c, /* 0x09f0-0x09f7 */
    0x2502							  /* 0x09f8-0x09ff */
};

static unsigned short const keysym_to_unicode_aa1_afe[] = {
	    0x2003, 0x2002, 0x2004, 0x2005, 0x2007, 0x2008, 0x2009, /* 0x0aa0-0x0aa7 */
    0x200a, 0x2014, 0x2013, 0x0000, 0x0000, 0x0000, 0x2026, 0x2025, /* 0x0aa8-0x0aaf */
    0x2153, 0x2154, 0x2155, 0x2156, 0x2157, 0x2158, 0x2159, 0x215a, /* 0x0ab0-0x0ab7 */
    0x2105, 0x0000, 0x0000, 0x2012, 0x2039, 0x2024, 0x203a, 0x0000, /* 0x0ab8-0x0abf */
    0x0000, 0x0000, 0x0000, 0x215b, 0x215c, 0x215d, 0x215e, 0x0000, /* 0x0ac0-0x0ac7 */
    0x0000, 0x2122, 0x2120, 0x0000, 0x25c1, 0x25b7, 0x25cb, 0x25ad, /* 0x0ac8-0x0acf */
    0x2018, 0x2019, 0x201c, 0x201d, 0x211e, 0x0000, 0x2032, 0x2033, /* 0x0ad0-0x0ad7 */
    0x0000, 0x271d, 0x0000, 0x220e, 0x25c2, 0x2023, 0x25cf, 0x25ac, /* 0x0ad8-0x0adf */
    0x25e6, 0x25ab, 0x25ae, 0x25b5, 0x25bf, 0x2606, 0x2022, 0x25aa, /* 0x0ae0-0x0ae7 */
    0x25b4, 0x25be, 0x261a, 0x261b, 0x2663, 0x2666, 0x2665, 0x0000, /* 0x0ae8-0x0aef */
    0x2720, 0x2020, 0x2021, 0x2713, 0x2612, 0x266f, 0x266d, 0x2642, /* 0x0af0-0x0af7 */
    0x2640, 0x2121, 0x2315, 0x2117, 0x2038, 0x201a, 0x201e	  /* 0x0af8-0x0aff */
};

/* none of the APL keysyms match the Unicode characters */

static unsigned short const keysym_to_unicode_cdf_cfa[] = {
							    0x2017, /* 0x0cd8-0x0cdf */
    0x05d0, 0x05d1, 0x05d2, 0x05d3, 0x05d4, 0x05d5, 0x05d6, 0x05d7, /* 0x0ce0-0x0ce7 */
    0x05d8, 0x05d9, 0x05da, 0x05db, 0x05dc, 0x05dd, 0x05de, 0x05df, /* 0x0ce8-0x0cef */
    0x05e0, 0x05e1, 0x05e2, 0x05e3, 0x05e4, 0x05e5, 0x05e6, 0x05e7, /* 0x0cf0-0x0cf7 */
    0x05e8, 0x05e9, 0x05ea					  /* 0x0cf8-0x0cff */
};

static unsigned short const keysym_to_unicode_da1_df9[] = {
	    0x0e01, 0x0e02, 0x0e03, 0x0e04, 0x0e05, 0x0e06, 0x0e07, /* 0x0da0-0x0da7 */
    0x0e08, 0x0e09, 0x0e0a, 0x0e0b, 0x0e0c, 0x0e0d, 0x0e0e, 0x0e0f, /* 0x0da8-0x0daf */
    0x0e10, 0x0e11, 0x0e12, 0x0e13, 0x0e14, 0x0e15, 0x0e16, 0x0e17, /* 0x0db0-0x0db7 */
    0x0e18, 0x0e19, 0x0e1a, 0x0e1b, 0x0e1c, 0x0e1d, 0x0e1e, 0x0e1f, /* 0x0db8-0x0dbf */
    0x0e20, 0x0e21, 0x0e22, 0x0e23, 0x0e24, 0x0e25, 0x0e26, 0x0e27, /* 0x0dc0-0x0dc7 */
    0x0e28, 0x0e29, 0x0e2a, 0x0e2b, 0x0e2c, 0x0e2d, 0x0e2e, 0x0e2f, /* 0x0dc8-0x0dcf */
    0x0e30, 0x0e31, 0x0e32, 0x0e33, 0x0e34, 0x0e35, 0x0e36, 0x0e37, /* 0x0dd0-0x0dd7 */
    0x0e38, 0x0e39, 0x0e3a, 0x0000, 0x0000, 0x0000, 0x0e3e, 0x0e3f, /* 0x0dd8-0x0ddf */
    0x0e40, 0x0e41, 0x0e42, 0x0e43, 0x0e44, 0x0e45, 0x0e46, 0x0e47, /* 0x0de0-0x0de7 */
    0x0e48, 0x0e49, 0x0e4a, 0x0e4b, 0x0e4c, 0x0e4d, 0x0000, 0x0000, /* 0x0de8-0x0def */
    0x0e50, 0x0e51, 0x0e52, 0x0e53, 0x0e54, 0x0e55, 0x0e56, 0x0e57, /* 0x0df0-0x0df7 */
    0x0e58, 0x0e59						  /* 0x0df8-0x0dff */
};

static unsigned short const keysym_to_unicode_ea0_eff[] = {
    0x0000, 0x1101, 0x1101, 0x11aa, 0x1102, 0x11ac, 0x11ad, 0x1103, /* 0x0ea0-0x0ea7 */
    0x1104, 0x1105, 0x11b0, 0x11b1, 0x11b2, 0x11b3, 0x11b4, 0x11b5, /* 0x0ea8-0x0eaf */
    0x11b6, 0x1106, 0x1107, 0x1108, 0x11b9, 0x1109, 0x110a, 0x110b, /* 0x0eb0-0x0eb7 */
    0x110c, 0x110d, 0x110e, 0x110f, 0x1110, 0x1111, 0x1112, 0x1161, /* 0x0eb8-0x0ebf */
    0x1162, 0x1163, 0x1164, 0x1165, 0x1166, 0x1167, 0x1168, 0x1169, /* 0x0ec0-0x0ec7 */
    0x116a, 0x116b, 0x116c, 0x116d, 0x116e, 0x116f, 0x1170, 0x1171, /* 0x0ec8-0x0ecf */
    0x1172, 0x1173, 0x1174, 0x1175, 0x11a8, 0x11a9, 0x11aa, 0x11ab, /* 0x0ed0-0x0ed7 */
    0x11ac, 0x11ad, 0x11ae, 0x11af, 0x11b0, 0x11b1, 0x11b2, 0x11b3, /* 0x0ed8-0x0edf */
    0x11b4, 0x11b5, 0x11b6, 0x11b7, 0x11b8, 0x11b9, 0x11ba, 0x11bb, /* 0x0ee0-0x0ee7 */
    0x11bc, 0x11bd, 0x11be, 0x11bf, 0x11c0, 0x11c1, 0x11c2, 0x0000, /* 0x0ee8-0x0eef */
    0x0000, 0x0000, 0x1140, 0x0000, 0x0000, 0x1159, 0x119e, 0x0000, /* 0x0ef0-0x0ef7 */
    0x11eb, 0x0000, 0x11f9, 0x0000, 0x0000, 0x0000, 0x0000, 0x20a9, /* 0x0ef8-0x0eff */
};

static unsigned short keysym_to_unicode_12a1_12fe[] = {
	    0x1e02, 0x1e03, 0x0000, 0x0000, 0x0000, 0x1e0a, 0x0000, /* 0x12a0-0x12a7 */
    0x1e80, 0x0000, 0x1e82, 0x1e0b, 0x1ef2, 0x0000, 0x0000, 0x0000, /* 0x12a8-0x12af */
    0x1e1e, 0x1e1f, 0x0000, 0x0000, 0x1e40, 0x1e41, 0x0000, 0x1e56, /* 0x12b0-0x12b7 */
    0x1e81, 0x1e57, 0x1e83, 0x1e60, 0x1ef3, 0x1e84, 0x1e85, 0x1e61, /* 0x12b8-0x12bf */
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, /* 0x12c0-0x12c7 */
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, /* 0x12c8-0x12cf */
    0x0174, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x1e6a, /* 0x12d0-0x12d7 */
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0176, 0x0000, /* 0x12d8-0x12df */
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, /* 0x12e0-0x12e7 */
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, /* 0x12e8-0x12ef */
    0x0175, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x1e6b, /* 0x12f0-0x12f7 */
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0177	  /* 0x12f0-0x12ff */
};
		
static unsigned short const keysym_to_unicode_13bc_13be[] = {
				    0x0152, 0x0153, 0x0178	  /* 0x13b8-0x13bf */
};

static unsigned short keysym_to_unicode_14a1_14ff[] = {
	    0x2741, 0x00a7, 0x0589, 0x0029, 0x0028, 0x00bb, 0x00ab, /* 0x14a0-0x14a7 */
    0x2014, 0x002e, 0x055d, 0x002c, 0x2013, 0x058a, 0x2026, 0x055c, /* 0x14a8-0x14af */
    0x055b, 0x055e, 0x0531, 0x0561, 0x0532, 0x0562, 0x0533, 0x0563, /* 0x14b0-0x14b7 */
    0x0534, 0x0564, 0x0535, 0x0565, 0x0536, 0x0566, 0x0537, 0x0567, /* 0x14b8-0x14bf */
    0x0538, 0x0568, 0x0539, 0x0569, 0x053a, 0x056a, 0x053b, 0x056b, /* 0x14c0-0x14c7 */
    0x053c, 0x056c, 0x053d, 0x056d, 0x053e, 0x056e, 0x053f, 0x056f, /* 0x14c8-0x14cf */
    0x0540, 0x0570, 0x0541, 0x0571, 0x0542, 0x0572, 0x0543, 0x0573, /* 0x14d0-0x14d7 */
    0x0544, 0x0574, 0x0545, 0x0575, 0x0546, 0x0576, 0x0547, 0x0577, /* 0x14d8-0x14df */
    0x0548, 0x0578, 0x0549, 0x0579, 0x054a, 0x057a, 0x054b, 0x057b, /* 0x14e0-0x14e7 */
    0x054c, 0x057c, 0x054d, 0x057d, 0x054e, 0x057e, 0x054f, 0x057f, /* 0x14e8-0x14ef */
    0x0550, 0x0580, 0x0551, 0x0581, 0x0552, 0x0582, 0x0553, 0x0583, /* 0x14f0-0x14f7 */
    0x0554, 0x0584, 0x0555, 0x0585, 0x0556, 0x0586, 0x2019, 0x0027, /* 0x14f8-0x14ff */
};

static unsigned short keysym_to_unicode_15d0_15f6[] = {
    0x10d0, 0x10d1, 0x10d2, 0x10d3, 0x10d4, 0x10d5, 0x10d6, 0x10d7, /* 0x15d0-0x15d7 */
    0x10d8, 0x10d9, 0x10da, 0x10db, 0x10dc, 0x10dd, 0x10de, 0x10df, /* 0x15d8-0x15df */
    0x10e0, 0x10e1, 0x10e2, 0x10e3, 0x10e4, 0x10e5, 0x10e6, 0x10e7, /* 0x15e0-0x15e7 */
    0x10e8, 0x10e9, 0x10ea, 0x10eb, 0x10ec, 0x10ed, 0x10ee, 0x10ef, /* 0x15e8-0x15ef */
    0x10f0, 0x10f1, 0x10f2, 0x10f3, 0x10f4, 0x10f5, 0x10f6	  /* 0x15f0-0x15f7 */
};

static unsigned short keysym_to_unicode_16a0_16f6[] = {
    0x0000, 0x0000, 0xf0a2, 0x1e8a, 0x0000, 0xf0a5, 0x012c, 0xf0a7, /* 0x16a0-0x16a7 */
    0xf0a8, 0x01b5, 0x01e6, 0x0000, 0x0000, 0x0000, 0x0000, 0x019f, /* 0x16a8-0x16af */
    0x0000, 0x0000, 0xf0b2, 0x1e8b, 0x01d1, 0xf0b5, 0x012d, 0xf0b7, /* 0x16b0-0x16b7 */
    0xf0b8, 0x01b6, 0x01e7, 0x0000, 0x0000, 0x01d2, 0x0000, 0x0275, /* 0x16b8-0x16bf */
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x018f, 0x0000, /* 0x16c0-0x16c7 */
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, /* 0x16c8-0x16cf */
    0x0000, 0x1e36, 0xf0d2, 0xf0d3, 0x0000, 0x0000, 0x0000, 0x0000, /* 0x16d0-0x16d7 */
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, /* 0x16d8-0x16df */
    0x0000, 0x1e37, 0xf0e2, 0xf0e3, 0x0000, 0x0000, 0x0000, 0x0000, /* 0x16e0-0x16e7 */
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, /* 0x16e8-0x16ef */
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0259	  /* 0x16f0-0x16f6 */
};

static unsigned short const keysym_to_unicode_1e9f_1eff[] = {
							    0x0303,
    0x1ea0, 0x1ea1, 0x1ea2, 0x1ea3, 0x1ea4, 0x1ea5, 0x1ea6, 0x1ea7, /* 0x1ea0-0x1ea7 */
    0x1ea8, 0x1ea9, 0x1eaa, 0x1eab, 0x1eac, 0x1ead, 0x1eae, 0x1eaf, /* 0x1ea8-0x1eaf */
    0x1eb0, 0x1eb1, 0x1eb2, 0x1eb3, 0x1eb4, 0x1eb5, 0x1eb6, 0x1eb7, /* 0x1eb0-0x1eb7 */
    0x1eb8, 0x1eb9, 0x1eba, 0x1ebb, 0x1ebc, 0x1ebd, 0x1ebe, 0x1ebf, /* 0x1eb8-0x1ebf */
    0x1ec0, 0x1ec1, 0x1ec2, 0x1ec3, 0x1ec4, 0x1ec5, 0x1ec6, 0x1ec7, /* 0x1ec0-0x1ec7 */
    0x1ec8, 0x1ec9, 0x1eca, 0x1ecb, 0x1ecc, 0x1ecd, 0x1ece, 0x1ecf, /* 0x1ec8-0x1ecf */
    0x1ed0, 0x1ed1, 0x1ed2, 0x1ed3, 0x1ed4, 0x1ed5, 0x1ed6, 0x1ed7, /* 0x1ed0-0x1ed7 */
    0x1ed8, 0x1ed9, 0x1eda, 0x1edb, 0x1edc, 0x1edd, 0x1ede, 0x1edf, /* 0x1ed8-0x1edf */
    0x1ee0, 0x1ee1, 0x1ee2, 0x1ee3, 0x1ee4, 0x1ee5, 0x1ee6, 0x1ee7, /* 0x1ee0-0x1ee7 */
    0x1ee8, 0x1ee9, 0x1eea, 0x1eeb, 0x1eec, 0x1eed, 0x1eee, 0x1eef, /* 0x1ee8-0x1eef */
    0x1ef0, 0x1ef1, 0x0300, 0x0301, 0x1ef4, 0x1ef5, 0x1ef6, 0x1ef7, /* 0x1ef0-0x1ef7 */
    0x1ef8, 0x1ef9, 0x01a0, 0x01a1, 0x01af, 0x01b0, 0x0309, 0x0323  /* 0x1ef8-0x1eff */
};

static unsigned short const keysym_to_unicode_20a0_20ac[] = {
    0x20a0, 0x20a1, 0x20a2, 0x20a3, 0x20a4, 0x20a5, 0x20a6, 0x20a7, /* 0x20a0-0x20a7 */
    0x20a8, 0x20a9, 0x20aa, 0x20ab, 0x20ac			  /* 0x20a8-0x20af */
};

static unsigned int
KeySymToUcs4(KeySym keysym)
{
    /* 'Unicode keysym' */
    if ((keysym & 0xff000000) == 0x01000000)
	return (keysym & 0x00ffffff);

    if (keysym > 0 && keysym < 0x100)
	return keysym;
    else if (keysym > 0x1a0 && keysym < 0x200)
	return keysym_to_unicode_1a1_1ff[keysym - 0x1a1];
    else if (keysym > 0x2a0 && keysym < 0x2ff)
	return keysym_to_unicode_2a1_2fe[keysym - 0x2a1];
    else if (keysym > 0x3a1 && keysym < 0x3ff)
	return keysym_to_unicode_3a2_3fe[keysym - 0x3a2];
    else if (keysym > 0x4a0 && keysym < 0x4e0)
	return keysym_to_unicode_4a1_4df[keysym - 0x4a1];
    else if (keysym > 0x589 && keysym < 0x5ff)
	return keysym_to_unicode_590_5fe[keysym - 0x590];
    else if (keysym > 0x67f && keysym < 0x700)
	return keysym_to_unicode_680_6ff[keysym - 0x680];
    else if (keysym > 0x7a0 && keysym < 0x7fa)
	return keysym_to_unicode_7a1_7f9[keysym - 0x7a1];
    else if (keysym > 0x8a3 && keysym < 0x8ff)
	return keysym_to_unicode_8a4_8fe[keysym - 0x8a4];
    else if (keysym > 0x9de && keysym < 0x9f9)
	return keysym_to_unicode_9df_9f8[keysym - 0x9df];
    else if (keysym > 0xaa0 && keysym < 0xaff)
	return keysym_to_unicode_aa1_afe[keysym - 0xaa1];
    else if (keysym > 0xcde && keysym < 0xcfb)
	return keysym_to_unicode_cdf_cfa[keysym - 0xcdf];
    else if (keysym > 0xda0 && keysym < 0xdfa)
	return keysym_to_unicode_da1_df9[keysym - 0xda1];
    else if (keysym > 0xe9f && keysym < 0xf00)
	return keysym_to_unicode_ea0_eff[keysym - 0xea0];
    else if (keysym > 0x12a0 && keysym < 0x12ff)
	return keysym_to_unicode_12a1_12fe[keysym - 0x12a1];
    else if (keysym > 0x13bb && keysym < 0x13bf)
	return keysym_to_unicode_13bc_13be[keysym - 0x13bc];
    else if (keysym > 0x14a0 && keysym < 0x1500)
	return keysym_to_unicode_14a1_14ff[keysym - 0x14a1];
    else if (keysym > 0x15cf && keysym < 0x15f7)
	return keysym_to_unicode_15d0_15f6[keysym - 0x15d0];
    else if (keysym > 0x169f && keysym < 0x16f7)
	return keysym_to_unicode_16a0_16f6[keysym - 0x16a0];
    else if (keysym > 0x1e9e && keysym < 0x1f00)
	return keysym_to_unicode_1e9f_1eff[keysym - 0x1e9f];
    else if (keysym > 0x209f && keysym < 0x20ad)
	return keysym_to_unicode_20a0_20ac[keysym - 0x20a0];
    else 
	return 0;
}

/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 */
