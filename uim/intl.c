#include <stdlib.h>
#include <string.h>
#include "config.h"
#ifdef ENABLE_NLS
#include <locale.h>
#endif

#include "gettext.h"
#include "siod.h"

#include "intl.h"

static LISP
intl_gettext_package()
{
  return strcons(-1, GETTEXT_PACKAGE);
}


static LISP
intl_textdomain(LISP domainname)
{
  const char *new_domain;

  if NULLP(domainname) {
    new_domain = textdomain(NULL);
  } else {
    new_domain = textdomain(get_c_string(domainname));
  }

  return strcons(-1, new_domain);
}

static LISP
intl_bindtextdomain(LISP domainname, LISP dirname)
{
  const char *domain, *new_dir;

  domain = get_c_string(domainname);

  if NULLP(dirname) {
    new_dir = bindtextdomain(domain, NULL);
  } else {
    new_dir = bindtextdomain(domain, get_c_string(dirname));
  }

  return strcons(-1, new_dir);
}

static LISP
intl_bind_textdomain_codeset(LISP domainname, LISP codeset)
{
  return strcons(-1, bind_textdomain_codeset(get_c_string(domainname),
                                             get_c_string(codeset)));
}

static LISP
intl_gettext(LISP msgid)
{
  return strcons(-1, gettext(get_c_string(msgid)));
}

static LISP
intl_dgettext(LISP domainname, LISP msgid)
{
  return strcons(-1, dgettext(get_c_string(domainname),
                              get_c_string(msgid)));
}

static LISP
intl_dcgettext(LISP domainname, LISP msgid, LISP category)
{
  return strcons(-1, dcgettext(get_c_string(domainname),
                               get_c_string(msgid),
                               get_c_int(category)));
}

static LISP
intl_ngettext(LISP msgid1, LISP msgid2, LISP n)
{
  return strcons(-1, ngettext(get_c_string(msgid1),
                              get_c_string(msgid2),
                              get_c_int(n)));
}

static LISP
intl_dngettext(LISP domainname, LISP msgid1, LISP msgid2, LISP n)
{
  return strcons(-1, dngettext(get_c_string(domainname),
                               get_c_string(msgid1),
                               get_c_string(msgid2),
                               get_c_int(n)));
}

static LISP
intl_dcngettext(LISP domainname, LISP msgid1, LISP msgid2, LISP n, LISP category)
{
  return strcons(-1, dcngettext(get_c_string(domainname),
                                get_c_string(msgid1),
                                get_c_string(msgid2),
                                get_c_int(n),
                                get_c_int(category)));
}

static void
intl_init_locale(void)
{
#ifdef ENABLE_NLS
  const char *current_locale;

  /* Perform setlocale() only if it maybe did not performed before. This  */
  current_locale = setlocale(LC_MESSAGES, NULL);
  if (!strcmp(current_locale, "C")) {
    setlocale(LC_ALL, "");
  }

  bindtextdomain(GETTEXT_PACKAGE, LOCALEDIR);
  /* bind_textdomain_codeset() should not be performed here. See
     UIM_EVAL_STRING()
  */

#if 0
  /* textdomain() must not be performed by a library because it masks
     application's one. Use dgettext() always.
  */
  textdomain(GETTEXT_PACKAGE);
#endif
#endif
}

void
init_intl(void)
{
  intl_init_locale();

  init_subr_0("gettext-package", intl_gettext_package);
  init_subr_1("textdomain", intl_textdomain);
  init_subr_2("bindtextdomain", intl_bindtextdomain);
  init_subr_2("bind-textdomain-codeset", intl_bind_textdomain_codeset);
  init_subr_1("gettext", intl_gettext);
  init_subr_2("dgettext", intl_dgettext);
  init_subr_3("dcgettext", intl_dcgettext);
  init_subr_3("ngettext", intl_ngettext);
  init_subr_4("dngettext", intl_dngettext);
  init_subr_5("dcngettext", intl_dcngettext);

#ifdef ENABLE_NLS
  siod_c_provide("nls");
#endif
}
