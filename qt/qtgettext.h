#ifndef _QT_GETTEXT_H_
#define _QT_GETTEXT_H_

#include "uim/config.h"

/* NLS can be disabled through the configure --disable-nls option.  */
#if ENABLE_NLS

#include "uim/gettext.h"
/* undef original _(String) macro for handling encoding */
#ifdef _
#undef _
#endif
#define _(String) QString::fromUtf8( dgettext(GETTEXT_PACKAGE,String) )
/* undef original N_(String) macro for handling encoding */
#ifdef N_
#undef N_
#endif
#ifdef gettext_noop
#define N_(String) gettext_noop(String)
#else
#define N_(String) (String)
#endif /* gettext_noop */

#endif /* ENABLE_NLS */

#endif /* Not def: _QT_GETTEXT_H_ */
