project('uim',
        'c',
        version: '2.0.0',
        license: 'BSD-3-Clause',
        # The bundled libgcroots is linked into libuim-scm as configure
        # does, so that uim needs no shared library of its own for it.
        default_options: ['libgcroots:default_library=static'],
        # The same as subprojects/sigscheme.
        meson_version: '>=0.63.0')

fs = import('fs')

cc = meson.get_compiler('c')

version_components = meson.project_version().split('.')
package_bugreport = 'uim-en@googlegroups.com'

prefix = get_option('prefix')
datadir = get_option('datadir')
includedir = get_option('includedir')
libdir = get_option('libdir')
libexecdir = get_option('libexecdir')
localedir = get_option('localedir')
pkgdatadir = datadir / meson.project_name()
pkglibdir = libdir / meson.project_name()
uimplugindir = pkglibdir / 'plugin'
uim_pixmapsdir = pkgdatadir / 'pixmaps'

# Workaround for NetBSD-i386-9.1 with gcc-7.5.0 and -O2.
# See also: https://github.com/uim/uim/issues/140#issuecomment-752380919
add_project_arguments(
  cc.get_supported_arguments('-fno-inline-small-functions'),
  language: 'c')


#
# Dependencies
#

dl = dependency('dl', required: false)
socket = cc.find_library('socket', required: false)
iconv = dependency('iconv')
intl = dependency('intl', required: false)
# For inet_aton(3) used by the SKK plugin.
net_libraries = []
if not cc.has_function('inet_aton')
  net_libraries += [cc.find_library('resolv', required: false)]
endif

# Anthy (EUC-JP) and Anthy (UTF-8). anthy.pc of 8622 or later, or
# Debian's that is older than 1000, can handle UTF-8.
anthy_utf8_capable_versions = ['>=8622', '<1000']
anthy = dependency('anthy', required: get_option('anthy'))
anthy_utf8_capable = false
if anthy.found()
  foreach version : anthy_utf8_capable_versions
    if anthy.version().version_compare(version)
      anthy_utf8_capable = true
    endif
  endforeach
endif
anthy_utf8 = dependency('', required: false)
if not get_option('anthy_utf8').disabled()
  anthy_utf8 = dependency('anthy-unicode', required: false)
  foreach version : anthy_utf8_capable_versions
    if not anthy_utf8.found()
      anthy_utf8 = dependency('anthy', required: false, version: version)
    endif
  endforeach
  if get_option('anthy_utf8').enabled() and not anthy_utf8.found()
    error('Anthy that can handle UTF-8 is needed for anthy_utf8')
  endif
  if anthy_utf8.found()
    anthy_utf8_capable = true
  endif
endif

curses = dependency('curses', required: get_option('fep'))

m17nlib = dependency('m17n-shell',
                     required: get_option('m17nlib'),
                     version: '>=1.3.1')

# SigScheme is compiled into libuim-scm as a single translation unit
# with uim-scm-sigscheme.c. See uim/uim-scm-sigscheme.c.
sigscheme_combined = dependency('sigscheme-combined',
                                fallback: 'sigscheme',
                                default_options: [
                                  'conf=uim',
                                  'libsscm=false',
                                  'master_pkg=uim',
                                  'shell=false',
                                ])


#
# config.h
#

config_data = configuration_data()
config_data.set_quoted('PACKAGE', meson.project_name())
config_data.set_quoted('PACKAGE_BUGREPORT', package_bugreport)
config_data.set_quoted('PACKAGE_NAME', meson.project_name())
config_data.set_quoted('PACKAGE_STRING',
                       '@0@ @1@'.format(meson.project_name(),
                                        meson.project_version()))
config_data.set_quoted('PACKAGE_TARNAME', meson.project_name())
config_data.set_quoted('PACKAGE_URL', '')
config_data.set_quoted('PACKAGE_VERSION', meson.project_version())
config_data.set_quoted('VERSION', meson.project_version())
config_data.set('_GNU_SOURCE', 1)
config_data.set('STDC_HEADERS', 1)

foreach header : [
  'alloca.h',
  'assert.h',
  'ctype.h',
  'curses.h',
  'dlfcn.h',
  'errno.h',
  'fcntl.h',
  'getopt.h',
  'inttypes.h',
  'libutil.h',
  'locale.h',
  'ncurses/term.h',
  'netdb.h',
  'poll.h',
  'pty.h',
  'pwd.h',
  'signal.h',
  'stdarg.h',
  'stdint.h',
  'stdio.h',
  'stdlib.h',
  'string.h',
  'strings.h',
  'stropts.h',
  'sys/inttypes.h',
  'sys/ioctl.h',
  'sys/param.h',
  'sys/poll.h',
  'sys/select.h',
  'sys/socket.h',
  'sys/stat.h',
  'sys/termios.h',
  'sys/time.h',
  'sys/types.h',
  'sys/un.h',
  'sysexits.h',
  'term.h',
  'termios.h',
  'unistd.h',
  'util.h',
  'utmp.h',
  'wchar.h',
]
  if cc.has_header(header)
    config_data.set('HAVE_' + header.underscorify().to_upper(), 1)
  endif
endforeach

foreach function : [
  'alloca',
  'asprintf',
  'cfmakeraw',
  'chmod',
  'daemon',
  'execv',
  'execvp',
  'fork',
  'freeaddrinfo',
  'getaddrinfo',
  'getnameinfo',
  'getpeereid',
  'getpid',
  'isascii',
  'issetugid',
  'mkdir',
  'mmap',
  'poll',
  'pselect',
  'setenv',
  'snprintf',
  'stat',
  'strdup',
  'strlcat',
  'strlcpy',
  'strsep',
  'strtoll',
  'strtonum',
  'unsetenv',
  'vasprintf',
  'vfork',
  'vsnprintf',
  'wcswidth',
]
  if cc.has_function(function)
    config_data.set('HAVE_' + function.to_upper(), 1)
  endif
endforeach
if cc.has_function('dlfunc', dependencies: dl)
  config_data.set('HAVE_DLFUNC', 1)
endif
if cc.has_function('realloc')
  config_data.set('HAVE_REALLOC', 1)
endif
# forkpty() is in libutil on some systems.
forkpty_dependencies = []
if cc.has_function('forkpty')
  config_data.set('HAVE_FORKPTY', 1)
else
  util = cc.find_library('util', required: false)
  if cc.has_function('forkpty', dependencies: util)
    config_data.set('HAVE_FORKPTY', 1)
    forkpty_dependencies += [util]
  endif
endif

sigsetjmp_code = '''
#include <setjmp.h>

int
main(void)
{
  sigjmp_buf env;
  while (!sigsetjmp(env, 1))
    siglongjmp(env, 1);
  return 0;
}
'''
if cc.links(sigsetjmp_code, name: 'sigsetjmp')
  config_data.set('HAVE_SIGSETJMP', 1)
endif

foreach type : [
  'intmax_t',
  'intptr_t',
  'uintmax_t',
  'uintptr_t',
]
  if cc.has_type(type, prefix: '#include <stdint.h>')
    config_data.set('HAVE_' + type.to_upper(), 1)
  endif
endforeach
foreach type : ['sig_atomic_t', 'sig_t']
  if cc.has_type(type, prefix: '#include <signal.h>')
    config_data.set('HAVE_' + type.to_upper(), 1)
  endif
endforeach
config_data.set('RETSIGTYPE', 'void')
if cc.has_type('long long int')
  config_data.set('HAVE_LONG_LONG_INT', 1)
endif
if cc.has_type('unsigned long long int')
  config_data.set('HAVE_UNSIGNED_LONG_LONG_INT', 1)
endif

socket_prefix = '''
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
'''
foreach type : [
  'struct addrinfo',
  'struct cmsgcred',
  'struct in6_addr',
  'struct sockaddr_in6',
  'struct sockaddr_storage',
  'struct sockcred',
]
  if cc.has_type(type, prefix: socket_prefix)
    config_data.set('HAVE_' + type.underscorify().to_upper(), 1)
  endif
endforeach

# IRIX has a const char return value for gai_strerror()
if cc.has_function('gai_strerror', prefix: socket_prefix)
  config_data.set('HAVE_GAI_STRERROR', 1)
  const_gai_strerror_code = socket_prefix + '''
const char *gai_strerror(int);

int
main(void)
{
  char *str;
  str = gai_strerror(0);
  return str != 0;
}
'''
  if cc.compiles(const_gai_strerror_code, name: 'const gai_strerror()')
    config_data.set('HAVE_CONST_GAI_STRERROR_PROTO', 1)
  endif
endif

if config_data.has('HAVE_SNPRINTF') and not meson.is_cross_build()
  broken_snprintf_code = '''
#include <stdio.h>

int
main(void)
{
  char b[5];
  snprintf(b, 5, "123456789");
  return b[4] != '\0';
}
'''
  if cc.run(broken_snprintf_code, name: 'snprintf terminates').returncode() != 0
    config_data.set('BROKEN_SNPRINTF', 1)
  endif
endif
snprintf_const_code = '''
#include <stdio.h>

int snprintf(char *a, size_t b, const char *c, ...) { return 0; }

int
main(void)
{
  snprintf(0, 0, 0);
  return 0;
}
'''
if cc.compiles(snprintf_const_code, name: 'snprintf() with const char *fmt')
  config_data.set('SNPRINTF_CONST', 'const')
else
  config_data.set('SNPRINTF_CONST', '/* not const */')
endif

langinfo_codeset_code = '''
#include <langinfo.h>

int
main(void)
{
  char *cs = nl_langinfo(CODESET);
  return !cs;
}
'''
if cc.links(langinfo_codeset_code, name: 'nl_langinfo(CODESET)')
  config_data.set('HAVE_LANGINFO_CODESET', 1)
endif

config_data.set('HAVE_ICONV', 1)
iconv_const_code = '''
#include <stdlib.h>
#include <iconv.h>

size_t iconv(iconv_t cd, char **inbuf, size_t *inbytesleft,
             char **outbuf, size_t *outbytesleft);

int
main(void)
{
  return 0;
}
'''
if cc.compiles(iconv_const_code, dependencies: iconv, name: 'non-const iconv()')
  config_data.set('ICONV_CONST', '')
else
  config_data.set('ICONV_CONST', 'const')
endif

config_data.set_quoted('GETTEXT_PACKAGE', meson.project_name())
if intl.found()
  config_data.set('ENABLE_NLS', 1)
  config_data.set('HAVE_GETTEXT', 1)
  config_data.set('HAVE_DCGETTEXT', 1)
endif

config_data.set_quoted('LIBDIR', prefix / libdir)
config_data.set_quoted('DATADIR', prefix / datadir)
config_data.set_quoted('UIM_LIBEXECDIR', prefix / libexecdir)
config_data.set_quoted('LOCALEDIR', prefix / localedir)
config_data.set_quoted('GNOMELOCALEDIR', prefix / localedir)
config_data.set_quoted('GNOME3LOCALEDIR', prefix / localedir)
config_data.set_quoted('UIM_PIXMAPSDIR', prefix / uim_pixmapsdir)

if anthy_utf8_capable
  config_data.set('LIBANTHY_UTF8_CAPABLE', 1)
endif

# longjmp-based error handlings
config_data.set('UIM_USE_ERROR_GUARD', 1)
if get_option('notify')
  config_data.set('UIM_USE_NOTIFY_PLUGINS', 1)
endif


subdir('replace')
subdir('uim')
subdir('scm')
if get_option('notify')
  subdir('notify')
endif
if get_option('tests').allowed()
  subdir('test2')
endif
subdir('pixmaps')
subdir('byeoru-data')
subdir('examples')
if curses.found()
  subdir('fep')
endif
if intl.found()
  subdir('po')
endif

pkgconfig = import('pkgconfig')
pkgconfig.generate(libraries: [libuim_scm, libuim],
                   name: meson.project_name(),
                   description: 'multilingual input method library',
                   subdirs: meson.project_name(),
                   variables: [
                     'uim_datadir=${prefix}' / pkgdatadir,
                     'uim_scmdir=${prefix}' / pkgdatadir,
                   ])

summary({
          'Canna': get_option('canna'),
          'FEP': curses.found(),
          'NLS': intl.found(),
          'Notify': get_option('notify'),
          'SKK': get_option('skk'),
        },
        section: 'Features')
summary(optional_plugins, section: 'Plugins')
if get_option('notify')
  summary(optional_notify_plugins, section: 'Notify plugins')
endif
