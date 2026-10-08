# libtool's -version-info CURRENT:REVISION:AGE is shared with
# Makefile.am via libtool-version.mk.
library_versions = {}
foreach line : fs.read('libtool-version.mk').split('\n')
  if line.contains('_version =')
    name_and_version = line.split('=')
    components = name_and_version[1].strip().split(':')
    current = components[0].to_int()
    revision = components[1].to_int()
    age = components[2].to_int()
    library_versions += {
      name_and_version[0].strip(): {
        'soversion': current - age,
        'version': '@0@.@1@.@2@'.format(current - age, age, revision),
      },
    }
  endif
endforeach

configure_file(output: 'config-meson.h',
               configuration: config_data)
configure_file(input: 'config.h.meson',
               output: 'config.h',
               copy: true)
version_h = configure_file(input: 'version.h.in',
                           output: 'version.h',
                           configuration: {
                             'UIM_VERSION_MAJOR': version_components[0],
                             'UIM_VERSION_MINOR': version_components[1],
                             'UIM_VERSION_PATCHLEVEL': version_components[2],
                           })

# The build directory, where config.h is, comes before the SigScheme
# directories from the dependency: SigScheme sources #include
# <config.h> for this one when they are compiled into libuim-scm.
uim_include_directories = [
  include_directories('.'),
  replace_include_directories,
  include_directories('..'),
]
uim_c_args = [
  '-DSCMLIBDIR="@0@"'.format(prefix / pkgdatadir / 'lib'),
]
uim_defs = [
  '-DSCM_FILES="@0@"'.format(prefix / pkgdatadir),
]

libreplace = static_library('replace',
                            replace_sources,
                            include_directories: uim_include_directories,
                            pic: true)

libuim_scm = library('uim-scm',
                     'uim-scm-sigscheme.c',
                     c_args: uim_c_args,
                     dependencies: sigscheme_combined,
                     include_directories: uim_include_directories,
                     install: true,
                     soversion: library_versions['libuim_scm_version']['soversion'],
                     version: library_versions['libuim_scm_version']['version'])

libuim_sources = files(
  'dynlib.c',
  'iconv.c',
  'intl.c',
  'rk.c',
  'uim-error.c',
  'uim-func.c',
  'uim-helper-client.c',
  'uim-helper.c',
  'uim-ipc.c',
  'uim-key.c',
  'uim-notify.c',
  'uim-posix.c',
  'uim-util.c',
  'uim.c',
)
libuim = library('uim',
                 libuim_sources,
                 c_args: (uim_c_args +
                          uim_defs +
                          [
                            '-DPKGLIBDIR="@0@"'.format(prefix / pkglibdir),
                            '-DPKGDATADIR="@0@"'.format(prefix / pkgdatadir),
                          ]),
                 dependencies: [dl, socket, iconv, intl],
                 include_directories: uim_include_directories,
                 install: true,
                 link_with: [libuim_scm, libreplace],
                 soversion: library_versions['libuim_version']['soversion'],
                 version: library_versions['libuim_version']['version'])

libuim_custom = library('uim-custom',
                        'uim-custom.c',
                        c_args: uim_c_args + uim_defs,
                        include_directories: uim_include_directories,
                        install: true,
                        link_with: [libuim_scm, libuim],
                        soversion: library_versions['libuim_custom_version']['soversion'],
                        version: library_versions['libuim_custom_version']['version'])

libuim_bsdlook = static_library('uim-bsdlook',
                                'bsdlook.c',
                                include_directories: uim_include_directories,
                                pic: true)

optional_plugins = {}
# [name, sources, extra link_with, extra dependencies]
plugin_definitions = [
  ['custom-enabler', ['uim-custom-enabler.c'], [libuim_custom], []],
  ['fileio', ['fileio.c'], [], []],
  ['lolevel', ['lolevel.c'], [], []],
  ['look', ['look.c'], [libuim_bsdlook], []],
  ['process', ['process.c'], [], []],
  ['socket', ['socket.c'], [], []],
  ['tutcode', ['tutcode.c'], [], []],
]
if get_option('skk')
  plugin_definitions += [
    ['skk', ['skk.c'], [libuim_bsdlook], net_libraries],
  ]
endif
# [plugin, option, dependency, version]
foreach definition : [
  ['editline', 'libedit', 'libedit', []],
  ['expat', 'expat', 'expat', []],
  ['ffi', 'ffi', 'libffi', ['>=3.0.0']],
  ['openssl', 'openssl', 'openssl', ['>=1.1.0']],
  ['sqlite3', 'sqlite3', 'sqlite3', ['>=3.0.0']],
]
  plugin_dependency = dependency(definition[2],
                                 required: get_option(definition[1]),
                                 version: definition[3])
  if plugin_dependency.found()
    plugin_definitions += [
      [definition[0], [definition[0] + '.c'], [], [plugin_dependency]],
    ]
  endif
  optional_plugins += {definition[0]: plugin_dependency.found()}
endforeach
plugins = []
foreach definition : plugin_definitions
  plugins += [
    shared_module('uim-' + definition[0],
                  definition[1],
                  c_args: uim_c_args,
                  dependencies: definition[3],
                  include_directories: uim_include_directories,
                  install: true,
                  install_dir: uimplugindir,
                  link_with: [libuim_scm, libuim] + definition[2]),
  ]
endforeach

# [name, extra c_args, link_with]
program_definitions = [
  ['uim-custom-cli', [], [libuim_custom, libuim]],
  ['uim-help', [], [libuim_scm, libuim]],
  ['uim-im-switcher-cli', [], [libuim]],
  ['uim-module-manager',
   ['-DUIM_DATADIR="@0@"'.format(prefix / pkgdatadir)],
   [libuim_scm, libuim]],
  ['uim-sh', [], [libuim_scm, libuim]],
]
programs = {}
foreach definition : program_definitions
  programs += {
    definition[0]: executable(definition[0],
                              definition[0] + '.c',
                              c_args: uim_c_args + uim_defs + definition[1],
                              include_directories: uim_include_directories,
                              install: true,
                              link_with: definition[2]),
  }
endforeach

executable('uim-helper-server',
           'uim-helper.c',
           'uim-helper-server.c',
           'uim-error.c',
           # uim-notify workaround for uim-error.c combined with
           # non-libuim program
           c_args: uim_c_args + uim_defs + ['-DUIM_NON_LIBUIM_PROG'],
           include_directories: uim_include_directories,
           install: true,
           install_dir: libexecdir,
           link_with: libreplace)

install_headers(
  'dynlib.h',
  'uim-custom.h',
  'uim-helper.h',
  'uim-im-switcher.h',
  'uim-notify.h',
  'uim-posix.h',
  'uim-scm-abbrev.h',
  'uim-scm.h',
  'uim-util.h',
  'uim.h',
  version_h,
  subdir: meson.project_name())
