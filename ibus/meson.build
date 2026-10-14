# The sources include config.h only when HAVE_CONFIG_H is defined as
# Autotools does.
ibus_engine_uim = executable('ibus-engine-uim',
                             'helper.c',
                             'ibus-engine-uim.c',
                             'property.c',
                             'text.c',
                             c_args: ['-DHAVE_CONFIG_H'],
                             dependencies: ibus,
                             include_directories: uim_include_directories,
                             install: true,
                             install_dir: libexecdir,
                             link_with: [libuim_scm, libuim])

# ibus-daemon lists the engines described in this directory.
configure_file(input: 'uim.xml.in',
               output: 'uim.xml',
               configuration: {
                 'libexecdir': prefix / libexecdir,
                 'PACKAGE_VERSION': meson.project_version(),
                 'uim_pixmapsdir': prefix / uim_pixmapsdir,
               },
               install_dir: datadir / 'ibus' / 'component')

# An ibus-daemon of the test's own, driven through libibus as GNOME
# Shell drives it. It needs no display and no desktop, but
# Ruby/GObjectIntrospection and the IBus typelib.
if get_option('tests').allowed()
  # The programs are required only when both IBus support and the tests
  # are enabled explicitly.
  ibus_test_required = (get_option('ibus').enabled() and
                        get_option('tests').enabled())
  ruby = find_program('ruby', required: ibus_test_required)
  ibus_daemon = find_program('ibus-daemon',
                             dirs: ['/usr/libexec', '/usr/lib/ibus'],
                             required: ibus_test_required)
  dbus_run_session = find_program('dbus-run-session',
                                  required: ibus_test_required)
  if ruby.found() and ibus_daemon.found() and dbus_run_session.found()
    test('ibus-engine-uim',
         dbus_run_session,
         args: ['--', ruby, files('test' / 'run.rb')],
         depends: ibus_engine_uim,
         env: {
           'IBUS_DAEMON': ibus_daemon.full_path(),
           'IBUS_ENGINE_UIM': ibus_engine_uim.full_path(),
           'LIBUIM_PLUGIN_LIB_DIR': meson.project_build_root() / 'uim',
           'LIBUIM_SCM_FILES': (meson.project_build_root() / 'scm' + ':' +
                                meson.project_source_root() / 'scm'),
           'LIBUIM_SYSTEM_SCM_FILES': (meson.project_source_root() /
                                       'subprojects' / 'sigscheme' / 'lib'),
         },
         suite: 'ibus')
  endif
endif
