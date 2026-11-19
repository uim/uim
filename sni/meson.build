uim_sni = executable('uim-sni',
                     'uim-sni.c',
                     dependencies: [gio],
                     include_directories: uim_include_directories,
                     install: true,
                     link_with: [libuim_scm, libuim])

# The tests run their own D-Bus session bus and stand in for a panel
# such as Waybar and for uim-helper-server.
if get_option('tests').allowed()
  # Ruby, ruby-dbus and dbus-daemon are required only when both SNI
  # support and the tests are enabled explicitly.
  sni_test_required = (get_option('sni').enabled() and
                       get_option('tests').enabled())
  ruby = find_program('ruby', required: sni_test_required)
  dbus_daemon = find_program('dbus-daemon', required: sni_test_required)
  have_ruby_dbus = (ruby.found() and
                    run_command(ruby, '-e', 'require "dbus"',
                                check: false).returncode() == 0)
  if sni_test_required and not have_ruby_dbus
    error('ruby-dbus is required to test uim-sni')
  endif
  if have_ruby_dbus and dbus_daemon.found()
    test('uim-sni',
         ruby,
         args: [files('test' / 'run.rb')],
         depends: uim_sni,
         env: uim_build_tree_env + {
           'DBUS_DAEMON': dbus_daemon.full_path(),
           # ~/.uim and the user's customizations stay out of the test.
           'LIBUIM_VANILLA': '2',
           'UIM_SNI': uim_sni.full_path(),
           # The icons aren't installed yet.
           'UIM_SNI_PIXMAPSDIR': meson.project_source_root() / 'pixmaps',
         },
         suite: 'sni')
  endif
endif
