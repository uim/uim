# Client code for zwp_input_method_v1 generated from wayland-protocols.
wayland_input_method_xml = (
  wayland_protocols.get_variable(pkgconfig: 'pkgdatadir') /
  'unstable' / 'input-method' / 'input-method-unstable-v1.xml')
wayland_scanner_program = find_program(
  wayland_scanner.get_variable(pkgconfig: 'wayland_scanner'),
  native: true)
wayland_protocol_sources = []
foreach definition : [
  ['client-header', 'input-method-unstable-v1-client-protocol.h'],
  ['private-code', 'input-method-unstable-v1-protocol.c'],
]
  wayland_protocol_sources += [
    custom_target(definition[1],
                  input: wayland_input_method_xml,
                  output: definition[1],
                  command: [wayland_scanner_program,
                            definition[0],
                            '@INPUT@',
                            '@OUTPUT@']),
  ]
endforeach

# The sources include config.h only when HAVE_CONFIG_H is defined as
# Autotools does.
uim_wayland = executable('uim-wayland',
                         'candwin.c',
                         'helper.c',
                         'input-method-v1.c',
                         'key.c',
                         'text.c',
                         'uim-wayland.c',
                         wayland_protocol_sources,
                         c_args: ['-DHAVE_CONFIG_H'],
                         dependencies: wayland_dependencies,
                         include_directories: uim_include_directories,
                         install: true,
                         link_with: [libuim_scm, libuim])

# KWin lists input methods from desktop files that have
# X-KDE-Wayland-VirtualKeyboard=true.
configure_file(input: 'uim-wayland.desktop.in',
               output: 'uim-wayland.desktop',
               configuration: {
                 'bindir': prefix / get_option('bindir'),
                 'uim_pixmapsdir': prefix / uim_pixmapsdir,
               },
               install_dir: datadir / 'applications')

# A compositor that drives uim-wayland over a socketpair. It needs no
# display and no compositor, so it can run unattended. It reads the
# wire format out of the same protocol descriptions wayland-scanner
# uses, so it needs no library of its own.
if get_option('tests').allowed()
  # Ruby and wayland.xml are required only when both Wayland support and
  # the tests are enabled explicitly.
  wayland_test_required = (get_option('wayland').enabled() and
                           get_option('tests').enabled())
  ruby = find_program('ruby', required: wayland_test_required)
  wayland_core_xml = (wayland_scanner.get_variable(pkgconfig: 'pkgdatadir') /
                      'wayland.xml')
  have_wayland_core_xml = fs.is_file(wayland_core_xml)
  if wayland_test_required and not have_wayland_core_xml
    error('wayland.xml is required to test uim-wayland')
  endif
  if ruby.found() and have_wayland_core_xml
    test('uim-wayland',
         ruby,
         args: [files('test' / 'run.rb')],
         depends: uim_wayland,
         env: {
           'INPUT_METHOD_XML': wayland_input_method_xml,
           'LIBUIM_PLUGIN_LIB_DIR': meson.project_build_root() / 'uim',
           'LIBUIM_SCM_FILES': (meson.project_build_root() / 'scm' + ':' +
                                meson.project_source_root() / 'scm'),
           'LIBUIM_SYSTEM_SCM_FILES': (meson.project_source_root() /
                                       'subprojects' / 'sigscheme' / 'lib'),
           'UIM_WAYLAND': uim_wayland.full_path(),
           'WAYLAND_XML': wayland_core_xml,
         },
         suite: 'wayland',
         # It runs uim-wayland for each test case and takes about two
         # minutes.
         timeout: 300)
  endif
endif
