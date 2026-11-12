# Client code generated from the protocol descriptions.
wayland_protocols_dir = wayland_protocols.get_variable(pkgconfig: 'pkgdatadir')
wayland_input_method_v1_xml = (wayland_protocols_dir / 'unstable' /
                               'input-method' /
                               'input-method-unstable-v1.xml')
# wayland-protocols doesn't have these.
wayland_input_method_v2_xml = (meson.current_source_dir() / 'protocol' /
                               'input-method-unstable-v2.xml')
wayland_virtual_keyboard_xml = (meson.current_source_dir() / 'protocol' /
                                'virtual-keyboard-unstable-v1.xml')
wayland_scanner_program = find_program(
  wayland_scanner.get_variable(pkgconfig: 'wayland_scanner'),
  native: true)
wayland_protocols_to_generate = [
  [wayland_input_method_v1_xml, 'input-method-unstable-v1'],
  [wayland_input_method_v2_xml, 'input-method-unstable-v2'],
  [wayland_virtual_keyboard_xml, 'virtual-keyboard-unstable-v1'],
]
if have_wayland_fractional_scale
  wayland_fractional_scale_xml = (wayland_protocols_dir / 'staging' /
                                  'fractional-scale' /
                                  'fractional-scale-v1.xml')
  wayland_viewporter_xml = (wayland_protocols_dir / 'stable' /
                            'viewporter' / 'viewporter.xml')
  wayland_protocols_to_generate += [
    [wayland_fractional_scale_xml, 'fractional-scale-v1'],
    [wayland_viewporter_xml, 'viewporter'],
  ]
else
  wayland_fractional_scale_xml = ''
  wayland_viewporter_xml = ''
endif
if have_wayland_cursor_shape
  wayland_cursor_shape_xml = (wayland_protocols_dir / 'staging' /
                              'cursor-shape' / 'cursor-shape-v1.xml')
  # cursor-shape-v1 refers to zwp_tablet_tool_v2.
  wayland_tablet_xml = (wayland_protocols_dir / 'unstable' / 'tablet' /
                        'tablet-unstable-v2.xml')
  wayland_protocols_to_generate += [
    [wayland_cursor_shape_xml, 'cursor-shape-v1'],
    [wayland_tablet_xml, 'tablet-unstable-v2'],
  ]
else
  wayland_cursor_shape_xml = ''
endif
wayland_protocol_sources = []
foreach protocol : wayland_protocols_to_generate
  foreach definition : [
    ['client-header', protocol[1] + '-client-protocol.h'],
    ['private-code', protocol[1] + '-protocol.c'],
  ]
    wayland_protocol_sources += [
      custom_target(definition[1],
                    input: protocol[0],
                    output: definition[1],
                    command: [wayland_scanner_program,
                              definition[0],
                              '@INPUT@',
                              '@OUTPUT@']),
    ]
  endforeach
endforeach

# The sources include config.h only when HAVE_CONFIG_H is defined as
# Autotools does.
uim_wayland = executable('uim-wayland',
                         'candwin.c',
                         'helper.c',
                         'input-method-v1.c',
                         'input-method-v2.c',
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
         env: uim_build_tree_env + {
           'INPUT_METHOD_V1_XML': wayland_input_method_v1_xml,
           'INPUT_METHOD_V2_XML': wayland_input_method_v2_xml,
           'UIM_WAYLAND': uim_wayland.full_path(),
           'VIRTUAL_KEYBOARD_XML': wayland_virtual_keyboard_xml,
           # Empty when uim-wayland is built without them.
           'FRACTIONAL_SCALE_XML': wayland_fractional_scale_xml,
           'VIEWPORTER_XML': wayland_viewporter_xml,
           'CURSOR_SHAPE_XML': wayland_cursor_shape_xml,
           'WAYLAND_XML': wayland_core_xml,
         },
         suite: 'wayland',
         # It runs uim-wayland for each test case and takes about two
         # minutes.
         timeout: 300)
  endif
endif
