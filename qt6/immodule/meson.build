qt6_immodule_source_dir = '..' / '..' / 'qt4' / 'immodule'
qt6_platform_input_context_dir = '..' / '..' / 'qt5' / 'immodule'
# The headers that have Q_OBJECT. plugin.h has the metadata that makes
# Qt accept this as a plugin.
qt6_immodule_moc = qt6_module.compile_moc(
  headers: [
    qt6_immodule_source_dir / 'candidatewindowproxy.h',
    qt6_immodule_source_dir / 'caretstateindicator.h',
    qt6_immodule_source_dir / 'plugin.h',
    qt6_immodule_source_dir / 'qhelpermanager.h',
    qt6_immodule_source_dir / 'qtextutil.h',
    qt6_platform_input_context_dir / 'quimplatforminputcontext.h',
  ],
  dependencies: qt6,
  include_directories: qt6_include_directories)
qt6_immodule = shared_module(
  'uimplatforminputcontextplugin',
  qt6_immodule_source_dir / 'candidatewindowproxy.cpp',
  qt6_immodule_source_dir / 'caretstateindicator.cpp',
  qt6_immodule_source_dir / 'plugin.cpp',
  qt6_immodule_source_dir / 'qhelpermanager.cpp',
  qt6_immodule_source_dir / 'qtextutil.cpp',
  qt6_immodule_source_dir / 'quiminfomanager.cpp',
  qt6_immodule_source_dir / 'quiminputcontext_compose.cpp',
  '..' / '..' / 'qt4' / 'candwin' / 'util.cpp',
  qt6_platform_input_context_dir / 'quimplatforminputcontext.cpp',
  qt6_immodule_moc,
  dependencies: [qt6, x11, intl],
  include_directories: qt6_include_directories,
  install: true,
  install_dir: qt6_plugins_dir / 'platforminputcontexts',
  link_with: [qt6_link_with, libuim_counted_init],
  override_options: qt6_override_options)

# Qt ignores a plugin that it can't load without any error, so load it
# as Qt does.
if get_option('tests').allowed()
  test('qt6-immodule-load',
       executable('test-load',
                  'test-load.cpp',
                  dependencies: qt6,
                  override_options: qt6_override_options),
       args: [qt6_immodule],
       env: uim_build_tree_env + {
         # ~/.uim and the user's customizations stay out of the test.
         'LIBUIM_VANILLA': '2',
         # No display is needed.
         'QT_QPA_PLATFORM': 'offscreen',
       },
       suite: 'qt6')
endif
