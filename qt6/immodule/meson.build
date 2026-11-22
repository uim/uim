qt6_immodule_hdrs = [
  '../../qt5/immodule/quimplatforminputcontext.h',
  '../../qt4/immodule/candidatewindowproxy.h',
  '../../qt4/immodule/caretstateindicator.h',
  '../../qt4/immodule/plugin.h',
  '../../qt4/immodule/qhelpermanager.h',
  '../../qt4/immodule/qtextutil.h',
  '../../qt4/immodule/quiminfomanager.h',
  ]
qt6_immodule_srcs = [
  '../../qt5/immodule/quimplatforminputcontext.cpp',
  '../../qt4/immodule/candidatewindowproxy.cpp',
  '../../qt4/immodule/caretstateindicator.cpp',
  '../../qt4/immodule/plugin.cpp',
  '../../qt4/immodule/qhelpermanager.cpp',
  '../../qt4/immodule/qtextutil.cpp',
  '../../qt4/immodule/quiminfomanager.cpp',
  '../../qt4/candwin/util.h',
  '../../qt4/candwin/util.cpp',
  ]

if x11.found()
  qt6_immodule_srcs += [
    '../../qt4/immodule/quiminputcontext_compose.h',
    '../../qt4/immodule/quiminputcontext_compose.cpp',
    ]
endif
qt6_immodule_mocs = qt6.compile_moc(
  headers: qt6_immodule_hdrs,
  include_directories: uim_qt6_inc,
  dependencies: qt6_dep)

qt6_immodule = shared_module('uimplatforminputcontextplugin',
  qt6_immodule_hdrs,
  qt6_immodule_srcs,
  qt6_immodule_mocs,
  dependencies: [qt6_dep, x11],
  include_directories: [
    uim_qt6_inc,
    include_directories('../../qt4/candwin'),
    include_directories('../../qt4/immodule'),
    include_directories('../../qt5/immodule'),
    ],
  install: true,
  install_dir: qt6_plugins_dir / 'platforminputcontexts',
  link_with: [
    uim_qt6_libs,
    libuim_counted_init
    ],
  override_options: qt6_opts,
  )

qt6_test_load = executable('test-load',
  'test-load.cpp',
  dependencies: qt6_dep,
  override_options: qt6_opts,
  )

test('qt6-immodule-load',
  qt6_test_load,
  args: [qt6_immodule],
  env: uim_build_tree_env + {
    'LIBUIM_VANILLA': '2',
    'QT_QPA_PLATFORM': 'offscreen',
    'QT_DEBUG_PLUGINS': '1',
    },
  suite: 'qt6')
