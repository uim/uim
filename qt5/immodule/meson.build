qt5_immodule_hdrs = [
  '../../qt5/immodule/quimplatforminputcontext.h',
  '../../qt4/immodule/candidatewindowproxy.h',
  '../../qt4/immodule/caretstateindicator.h',
  '../../qt4/immodule/plugin.h',
  '../../qt4/immodule/qhelpermanager.h',
  '../../qt4/immodule/qtextutil.h',
  '../../qt4/immodule/quiminfomanager.h',
  '../../qt4/candwin/util.h',
  ]
qt5_immodule_srcs = [
  '../../qt5/immodule/quimplatforminputcontext.cpp',
  '../../qt4/immodule/candidatewindowproxy.cpp',
  '../../qt4/immodule/caretstateindicator.cpp',
  '../../qt4/immodule/plugin.cpp',
  '../../qt4/immodule/qhelpermanager.cpp',
  '../../qt4/immodule/qtextutil.cpp',
  '../../qt4/immodule/quiminfomanager.cpp',
  '../../qt4/candwin/util.cpp',
  ]

if x11.found()
  qt5_immodule_srcs += [
    '../../qt4/immodule/quiminputcontext_compose.h',
    '../../qt4/immodule/quiminputcontext_compose.cpp',
    ]
endif
qt5_immodule_mocs = qt5.compile_moc(
  headers: qt5_immodule_hdrs,
  include_directories: uim_qt5_inc,
  dependencies: qt5_dep)

qt5_immodule = shared_module('uimplatforminputcontextplugin',
  qt5_immodule_hdrs,
  qt5_immodule_srcs,
  qt5_immodule_mocs,
  dependencies: [qt5_dep, x11],
  include_directories: [
    uim_qt5_inc,
    include_directories('../../qt4/candwin'),
    include_directories('../../qt4/immodule'),
    include_directories('../../qt5/immodule'),
    ],
  install: true,
  install_dir: qt5_plugins_dir / 'platforminputcontexts',
  link_with: [
    uim_qt5_libs,
    libuim_counted_init
    ],
  )

qt5_test_load = executable('test-load',
  '../../qt6/immodule/test-load.cpp',
  dependencies: qt5_dep,
  )

test('qt5-immodule-load',
  qt5_test_load,
  args: [qt5_immodule],
  env: uim_build_tree_env + {
    'LIBUIM_VANILLA': '2',
    'QT_QPA_PLATFORM': 'offscreen',
    'QT_DEBUG_PLUGINS': '1',
    },
  suite: 'qt5')
