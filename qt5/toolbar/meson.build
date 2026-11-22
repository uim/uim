qt5_toolbar_hdrs = [
  '../../qt4/toolbar/common-quimhelpertoolbar.h',
  '../../qt4/toolbar/common-uimstateindicator.h',
  '../../qt4/toolbar/standalone-qt4.h',
  ]
qt5_toolbar_srcs = [
  '../../qt4/toolbar/common-quimhelpertoolbar.cpp',
  '../../qt4/toolbar/common-uimstateindicator.cpp',
  '../../qt4/toolbar/standalone-qt4.cpp',
  ]
qt5_toolbar_mocs = qt5.compile_moc(
  headers: qt5_toolbar_hdrs,
  include_directories: uim_qt5_inc,
  dependencies: qt5_dep)

executable('uim-toolbar-qt5',
  qt5_toolbar_hdrs,
  qt5_toolbar_srcs,
  qt5_toolbar_mocs,
  cpp_args: [
    f'-DKDE4_ICONDIR="@qt5_icons_dir@"',
  ],
  dependencies: qt5_dep,
  include_directories: uim_qt5_inc,
  install: true,
  install_dir: libexecdir,
  link_with: uim_qt5_libs,
  )
