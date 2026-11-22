qt6_toolbar_hdrs = [
  '../../qt4/toolbar/common-quimhelpertoolbar.h',
  '../../qt4/toolbar/common-uimstateindicator.h',
  '../../qt4/toolbar/standalone-qt4.h',
  ]
qt6_toolbar_srcs = [
  '../../qt4/toolbar/common-quimhelpertoolbar.cpp',
  '../../qt4/toolbar/common-uimstateindicator.cpp',
  '../../qt4/toolbar/standalone-qt4.cpp',
  ]
qt6_toolbar_mocs = qt6.compile_moc(
  headers: qt6_toolbar_hdrs,
  include_directories: uim_qt6_inc,
  dependencies: qt6_dep)

executable('uim-toolbar-qt6',
  qt6_toolbar_hdrs,
  qt6_toolbar_srcs,
  qt6_toolbar_mocs,
  cpp_args: [
    f'-DKDE4_ICONDIR="@qt6_icons_dir@"',
  ],
  dependencies: qt6_dep,
  include_directories: uim_qt6_inc,
  install: true,
  install_dir: libexecdir,
  link_with: uim_qt6_libs,
  override_options: qt6_opts,
  )
