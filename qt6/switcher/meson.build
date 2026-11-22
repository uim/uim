qt6_switcher_hdrs = [ '../../qt4/switcher/qt4.h' ]
qt6_switcher_srcs = [ '../../qt4/switcher/qt4.cpp' ]
qt6_switcher_mocs = qt6.compile_moc(
  headers: qt6_switcher_hdrs,
  include_directories: uim_qt6_inc,
  dependencies: qt6_dep,
  )

executable('uim-im-switcher-qt6',
  qt6_switcher_hdrs,
  qt6_switcher_srcs,
  qt6_switcher_mocs,
  dependencies: qt6_dep,
  include_directories: uim_qt6_inc,
  install: true,
  link_with: [
    uim_qt6_libs,
    libuim_custom,
    ],
  override_options: qt6_opts,
  )
