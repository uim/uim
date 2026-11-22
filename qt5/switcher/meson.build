qt5_switcher_hdrs = [ '../../qt4/switcher/qt4.h' ]
qt5_switcher_srcs = [ '../../qt4/switcher/qt4.cpp' ]
qt5_switcher_mocs = qt5.compile_moc(
  headers: qt5_switcher_hdrs,
  include_directories: uim_qt5_inc,
  dependencies: qt5_dep,
  )

executable('uim-im-switcher-qt5',
  qt5_switcher_hdrs,
  qt5_switcher_srcs,
  qt5_switcher_mocs,
  dependencies: qt5_dep,
  include_directories: uim_qt5_inc,
  install: true,
  link_with: [
    uim_qt5_libs,
    libuim_custom,
    ],
  )
    
