qt5_pref_hdrs = [
  '../../qt4/pref/customwidgets.h',
  '../../qt4/pref/qt4.h',
  ]
qt5_pref_srcs = [
  '../../qt4/pref/customwidgets.cpp',
  '../../qt4/pref/qt4.cpp',
  '../../qt4/pref/keyeditformbase.h',
  '../../qt4/pref/keyeditformbase.cpp',
  '../../qt4/pref/olisteditformbase.h',
  '../../qt4/pref/olisteditformbase.cpp',
  ]
qt5_pref_mocs = qt5.compile_moc(
  headers: qt5_pref_hdrs,
  include_directories: uim_qt5_inc,
  dependencies: qt5_dep)

executable('uim-pref-qt5',
  qt5_pref_hdrs,
  qt5_pref_srcs,
  qt5_pref_mocs,
  dependencies: qt5_dep,
  include_directories: uim_qt5_inc,
  install: true,
  link_with: [
    uim_qt5_libs,
    libuim_custom,
    libuim_counted_init,
    ],
  )
