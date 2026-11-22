qt5_candwin_hdrs = [
  '../../qt4/candwin/abstractcandidatewindow.h',
  '../../qt4/candwin/candidatetablewindow.h',
  '../../qt4/candwin/candidatewindow.h',
  '../../qt4/candwin/subwindow.h',
  '../../qt4/candwin/ximcandidatewindow.h',
  ]
qt5_candwin_srcs = [
  '../../qt4/candwin/abstractcandidatewindow.cpp',
  '../../qt4/candwin/candidatetablewindow.cpp',
  '../../qt4/candwin/candidatewindow.cpp',
  '../../qt4/candwin/qt4.cpp',
  '../../qt4/candwin/subwindow.cpp',
  '../../qt4/candwin/util.h',
  '../../qt4/candwin/util.cpp',
  '../../qt4/candwin/ximcandidatewindow.cpp',
  ]
qt5_candwin_mocs = qt5.compile_moc(
  headers: qt5_candwin_hdrs,
  include_directories: uim_qt5_inc,
  dependencies: qt5_dep)

executable('uim-candwin-qt5',
  qt5_candwin_srcs,
  qt5_candwin_hdrs,
  qt5_candwin_mocs,
  dependencies: qt5_dep,
  include_directories: uim_qt5_inc,
  install: true,
  install_dir: libexecdir,
  link_with: uim_qt5_libs,
  )
