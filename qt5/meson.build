qt5 = import('qt5')
qt5_dep = dependency('qt5',
  modules: ['Core', 'Gui', 'Widgets', 'X11Extras'],
  private_headers: true,
  required: get_option('qt5'))

if (qt5_dep.found())
  qt5_plugins_dir = get_option('qt5_plugins_dir')
  if (qt5_plugins_dir == '')
    qmake5 = find_program('qmake5', 'qmake-qt5', required: true)
    qt5_plugins_dir = run_command(
      qmake5, '-query', 'QT_INSTALL_PLUGINS',
      check: true,
    ).stdout()
  endif
  qt5_icons_dir = datadir / 'icons'

  uim_qt5_inc = [
    uim_include_directories,
    include_directories('../qt4'),
    ]
  uim_qt5_libs = [
    libreplace,
    libuim,
    libuim_scm,
    ]

  if (x11.found())
    uim_qt5_libs += libuim_x_util
  endif
  
  subdir('candwin')
  subdir('chardict')
  subdir('edittest')
  subdir('immodule')
  subdir('pref')
  subdir('switcher')
  subdir('toolbar')
endif
