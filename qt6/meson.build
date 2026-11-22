qt6 = import('qt6')
qt6_dep = dependency('qt6',
  modules: [
    'Core',
    'Gui',
    'Widgets',
    'Core5Compat',
    ],
  private_headers: true,
  required: get_option('qt6'))

qt6_opts = [
  'cpp_std=c++17',
  ]

if (qt6_dep.found())
  qt6_plugins_dir = get_option('qt6_plugins_dir')
  if (qt6_plugins_dir == '')
    qt6_bindir = qt6_dep.get_variable(pkgconfig: 'bindir')
    qmake6 = find_program('qmake6', 'qmake',
      dirs: qt6_bindir,
      )
    qt6_plugins_dir = run_command(
      qmake6, '-query', 'QT_INSTALL_PLUGINS',
      check: true,
    ).stdout()
  endif
  message(f'Qt6 pluginsdir: @qt6_plugins_dir@')
  qt6_icons_dir = datadir / 'icons'

  uim_qt6_inc = [
    uim_include_directories,
    include_directories('../qt4'),
    ]
  uim_qt6_libs = [
    libreplace,
    libuim,
    libuim_scm,
    ]

  if (x11.found())
    uim_qt6_libs += libuim_x_util
  endif

  subdir('candwin')
  subdir('chardict')
  subdir('edittest')
  subdir('immodule')
  subdir('pref')
  subdir('switcher')
  subdir('toolbar')
endif
