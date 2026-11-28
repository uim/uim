qt6_module = import('qt6')

qt6_plugins_dir = qt6.get_variable(configtool: 'QT_INSTALL_PLUGINS')

# The sources are shared with the ones for Qt 4 and Qt 5.
qt6_include_directories = [
  uim_include_directories,
  include_directories('..' / 'qt4'),
  include_directories('..' / 'qt4' / 'candwin'),
  include_directories('..' / 'qt4' / 'immodule'),
  include_directories('..' / 'qt5' / 'immodule'),
]
qt6_link_with = [libuim_x_util, libuim, libuim_scm, libreplace]
# Qt 6 needs C++17.
qt6_override_options = ['cpp_std=c++17']

subdir('candwin')
subdir('immodule')
subdir('switcher')
