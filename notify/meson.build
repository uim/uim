libnotify = dependency('libnotify',
                       required: get_option('libnotify'),
                       version: '>=0.4')
if libnotify.found()
  shared_module('uimnotify-libnotify',
                'uim-libnotify.c',
                dependencies: libnotify,
                include_directories: uim_include_directories,
                install: true,
                install_dir: pkglibdir / 'notify')
endif
optional_notify_plugins = {'libnotify': libnotify.found()}
