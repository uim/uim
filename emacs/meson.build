uimel_lispdir = datadir / 'emacs' / 'site-lisp' / 'uim-el'

install_data('uim-candidate.el',
             'uim-debug.el',
             'uim-helper.el',
             'uim-key.el',
             'uim-keymap.el',
             'uim-leim.el',
             'uim-preedit.el',
             'uim-util.el',
             'uim-var.el',
             'uim.el',
             install_dir: uimel_lispdir)
configure_file(input: 'uim-version.el.in',
               output: 'uim-version.el',
               configuration: {'PACKAGE_VERSION': meson.project_version()},
               install_dir: uimel_lispdir)

# They use libreplace, which Autotools links through libuim.
uim_el_agent = executable('uim-el-agent',
                          'callback.c',
                          'candidate.c',
                          'commit.c',
                          'context.c',
                          'debug.c',
                          'encoding.c',
                          'helper-message.c',
                          'helper.c',
                          'im.c',
                          'key.c',
                          'output.c',
                          'preedit.c',
                          'prop.c',
                          'uim-el-agent.c',
                          include_directories: uim_include_directories,
                          install: true,
                          link_with: [libuim_scm, libuim, libreplace])
uim_el_helper_agent = executable('uim-el-helper-agent',
                                 'debug.c',
                                 'helper-message.c',
                                 'helper-server.c',
                                 'output.c',
                                 'uim-el-helper-agent.c',
                                 include_directories: uim_include_directories,
                                 install: true,
                                 link_with: [libuim_scm, libuim, libreplace])

# ERT tests that drive uim.el in a batch Emacs with the agents in the
# build tree.
if get_option('tests').allowed()
  # Emacs is required only when both Emacs support and the tests are
  # enabled explicitly.
  emacs = find_program('emacs',
                       required: (get_option('emacs').enabled() and
                                  get_option('tests').enabled()))
  if emacs.found()
    test('uim.el',
         emacs,
         args: [
           '-Q', '--batch',
           '-L', meson.current_source_dir(),
           # uim-version.el is made in the build tree.
           '-L', meson.current_build_dir(),
           '-l', meson.current_source_dir() / 'test' / 'run.el',
         ],
         depends: [uim_el_agent, uim_el_helper_agent],
         env: {
           'LIBUIM_PLUGIN_LIB_DIR': meson.project_build_root() / 'uim',
           'LIBUIM_SCM_FILES': (meson.project_build_root() / 'scm' + ':' +
                                meson.project_source_root() / 'scm'),
           'LIBUIM_SYSTEM_SCM_FILES': (meson.project_source_root() /
                                       'subprojects' / 'sigscheme' / 'lib'),
           'UIM_EL_AGENT': uim_el_agent.full_path(),
           'UIM_EL_HELPER_AGENT': uim_el_helper_agent.full_path(),
         },
         suite: 'emacs',
         # test/run.el fails the tests after 60 seconds by itself.
         timeout: 90)
  endif
endif
