docs = [
  '00INDEX',
  'COMPATIBILITY',
  'DOT-UIM',
  'ENV',
  'HELPER-PROTOCOL',
  'KEY',
  'PACKAGING',
  'PLUGIN',
  'UIM-SCM',
  'UIM-SH',
  'UNIT-TEST',
  'action.md',
  'custom.md',
  'karukan.md',
  'tutcode-utf8-migration-guide-ja.md',
  'tutcode-utf8-migration-guide.md',
  'uim-custom-cli.md',
  'uim-im-switcher-cli.md',
]
# The documents of the optional components are installed only with
# them.
if get_option('skk')
  docs += ['skk-utf8-migration-guide-ja.md', 'skk-utf8-migration-guide.md']
endif
if gio.found()
  docs += ['uim-sni.md']
endif
if rime.found()
  docs += ['rime.md']
endif
if build_wayland
  docs += ['uim-wayland.md']
endif
if build_xim
  docs += ['HELPER-CANDWIN']
endif
install_data(docs, install_dir: docdir / 'doc', install_tag: 'doc')
