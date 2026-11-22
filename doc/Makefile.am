EXTRA_DIST = \
	00INDEX \
	COMPATIBILITY \
	DOT-UIM \
	ENV \
	HELPER-CANDWIN \
	HELPER-PROTOCOL \
	KEY \
	PACKAGING \
	PLUGIN \
	UIM-SCM \
	UIM-SH \
	UNIT-TEST \
	action.md \
	custom.md \
	karukan.md \
	skk-utf8-migration-guide-ja.md \
	skk-utf8-migration-guide.md \
	meson.build \
	tutcode-utf8-migration-guide-ja.md \
	tutcode-utf8-migration-guide.md \
	uim-custom-cli.md \
	uim-im-switcher-cli.md \
	uim-sni.md \
	uim-wayland.md

# The documents are installed into docdir with their paths in the
# source as Meson does.
docdocdir = $(docdir)/doc
docdoc_DATA = \
	00INDEX \
	COMPATIBILITY \
	DOT-UIM \
	ENV \
	HELPER-PROTOCOL \
	KEY \
	PACKAGING \
	PLUGIN \
	UIM-SCM \
	UIM-SH \
	UNIT-TEST \
	action.md \
	custom.md \
	karukan.md \
	tutcode-utf8-migration-guide-ja.md \
	tutcode-utf8-migration-guide.md \
	uim-custom-cli.md \
	uim-im-switcher-cli.md
# The documents of the optional components are installed only with
# them. uim-sni.md isn't installed because configure doesn't build
# uim-sni.
if SKK
docdoc_DATA += \
	skk-utf8-migration-guide-ja.md \
	skk-utf8-migration-guide.md
endif
if WAYLAND
docdoc_DATA += uim-wayland.md
endif
if XIM
docdoc_DATA += HELPER-CANDWIN
endif
