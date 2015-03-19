scmdir = $(pkgdatadir)

MOSTLYCLEANFILES = $(GENERATED_SCM_FILES)
dist_scm_DATA = $(SCM_FILES)
scm_DATA = $(GENERATED_SCM_FILES)
GENERATED_SCM_FILES = installed-modules.scm loader.scm
SCM_FILES = plugin.scm im.scm im-custom.scm lazy-load.scm init.scm \
 im-switcher.scm \
 default.scm \
 util.scm deprecated-util.scm ichar.scm light-record.scm wlos.scm \
 ustr.scm trec.scm i18n.scm iso-639-1.scm \
 event.scm ng-key.scm ng-action.scm composer.scm \
 key.scm action.scm load-action.scm \
 uim-sh.scm editline.scm custom.scm custom-rt.scm \
 uim-help.scm \
 direct.scm \
 rk.scm \
 generic.scm generic-custom.scm generic-key-custom.scm \
 generic-predict.scm predict-custom.scm \
 predict-look.scm predict-look-skk.scm predict-sqlite3.scm \
 predict-google-suggest.scm \
 pyload.scm py.scm pyunihan.scm pinyin-big5.scm \
 xmload.scm \
 japanese.scm japanese-azik.scm japanese-kana.scm \
 japanese-act.scm japanese-kzik.scm japanese-custom.scm \
 anthy.scm anthy-custom.scm anthy-key-custom.scm \
 anthy-utf8.scm anthy-utf8-custom.scm \
 canna.scm cannav3-socket.scm canna-custom.scm canna-key-custom.scm \
 wnn.scm wnn-custom.scm wnn-key-custom.scm \
 sj3.scm sj3-custom.scm sj3-key-custom.scm sj3v2-socket.scm \
 prime.scm prime-custom.scm prime-key-custom.scm \
 skk.scm skk-editor.scm skk-custom.scm skk-key-custom.scm skk-dialog.scm \
 mana.scm mana-custom.scm mana-key-custom.scm \
 tcode.scm trycode.scm \
 tutcode.scm tutcode-custom.scm tutcode-key-custom.scm tutcode-bushu.scm \
 tutcode-bushudic.scm tutcode-rule.scm tutcode-kigoudic.scm \
 tutcode-editor.scm tutcode-dialog.scm tutcode-kigou-rule.scm \
 tutcode-rule-custom.scm \
 hangul.scm hangul2.scm hangul3.scm romaja.scm \
 byeoru.scm byeoru-symbols.scm \
 byeoru-custom.scm byeoru-key-custom.scm \
 viqr.scm \
 ipa-x-sampa.scm \
 latin.scm \
 elatin.scm elatin-rules.scm elatin-custom.scm \
 look.scm look-custom.scm \
 m17nlib.scm m17nlib-custom.scm \
 zaurus.scm \
 social-ime.scm social-ime-custom.scm social-ime-key-custom.scm \
 ajax-ime.scm ajax-ime-custom.scm ajax-ime-key-custom.scm \
 yahoo-jp.scm yahoo-jp-custom.scm yahoo-jp-key-custom.scm \
 google-cgiapi-jp.scm google-cgiapi-jp-custom.scm google-cgiapi-jp-key-custom.scm \
 baidu-olime-jp.scm baidu-olime-jp-custom.scm baidu-olime-jp-key-custom.scm \
 uim-module-manager.scm \
 fileio.scm socket.scm process.scm \
 openssl.scm \
 sqlite3.scm \
 lolevel.scm \
 input-parse.scm match.scm pregexp.scm \
 packrat.scm \
 json.scm json-parser-expanded.scm \
 http-client.scm http-server.scm \
 sxml-tools.scm sxpathlib.scm \
 annotation.scm annotation-custom.scm annotation-dict.scm annotation-eb.scm \
 annotation-filter.scm annotation-osx-dcs.scm \
 xkb.scm \
 dynlib.scm \
 ct.scm \
 dict-socket.scm

OTHER_SCM_FILES = \
 wb86.scm zm.scm \
 macro-expander.scm macro-expander-chicken.scm \
 json-parser-expander.scm json.scm.in packrat.scm.in
EXTRA_DIST = $(OTHER_SCM_FILES)

ETAGS_ARGS=$(SCM_FILES) $(GENERATED_SCM_FILES)

module_names =
if ANTHY
  module_names += "anthy"
endif
if ANTHY_UTF8
  module_names += "anthy-utf8"
endif
if CANNA
  module_names += "canna"
endif
if WNN
  module_names += "wnn"
endif
if MANA
  module_names += "mana"
endif
if PRIME
  module_names += "prime"
endif
if SJ3
  module_names += "sj3"
endif
if SKK
  module_names += "skk"
endif
#module_names += "tcode" "trycode"
module_names += "tutcode" "byeoru" "latin" "elatin"
if M17NLIB
  module_names += "m17nlib"
endif
module_names += "xmload"
# less preferable IMs
module_names += "pyload" "viqr" "ipa-x-sampa"  "look"
#"hangul"
# web based IMs
  module_names += "ajax-ime" "social-ime" "google-cgiapi-jp" "baidu-olime-jp"
if EXPAT
  module_names += "yahoo-jp"
endif

UIM_MODULE_MANAGER = $(top_builddir)/uim/uim-module-manager
UIM_MODULE_MANAGER_ENV = \
        LIBUIM_SYSTEM_SCM_FILES=$(abs_top_srcdir)/sigscheme/lib \
        LIBUIM_SCM_FILES=$(abs_srcdir) \
        LIBUIM_PLUGIN_LIB_DIR=$(abs_top_builddir)/uim/.libs \
        UIM_DISABLE_NOTIFY=1

loader.scm: installed-modules.scm

installed-modules.scm: $(SCM_FILES)
	$(MAKE) $(AM_MAKEFLAGS) -C $(top_builddir)/uim uim-module-manager && \
	$(UIM_MODULE_MANAGER_ENV) $(UIM_MODULE_MANAGER) \
	    --path $(top_builddir)/scm --register $(module_names)

json-parser-expanded.scm: macro-expander.scm json-parser-expander.scm packrat.scm.in
	if test -n "$(CSI)"; then \
	    $(CSI) -R syntax-case -q json-parser-expander.scm > json-parser-expanded.scm; \
	fi
