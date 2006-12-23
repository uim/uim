include libtool-version.mk

lib_LTLIBRARIES = libsscm.la

SCRIPT_DIR = $(top_srcdir)/tools
BUILD_FUNCTBL = $(SCRIPT_DIR)/build_func_table.rb
BUILD_FUNCTBL_CMD = $(RUBY) -I$(top_srcdir)/tools $(BUILD_FUNCTBL)
BUILD_FUNCTBL_DEPS = \
        $(BUILD_FUNCTBL) \
        $(SCRIPT_DIR)/scm_decl.rb \
        $(SCRIPT_DIR)/functable-header.txt \
        $(SCRIPT_DIR)/functable-footer.txt

FUNC_TABLES = \
        functable-sscm-core.c \
        functable-r5rs-core.c \
        functable-r5rs-syntax.c \
        functable-r5rs-qquote.c \
        functable-r5rs-macro.c \
        functable-r5rs-promise.c \
        functable-r5rs-number.c \
        functable-r5rs-number-io.c \
        functable-r5rs-char.c \
        functable-r5rs-string.c \
        functable-r5rs-string-procedure.c \
        functable-r5rs-vector.c \
        functable-r5rs-port.c \
        functable-r5rs-read.c \
        functable-r5rs-write.c \
        functable-r5rs-load.c \
        functable-r5rs-deep-cadrs.c \
        functable-sscm-ext.c \
        functable-siod.c \
        functable-srfi1.c \
        functable-srfi2.c \
        functable-srfi6.c \
        functable-srfi8.c \
        functable-srfi23.c \
        functable-srfi28.c \
        functable-srfi34.c \
        functable-srfi38.c \
        functable-srfi48.c \
        functable-srfi60.c

if MAINTAINER_MODE
# Core
SSCM_CORE_SRCS = error.c module.c
R5RS_CORE_SRCS = eval.c procedure.c list.c
functable-sscm-core.c: $(SSCM_CORE_SRCS) $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_functable_sscm_core" $(SSCM_CORE_SRCS)
functable-r5rs-core.c: $(R5RS_CORE_SRCS) $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_functable_r5rs_core" $(R5RS_CORE_SRCS)
# R5RS syntaxes
functable-r5rs-syntax.c: syntax.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_functable_r5rs_syntax" $<
functable-r5rs-qquote.c: qquote.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_functable_r5rs_qquote" $<
functable-r5rs-macro.c: macro.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_functable_r5rs_macro" $<
functable-r5rs-promise.c: promise.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_functable_r5rs_promise" $<
# R5RS procedures
functable-r5rs-number.c: number.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_functable_r5rs_number" $<
functable-r5rs-number-io.c: number-io.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_functable_r5rs_number_io" $<
functable-r5rs-char.c: char.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_functable_r5rs_char" $<
functable-r5rs-string.c: string.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_functable_r5rs_string" $<
functable-r5rs-string-procedure.c: string-procedure.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_functable_r5rs_string_procedure" $<
functable-r5rs-vector.c: vector.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_functable_r5rs_vector" $<
functable-r5rs-port.c: port.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_functable_r5rs_port" $<
functable-r5rs-read.c: read.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_functable_r5rs_read" $<
functable-r5rs-write.c: write.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_functable_r5rs_write" $<
functable-r5rs-load.c: load.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_functable_r5rs_load" $<
functable-r5rs-deep-cadrs.c: deep-cadrs.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_functable_r5rs_deep_cadrs" $<
# Optional modules
functable-sscm-ext.c: module-sscm-ext.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_functable_sscm_ext" $<
functable-siod.c: module-siod.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_functable_siod" $<
functable-srfi1.c: module-srfi1.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_functable_srfi1" $<
functable-srfi2.c: module-srfi2.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_functable_srfi2" $<
functable-srfi6.c: module-srfi6.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_functable_srfi6" $<
functable-srfi8.c: module-srfi8.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_functable_srfi8" $<
functable-srfi23.c: module-srfi23.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_functable_srfi23" $<
functable-srfi28.c: module-srfi28.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_functable_srfi28" $<
functable-srfi34.c: module-srfi34.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_functable_srfi34" $<
functable-srfi38.c: module-srfi38.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_functable_srfi38" $<
functable-srfi48.c: module-srfi48.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_functable_srfi48" $<
functable-srfi60.c: module-srfi60.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_functable_srfi60" $<
endif

# Optional and not built by 'make all'. Build explicitly with 'make combined'
# if needed. This target does not use non-standard scripting languages for
# portability.
sigscheme-combined.c: $(libsscm_sources) config.h
	@echo 'generating $@'
	@echo "/* This is an auto-generated file. Don't edit directly. */" >$@
	@echo >>$@
	@echo '#include "sigscheme-combined.h"' >>$@
	@echo >>$@
	@for src in $^ $>; do                           \
	    case $$src in                               \
	    *.c)                                        \
		echo $(ECHO_N) '#include "' >>$@;       \
		echo $(ECHO_N) $$src >>$@;              \
		echo '"' >>$@;                          \
		;;                                      \
	    esac                                        \
	done
	@echo >>$@
	@echo '#include "sigscheme-combined-trim.h"' >>$@
	@echo >>$@
	@echo '#if SCM_USE_AGGREGATED_GLOBAL_VARS' >>$@
	@echo 'SCM_AGGREGATED_GLOBAL_VARS_BEGIN;' >>$@
	@for src in $^ $>; do                           \
	    case $$src in                               \
	    global.h) ;;                                \
	    *.h | *.c)                                 \
	        $(EGREP) -e '^ *SCM_DEFINE_(STATIC|EXPORTED)_VARS\b' $$src \
	          | sed -e 's/^.*SCM_DEFINE_STATIC_VARS/    SCM_AGGREGATED_GLOBAL_VARS/' \
	                -e 's/^.*SCM_DEFINE_EXPORTED_VARS/    SCM_AGGREGATED_GLOBAL_VARS/' >>$@; \
		;;                                      \
	    esac                                        \
	done
	@echo 'SCM_AGGREGATED_GLOBAL_VARS_END;' >>$@
	@echo >>$@
	@echo '#include "global-aggregated.c"' >>$@
	@echo >>$@
	@for src in $^ $>; do                           \
	    case $$src in                               \
	    global.h) ;;                                \
	    *.h | *.c)                                 \
	        $(EGREP) -e '^ *SCM_DEFINE_(STATIC|EXPORTED)_VARS\b' $$src \
	          | sed -e 's/^.*SCM_DEFINE_STATIC_VARS/SCM_DEFINE_GLOBAL_VARS_INSTANCE_ACCESSOR/' \
	                -e 's/^.*SCM_DEFINE_EXPORTED_VARS/SCM_DEFINE_GLOBAL_VARS_INSTANCE_ACCESSOR/' >>$@; \
		;;                                      \
	    esac                                        \
	done
	@echo '#endif /* SCM_USE_AGGREGATED_GLOBAL_VARS */' >>$@

if MAINTAINER_MODE
# Internal macros trimmer for combined-source version of SigScheme.
# All source files are processed regardless of user configuration.
sigscheme-combined-trim.h: $(DIST_SOURCES)
	@test -n "$(PERL)" || (echo 'perl(1) not found: install Perl'; exit 1)
	@echo 'generating $@'
	@echo "/* This is an auto-generated file. Don't edit directly. */" >$@
	@for src in $^ $>; do                   \
	    case $$src in                       \
	    config-old.h)       ;;              \
	    scmint.h)           ;;              \
	    encoding.h)         ;;              \
	    encoding-dummy.h)   ;;              \
	    encoding-config.h)  ;;              \
	    scmport.h)          ;;              \
	    scmport-config.h)   ;;              \
	    scmport-null.h)     ;;              \
	    scmport-file.h)     ;;              \
	    scmport-str.h)      ;;              \
	    scmport-sbchar.h)   ;;              \
	    scmport-mbchar.h)   ;;              \
	    storage-*.h)        ;;              \
	    sigscheme.h)        ;;              \
	    main.c)             ;;              \
	    *.h | *.c)                          \
		echo >>$@;                      \
		$(PERL) -ne 'BEGIN { print "/* $$ARGV[0] */\n"; } if (s/^\s*#define\s+(\w+).*$$/#undef \1/) { print if (!$$macros{$$1} && $$1 !~ /^__\w+_H$$/); $$macros{$$1} = true; }' $$src >>$@; \
		;;                              \
	    esac                                \
	done
endif

BUILT_DIST = $(FUNC_TABLES) sigscheme-combined-trim.h

# Since no target of sigscheme directory uses sigscheme-combined.h as source,
# the file is not listed into *_HEADERS.
EXTRA_DIST = \
        $(BUILT_DIST) $(BUILD_FUNCTBL_DEPS) \
        sigscheme-combined.h global-aggregated.c template.h template.c

# FIXME: Build dll on supported platforms
EXTRA_DIST += dllentry.c

# Only works on 'make all', 'make check' and 'make install'.
# See "Built sources" section of automake info.
BUILT_SOURCES = $(BUILT_DIST)

nodist_pkginclude_HEADERS = config.h
pkginclude_HEADERS = config-old.h scmint.h global.h sigscheme.h

libsscm_sources = \
        alloc.c storage.c storage-gc.c \
        error.c symbol.c env.c eval.c \
	syntax.c procedure.c list.c \
        sigschemeinternal.h module.c sigscheme.c
if USE_STORAGE_COMPACT
  pkginclude_HEADERS += storage-common.h storage-compact.h
else
  pkginclude_HEADERS += storage-common.h storage-fatty.h
endif
if USE_CONTINUATION
  libsscm_sources += continuation.c
endif
if USE_PORT
  libsscm_sources += \
        scmport.h scmport-config.h \
        scmport-file.h scmport-file.c \
        scmport-basechar.c
if USE_MULTIBYTE_CHAR
  pkginclude_HEADERS += encoding.h
  libsscm_sources    += encoding-config.h encoding.c \
                        scmport-mbchar.h scmport-mbchar.c
else
  pkginclude_HEADERS += encoding-dummy.h
  libsscm_sources    += scmport-sbchar.h scmport-sbchar.c
endif
endif
if USE_FORMAT
  libsscm_sources += format.c
endif
if USE_QUASIQUOTE
  libsscm_sources += qquote.c
endif
# FIXME: support SCM_USE_SYNTAX_CASE, SCM_USE_UNHYGIENIC_MACRO and
# SCM_USE_LEGACY_MACRO
if USE_HYGIENIC_MACRO
  libsscm_sources += macro.c
endif
if USE_PROMISE
  libsscm_sources += promise.c
endif
if USE_NUMBER
  libsscm_sources += number.c
endif
if USE_NUMBER_IO
  libsscm_sources += number-io.c
endif
if USE_CHAR
  libsscm_sources += char.c
endif
if USE_STRING
  libsscm_sources += string.c
endif
if USE_STRING_PROCEDURE
  libsscm_sources += string-procedure.c
endif
if USE_VECTOR
  libsscm_sources += vector.c
endif
if USE_PORT
  libsscm_sources += port.c
endif
if USE_READER
  libsscm_sources += read.c
endif
if USE_WRITER
  libsscm_sources += write.c
endif
if USE_LOAD
  libsscm_sources += load.c
endif
if USE_DEEP_CADRS
  libsscm_sources += deep-cadrs.c
endif

# optional modules
if USE_SSCM_EXTENSIONS
  libsscm_sources += module-sscm-ext.c
endif
if COMPAT_SIOD
  libsscm_sources += module-siod.c scmport-null.h scmport-null.c
endif
if USE_SRFI1
  libsscm_sources += module-srfi1.c
endif
if USE_SRFI2
  libsscm_sources += module-srfi2.c
endif
if USE_SRFI6
  libsscm_sources += module-srfi6.c scmport-str.h scmport-str.c
endif
if USE_SRFI8
  libsscm_sources += module-srfi8.c
endif
if USE_SRFI23
  libsscm_sources += module-srfi23.c
endif
if USE_SRFI28
  libsscm_sources += module-srfi28.c
endif
if USE_SRFI34
  libsscm_sources += module-srfi34.c
endif
if USE_SRFI38
  libsscm_sources += module-srfi38.c
endif
if USE_SRFI48
  libsscm_sources += module-srfi48.c
endif
if USE_SRFI60
  libsscm_sources += module-srfi60.c
endif

if WITH_LIBGCROOTS
else
libsscm_sources += gcroots/gcroots.h gcroots/gcroots.c
endif

if USE_LIBSSCM
libsscm_la_SOURCES  = $(libsscm_sources)
libsscm_la_LIBADD   = $(LIBOBJS)
libsscm_la_LDFLAGS  = -version-info $(libsscm_version)
libsscm_la_CPPFLAGS = -I$(top_builddir)/include -DSCM_COMPILING_LIBSSCM=1
libsscm_la_CFLAGS   =
if WITH_LIBGCROOTS
libsscm_la_LIBADD   += @GCROOTS_LIBS@
libsscm_la_CFLAGS   += @GCROOTS_CFLAGS@
endif
endif

if USE_SHELL
bin_PROGRAMS  = sscm
sscm_SOURCES  = main.c
sscm_LDADD    = libsscm.la
sscm_CPPFLAGS = -I$(top_builddir)/include
sscm_CFLAGS   =
endif

# To enable ruby-and-perl-less build on tarball distribution, we should not
# delete $(FUNC_TABLES) and sigscheme-combined-trim.h here.
CLEANFILES = sigscheme-combined.c
DISTCLEANFILES =
MAINTAINERCLEANFILES = $(BUILT_DIST)

ETAGS_ARGS = sigschemeinternal.h

.PHONY: lib combined func-tables clean-func-tables clean-built
lib: libsscm.la
combined: sigscheme-combined-trim.h sigscheme-combined.c
clean-combined:
	rm -f sigscheme-combined-trim.h sigscheme-combined.c
func-tables: $(FUNC_TABLES)
clean-func-tables:
	rm -f $(FUNC_TABLES)

dist-hook:
	@test -n "$(RUBY)" || (echo 'ruby(1) not found: install Ruby'; exit 1)
	@test -n "$(PERL)" || (echo 'perl(1) not found: install Perl'; exit 1)
