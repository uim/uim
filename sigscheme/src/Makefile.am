#noinst_LTLIBRARIES = libsscm.la
lib_LTLIBRARIES = libsscm.la

SCRIPT_DIR = $(top_srcdir)/tools
BUILD_FUNCTBL = $(SCRIPT_DIR)/build_func_table.rb
BUILD_FUNCTBL_CMD = \
        ruby -I$(top_srcdir)/tools $(SCRIPT_DIR)/build_func_table.rb
BUILD_FUNCTBL_DEPS = \
        $(BUILD_FUNCTBL) \
        $(SCRIPT_DIR)/scm_decl.rb \
        $(SCRIPT_DIR)/functable-header.txt \
        $(SCRIPT_DIR)/functable-footer.txt

FUNC_TABLES = \
        functable-sscm-core.c \
        functable-r5rs-syntax.c \
        functable-r5rs-procedure.c \
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

SSCM_PROC_SRCS = error.c module.c
R5RS_PROC_SRCS = eval.c procedure.c list.c
if USE_NUMBER
  R5RS_PROC_SRCS += number.c
endif
if USE_CHAR
  R5RS_PROC_SRCS += char.c
endif
if USE_STRING
  R5RS_PROC_SRCS += string.c
endif
if USE_VECTOR
  R5RS_PROC_SRCS += vector.c
endif
if USE_PORT
  R5RS_PROC_SRCS += port.c
endif
if USE_READER
  R5RS_PROC_SRCS += read.c
endif
if USE_WRITER
  R5RS_PROC_SRCS += write.c
endif
if USE_LOAD
  R5RS_PROC_SRCS += load.c
endif
if USE_DEEP_CADRS
  R5RS_PROC_SRCS += deep-cadrs.c
endif

# For developers
.PHONY: func-tables clean-func-tables
func-tables: $(FUNC_TABLES)
clean-func-tables:
	rm -rf $(FUNC_TABLES)

functable-sscm-core.c: $(SSCM_PROC_SRCS) $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_sscm_core_func_info_table" \
	  $(SSCM_PROC_SRCS)
functable-r5rs-syntax.c: syntax.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_r5rs_syntax_func_info_table" syntax.c
functable-r5rs-procedure.c: $(R5RS_PROC_SRCS) $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_r5rs_procedure_func_info_table" \
	  $(R5RS_PROC_SRCS)
functable-sscm-ext.c: module-sscm-ext.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_sscm_ext_func_info_table" $<
functable-srfi1.c: module-srfi1.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_srfi1_func_info_table" $<
functable-srfi2.c: module-srfi2.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_srfi2_func_info_table" $<
functable-srfi6.c: module-srfi6.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_srfi6_func_info_table" $<
functable-srfi8.c: module-srfi8.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_srfi8_func_info_table" $<
functable-srfi23.c: module-srfi23.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_srfi23_func_info_table" $<
functable-srfi28.c: module-srfi28.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_srfi28_func_info_table" $<
functable-srfi34.c: module-srfi34.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_srfi34_func_info_table" $<
functable-srfi38.c: module-srfi38.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_srfi38_func_info_table" $<
functable-srfi48.c: module-srfi48.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_srfi48_func_info_table" $<
functable-srfi60.c: module-srfi60.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_srfi60_func_info_table" $<
functable-siod.c: module-siod.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL_CMD) $@ "scm_siod_func_info_table" $<

sigscheme-combined.c: $(libsscm_la_SOURCES)
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

# Internal macros trimmer for combined-source version of SigScheme.
# All source files are processed regardless of user configuration.
sigscheme-combined-trim.h: $(DIST_SOURCES)
	@echo 'generating $@'
	@echo "/* This is an auto-generated file. Don't edit directly. */" >$@
	@for src in $^ $>; do                   \
	    case $$src in                       \
	    config.h)          ;;               \
	    my-stdint.h)       ;;               \
	    scmint.h)          ;;               \
	    encoding.h)        ;;               \
	    encoding-config.h) ;;               \
	    scmport.h)         ;;               \
	    scmport-config.h)  ;;               \
	    scmport-null.h)    ;;               \
	    scmport-file.h)    ;;               \
	    scmport-str.h)     ;;               \
	    scmport-sbchar.h)  ;;               \
	    scmport-mbchar.h)  ;;               \
	    storage-*.h)       ;;               \
	    sigscheme.h)       ;;               \
	    main.c)            ;;               \
	    *.h | *.c)                          \
		echo >>$@;                      \
		perl -ne 'BEGIN { print "/* $$ARGV[0] */\n"; } if (s/^\s*#define\s+(\w+).*$$/#undef \1/) { print if (!$$macros{$$1} && $$1 !~ /^__\w+_H$$/); $$macros{$$1} = true; }' $$src >>$@; \
		;;                              \
	    esac                                \
	done

# Since no target of sigscheme directory uses sigscheme-combined.h as source,
# the file is not listed into *_HEADERS.
EXTRA_DIST = $(FUNC_TABLES) $(BUILD_FUNCTBL_DEPS) \
             sigscheme-combined.h sigscheme-combined-trim.h \
             template.h template.c

# Only works on 'make all', 'make check' and 'make install'.
# See "Built sources" section of automake info.
BUILT_SOURCES = $(FUNC_TABLES) sigscheme-combined-trim.h sigscheme-combined.c

pkginclude_HEADERS = config.h my-stdint.h scmint.h global.h sigscheme.h

libsscm_la_SOURCES = \
        alloc.c storage.c storage-gc.c storage-symbol.c \
        error.c env.c eval.c syntax.c procedure.c list.c \
        module.c \
        sigschemeinternal.h sigscheme.c
if USE_STORAGE_COMPACT
  pkginclude_HEADERS += storage-compact.h
else
  pkginclude_HEADERS += storage-fatty.h
endif
if USE_CONTINUATION
  libsscm_la_SOURCES += storage-continuation.c
endif
if USE_NUMBER
  libsscm_la_SOURCES += number.c
endif
if USE_CHAR
  libsscm_la_SOURCES += char.c
endif
if USE_STRING
  libsscm_la_SOURCES += string.c
endif
if USE_VECTOR
  libsscm_la_SOURCES += vector.c
endif
if USE_PORT
  libsscm_la_SOURCES += \
        port.c scmport.h scmport-config.h \
        scmport-file.h scmport-file.c \
        scmport-basechar.c
if USE_MULTIBYTE_CHAR
    libsscm_la_SOURCES += scmport-mbchar.h scmport-mbchar.c
else
    libsscm_la_SOURCES += scmport-sbchar.h scmport-sbchar.c
endif
endif
if USE_READER
  libsscm_la_SOURCES += read.c
endif
if USE_WRITER
  libsscm_la_SOURCES += write.c
endif
if USE_LOAD
  libsscm_la_SOURCES += load.c
endif
if USE_DEEP_CADRS
  libsscm_la_SOURCES += deep-cadrs.c
endif
if USE_MULTIBYTE_CHAR
  pkginclude_HEADERS += encoding.h
  libsscm_la_SOURCES += encoding-config.h encoding.c
endif
if USE_FORMAT
  libsscm_la_SOURCES += format.c
endif
if USE_SSCM_EXTENSIONS
  libsscm_la_SOURCES += module-sscm-ext.c
endif
if COMPAT_SIOD
  libsscm_la_SOURCES += module-siod.c scmport-null.h scmport-null.c
endif
if USE_SRFI1
  libsscm_la_SOURCES += module-srfi1.c
endif
if USE_SRFI2
  libsscm_la_SOURCES += module-srfi2.c
endif
if USE_SRFI6
  libsscm_la_SOURCES += module-srfi6.c scmport-str.h scmport-str.c
endif
if USE_SRFI8
  libsscm_la_SOURCES += module-srfi8.c
endif
if USE_SRFI23
  libsscm_la_SOURCES += module-srfi23.c
endif
if USE_SRFI28
  libsscm_la_SOURCES += module-srfi28.c
endif
if USE_SRFI34
  libsscm_la_SOURCES += module-srfi34.c
endif
if USE_SRFI38
  libsscm_la_SOURCES += module-srfi38.c
endif
if USE_SRFI48
  libsscm_la_SOURCES += module-srfi48.c
endif
if USE_SRFI60
  libsscm_la_SOURCES += module-srfi60.c
endif
libsscm_la_LIBADD = $(LIBOBJS)

libsscm_la_CFLAGS   = -Wall

bin_PROGRAMS  = sscm
sscm_SOURCES  = main.c
sscm_CFLAGS   = -Wall
sscm_LDADD    = $(top_builddir)/src/libsscm.la

# To enable ruby-and-perl-less build on tarball distribution, we should not
# delete $(FUNC_TABLES) and sigscheme-combined-trim.h here.
CLEANFILES = sigscheme-combined.c
