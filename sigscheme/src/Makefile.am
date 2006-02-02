#noinst_LTLIBRARIES = libsscm.la
lib_LTLIBRARIES = libsscm.la

SCRIPT_DIR = ./script
BUILD_FUNCTBL = $(SCRIPT_DIR)/build_func_table.rb
BUILD_FUNCTBL_DEPS = \
        $(BUILD_FUNCTBL) \
        $(SCRIPT_DIR)/scm_decl.rb \
        $(SCRIPT_DIR)/functable-header.txt \
        $(SCRIPT_DIR)/functable-footer.txt

FUNC_TABLES = \
        functable-sscm-core.c \
        functable-r5rs-syntax.c \
        functable-r5rs-procedure.c \
        functable-r5rs-deepcadrs.c \
        functable-nonstd.c \
        functable-siod.c \
        functable-srfi1.c \
        functable-srfi2.c \
        functable-srfi6.c \
        functable-srfi8.c \
        functable-srfi23.c \
        functable-srfi34.c \
        functable-srfi38.c \
        functable-srfi60.c

SSCM_PROC_SRCS = error.c module.c
R5RS_PROC_SRCS = eval.c procedure.c list.c
if USE_NUMBER
  R5RS_PROC_SRCS += number.c
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

.PHONY: func-tables
func-tables: $(FUNC_TABLES)
functable-sscm-core.c: $(SSCM_PROC_SRCS) $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL) $@ "scm_sscm_core_func_info_table" $(SSCM_PROC_SRCS)
functable-r5rs-syntax.c: syntax.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL) $@ "scm_r5rs_syntax_func_info_table" syntax.c
functable-r5rs-procedure.c: $(R5RS_PROC_SRCS) $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL) $@ "scm_r5rs_procedure_func_info_table" \
	  $(R5RS_PROC_SRCS)
functable-r5rs-deepcadrs.c: module-r5rs-deepcadrs.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL) $@ "scm_r5rs_deepcadrs_func_info_table" $<
functable-nonstd.c: module-nonstd.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL) $@ "scm_nonstd_func_info_table" $<
functable-srfi1.c: module-srfi1.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL) $@ "scm_srfi1_func_info_table" $<
functable-srfi2.c: module-srfi2.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL) $@ "scm_srfi2_func_info_table" $<
functable-srfi6.c: module-srfi6.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL) $@ "scm_srfi6_func_info_table" $<
functable-srfi8.c: module-srfi8.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL) $@ "scm_srfi8_func_info_table" $<
functable-srfi23.c: module-srfi23.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL) $@ "scm_srfi23_func_info_table" $<
functable-srfi34.c: module-srfi34.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL) $@ "scm_srfi34_func_info_table" $<
functable-srfi38.c: module-srfi38.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL) $@ "scm_srfi38_func_info_table" $<
functable-srfi60.c: module-srfi60.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL) $@ "scm_srfi60_func_info_table" $<
functable-siod.c: module-siod.c $(BUILD_FUNCTBL_DEPS)
	$(BUILD_FUNCTBL) $@ "scm_siod_func_info_table" $<

EXTRA_DIST = $(FUNC_TABLES) $(BUILD_FUNCTBL_DEPS) test-compact.c

# Only works on 'make all', 'make check' and 'make install'.
# See "Built sources" section of automake info.
BUILT_SOURCES = $(FUNC_TABLES)

pkginclude_HEADERS = sigscheme.h config.h my-stdint.h

libsscm_la_SOURCES = \
        config-nonstd-string.h config-asprintf.h \
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
if USE_STRING
  libsscm_la_SOURCES += string.c
endif
if USE_VECTOR
  libsscm_la_SOURCES += vector.c
endif
if USE_PORT
  libsscm_la_SOURCES += port.c baseport.h basecport.c fileport.h fileport.c
if USE_MULTIBYTE_CHAR
    libsscm_la_SOURCES += mbcport.h mbcport.c
else
    libsscm_la_SOURCES += sbcport.h sbcport.c
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
if USE_MULTIBYTE_CHAR
  pkginclude_HEADERS += encoding.h
  libsscm_la_SOURCES += encoding.c
endif
if USE_DEEP_CADRS
  libsscm_la_SOURCES += module-r5rs-deepcadrs.c
endif
if USE_NONSTD_FEATURES
  libsscm_la_SOURCES += module-nonstd.c
endif
if COMPAT_SIOD
  libsscm_la_SOURCES += module-siod.c nullport.h nullport.c
endif
if USE_SRFI1
  libsscm_la_SOURCES += module-srfi1.c
endif
if USE_SRFI2
  libsscm_la_SOURCES += module-srfi2.c
endif
if USE_SRFI6
  libsscm_la_SOURCES += module-srfi6.c strport.h strport.c
endif
if USE_SRFI8
  libsscm_la_SOURCES += module-srfi8.c
endif
if USE_SRFI23
  libsscm_la_SOURCES += module-srfi23.c
endif
if USE_SRFI34
  libsscm_la_SOURCES += module-srfi34.c
endif
if USE_SRFI38
  libsscm_la_SOURCES += module-srfi38.c
endif
if USE_SRFI60
  libsscm_la_SOURCES += module-srfi60.c
endif

libsscm_la_CFLAGS   = -Wall

bin_PROGRAMS  = sscm
sscm_SOURCES  = main.c
sscm_CFLAGS   = -Wall
sscm_LDADD    = $(top_builddir)/src/libsscm.la

CLEANFILES = \
	$(FUNC_TABLES)
