EXTRA_DIST = README.jp

if UIM_EL

uimel_lispdir = $(UIMEL_LISP_DIR)
dist_uimel_lisp_DATA = uim.el uim-var.el uim-keymap.el uim-key.el uim-util.el \
 uim-candidate.el uim-preedit.el uim-debug.el uim-leim.el

bin_PROGRAMS = uim-el-agent

uim_el_agent_SOURCES = uim-el-agent.c debug.c output.c context.c preedit.c \
 key.c candidate.c encoding.c im.c callback.c commit.c helper-server.c \
 helper.c prop.c callback.h candidate.h commit.h context.h debug.h \
 encoding.h helper-server.h helper.h im.h key.h output.h preedit.h \
 prop.h uim-el-agent.h

uim_el_agent_CPPFLAGS = -I$(top_srcdir)
uim_el_agent_LDADD = $(top_builddir)/uim/libuim.la

endif
