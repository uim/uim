EXTRA_DIST = README.ja

if UIM_EL

uimel_lispdir = $(UIMEL_LISP_DIR)
dist_uimel_lisp_DATA = uim.el uim-var.el uim-keymap.el uim-key.el uim-util.el \
 uim-candidate.el uim-preedit.el uim-debug.el uim-leim.el uim-helper.el

bin_PROGRAMS = uim-el-agent uim-el-helper-agent

uim_el_agent_SOURCES = uim-el-agent.c debug.c output.c context.c preedit.c \
 key.c candidate.c encoding.c im.c callback.c commit.c \
 helper.c prop.c helper-message.c callback.h candidate.h commit.h context.h \
 debug.h encoding.h helper.h im.h key.h output.h preedit.h \
 prop.h uim-el-agent.h helper-message.h

uim_el_helper_agent_SOURCES = uim-el-helper-agent.c uim-el-helper-agent.h \
 helper-message.c helper-message.h helper-server.c helper-server.h output.c \
 output.h debug.c debug.h

uim_el_agent_CPPFLAGS = -I$(top_srcdir)
uim_el_agent_LDADD = $(top_builddir)/uim/libuim.la

uim_el_helper_agent_CPPFLAGS = -I$(top_srcdir)
uim_el_helper_agent_LDADD = $(top_builddir)/uim/libuim.la

endif
