AUTOMAKE_OPTIONS = subdir-objects

if MOZC
bin_PROGRAMS = uim-mozc-helper
endif

PROTO_FILES = \
	protocol/candidate_window.proto \
	protocol/commands.proto \
	protocol/config.proto \
	protocol/engine_builder.proto \
	protocol/user_dictionary_storage.proto \
	ipc/ipc.proto

PROTO_CC_FILES = $(PROTO_FILES:.proto=.pb.cc)
PROTO_H_FILES = $(PROTO_FILES:.proto=.pb.h)

BUILT_SOURCES = $(PROTO_H_FILES) $(PROTO_CC_FILES)

uim_mozc_helper_SOURCES = uim-mozc-helper.cpp
nodist_uim_mozc_helper_SOURCES = $(PROTO_CC_FILES)
uim_mozc_helper_CPPFLAGS = -I$(builddir) -DUIM_VERSION=\"$(VERSION)\" $(PROTOBUF_CFLAGS)
uim_mozc_helper_CXXFLAGS = $(MOZC_CXXFLAGS)
uim_mozc_helper_LDADD = $(PROTOBUF_LIBS)

EXTRA_DIST = $(PROTO_FILES) README.md

CLEANFILES = $(PROTO_H_FILES) $(PROTO_CC_FILES)

# protoc generates both .pb.cc and .pb.h. The .pb.h rule depends on the
# .pb.cc rule to avoid running protoc twice in parallel builds.
%.pb.cc: %.proto
	$(MKDIR_P) $(builddir)/$(dir $@)
	$(PROTOC) --proto_path=$(srcdir) --cpp_out=$(builddir) $<

%.pb.h: %.pb.cc
	@test -f $@ || rm -f $<
	@test -f $@ || $(MAKE) $(AM_MAKEFLAGS) $<
