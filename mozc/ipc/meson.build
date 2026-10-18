mozc_ipc_proto = meson.current_source_dir() / 'ipc.proto'
mozc_proto_sources += custom_target('ipc.pb',
                                    input: mozc_ipc_proto,
                                    output: ['ipc.pb.cc', 'ipc.pb.h'],
                                    command: mozc_protoc_command +
                                             [mozc_ipc_proto])
