# protoc writes the code for protocol/foo.proto to protocol/foo.pb.cc
# and protocol/foo.pb.h in --cpp_out, and the generated code includes
# them by those paths. Each directory generates its own files. protoc
# needs the .proto paths under --proto_path as they are written, so
# they are absolute here.
mozc_protoc_command = [
  protoc,
  '--proto_path=' + meson.current_source_dir(),
  '--cpp_out=' + meson.current_build_dir(),
]
mozc_proto_sources = []
subdir('ipc')
subdir('protocol')

executable('uim-mozc-helper',
           'uim-mozc-helper.cpp',
           mozc_proto_sources,
           cpp_args: ['-DUIM_VERSION="@0@"'.format(meson.project_version())],
           dependencies: protobuf,
           install: true,
           override_options: ['cpp_std=c++17'])
