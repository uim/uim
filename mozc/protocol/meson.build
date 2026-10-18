foreach name : [
  'candidate_window',
  'commands',
  'config',
  'engine_builder',
  'user_dictionary_storage',
]
  proto = meson.current_source_dir() / name + '.proto'
  mozc_proto_sources += custom_target(name + '.pb',
                                      input: proto,
                                      output: [name + '.pb.cc',
                                               name + '.pb.h'],
                                      command: mozc_protoc_command + [proto])
endforeach
