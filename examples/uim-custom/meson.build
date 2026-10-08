foreach name : ['uim-custom-dump', 'uim-custom-update', 'uim-custom-variable']
  executable(name,
             name + '.c',
             include_directories: uim_include_directories,
             link_with: [libuim_custom, libuim_scm, libuim])
endforeach
