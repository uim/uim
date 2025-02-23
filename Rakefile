# -*- ruby -*-

def version
  configure_ac = File.read("configure.ac")
  major = configure_ac[/m4_define\(\[UIM_MAJOR_VERSION\],\s*\[(\d+)\]/, 1]
  minor = configure_ac[/m4_define\(\[UIM_MINOR_VERSION\],\s*\[(\d+)\]/, 1]
  patch = configure_ac[/m4_define\(\[UIM_PATCHLEVEL_VERSION\],\s*\[(\d+)\]/, 1]
  "#{major}.#{minor}.#{patch}"
end

desc "Release"
task :release do
  sh("echo", "git", "tag", "-a", version, "-m", "uim #{version}!!!")
  sh("echo", "git", "push", "origin", version)
end
