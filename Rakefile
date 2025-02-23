# -*- ruby -*-

def version
  configure_ac = File.read("configure.ac")
  major = configure_ac[/m4_define\(\[UIM_MAJOR_VERSION\],\s*\[(\d+)\]/, 1]
  minor = configure_ac[/m4_define\(\[UIM_MINOR_VERSION\],\s*\[(\d+)\]/, 1]
  patch = configure_ac[/m4_define\(\[UIM_PATCHLEVEL_VERSION\],\s*\[(\d+)\]/, 1]
  "#{major}.#{minor}.#{patch}"
end

desc "Tag"
task :tag do
  sh("git", "tag", "-a", version, "-m", "uim #{version}!!!")
  sh("git", "push", "origin", version)
end

namespace :version do
  desc "Bump version"
  task :bump do
    next_version = version.succ
    next_major, next_minor, next_patch = next_version.split(".")
    configure_ac =
      File.read("configure.ac").
        gsub(/(UIM_MAJOR_VERSION\],\s*\[)\d+/) {"#{$1}#{next_major}"}.
        gsub(/(UIM_MINOR_VERSION\],\s*\[)\d+/) {"#{$1}#{next_minor}"}.
        gsub(/(UIM_PATCHLEVEL_VERSION\],\s*\[)\d+/) {"#{$1}#{next_patch}"}
    File.write("configure.ac", configure_ac)
    sh("git", "add", "configure.ac")
    sh("git", "commit", "-m", "Bump version")
    sh("git", "push")
  end
end

desc "Release"
task :release => ["tag", "version:bump"]
