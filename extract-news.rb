#!/usr/bin/env ruby

tag_ref = ARGV[0]
if tag_ref
  version =  tag_ref.sub(/\Arefs\/tags\//, "")
else
  version = nil
end
news = File.read(File.join(__dir__, "NEWS"))

entry = ""
news.each_line do |line|
  case line
  when /\AOverview of changes from /
    unless entry.empty?
      if version
        break if entry.lines.first.chomp.end_with?("to #{version}")
      else
        break
      end
      entry.clear
    end
  end
  entry << line
end

puts(entry.strip)
