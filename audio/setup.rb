#!/usr/bin/ruby

require 'erb'
require 'securerandom'
require 'socket'
require 'fileutils'

MYDIR = File.dirname(File.expand_path __FILE__)
SHAIRDIR = File.join(MYDIR, "shairport")
SHAIRPORT_NAME = "Sonos-#{Socket.gethostname.split('.')[0]}"
SEKRIT = SecureRandom.hex(4)
AUDIO_USER = "icecast2"
AUDIO_GROUP = "icecast"

CFG_FILES = {
  "darkice.cfg" => "/etc/icecast2",
  "icecast.xml" => "/etc/icecast2",
  "darkice" => "/etc/default",
  "shairport" => "/etc/init.d"
}

CP_FILES = {
  "shairport/hairtunes" => "/usr/local/bin",
  "shairport/shairport" => "/usr/local/bin",
  "shairport/shairport.pl" => "/usr/local/bin"
}


if Process.uid != 0 then
  puts "sorry, re-run with sudo"
  exit 1
end

`setcap cap_sys_nice+ep /usr/bin/darkice`
`cd #{SHAIRDIR}; make clean ; make`

CFG_FILES.each {|f, dest|
  #File.new(File.join(dest, f), "w").write(ERB.new(f + ".erb").result)
  File.new(File.join(dest, f), "w").write(ERB.new(File.read(f + ".erb")).result)
}

CP_FILES.each {|f, dest| FileUtils.cp(f, dest, :preserve => true) }

`update-rc.d shairport defaults`
`mkdir /var/run/shairport; chown #{AUDIO_USER}:#{AUDIO_GROUP} /var/run/shairport`
