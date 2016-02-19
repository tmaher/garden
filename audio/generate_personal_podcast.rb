#!/usr/bin/env ruby
#
# derived from https://gist.github.com/lukf/44cbd90dfa86c6cd3131
#
# by Kelan Champagne http://yeahrightkeller.com
# with edits by sjschultze
# and advanced metadata handling by lukf
#
# A script to generate a personal podcast feed
#
# Put this, and some .mp3 or .m4a files in a sub-dir under your Dropbox
# Public folder, create your config.txt file in the same directory, and
# run the script.  To get the public_url_base value, you can right click
# on a file in that folder in Finder, then go to Dropbox > Copy Public Link,
# and then remove the filename.
#
# iTunes recommends artwork in the JPEG or PNG file formats and in
# the RGB color space with a minimum size of 1400 x 1400 pixels and
# a maximum size of 2048 x 2048 pixels. You'll need a *direct* link to
# the image, hosted wherever you want. I use cl.ly.
#
# The following lines are a template for your config.txt file;
# just remove the hashes:
#
# podcast_title = The Adventures Of Harry Lime
# podcast_description = Orson Welles' radio drama, between 1951 and 1952
# podcast_artwork = http://cl.ly/image/2x3y3A2l1P2S/01.%20Too%20Many%20Crooks.jpg
# public_url_base = https://dl.dropboxusercontent.com/u/55322715/Audio/The%20Adventures%20Of%20Harry%20Lime
#
# Todo:
#  * Parse items' source date, instead of passing through
#  * Do even more sanitising of the metadata.
#
# Notes:
#  * You'll need to re-run it after adding new files to the dir, or you can
#    set up Folder Actions as suggested by the above hint (sample AppleScript
#    in comments at the bottom of this file).
#  * This script uses `ffprobe` to get the source media metadata.
#    You'll need to have the binary installed (in /usr/bin on OS X).

require 'date'
require 'time'
require 'digest'
require 'erb'

include ERB::Util

MIMES = { :default => "audio/mpeg",
  ".mp3" => "audio/mpeg",
  ".m4a" => "audio/x-m4a",
  ".m4b" => "audio/x-m4b",
  ".mp4" => "video/mp4",
  ".m4v" => "video/x-m4v",
  ".mov" => "video/quicktime",
  ".epub" => "document/x-epub"
}

# Import configuration data
conf={}
conf_regexp = /^\s*(\w[\w]\w*)\s*=\s*(.*)\s*$/
IO.readlines("config.txt").select {|l| l.match(conf_regexp)}.each do |line|
  key, val =  line.match(conf_regexp)[1,2]
  conf[key.gsub(/^(podcast|public)_/, '').to_sym] = val.to_s
end

# Generated values
conf[:pub_date] = Time.now.rfc822
conf[:url_homepage] ||= conf[:url_base]

# Build the items
items_content = ""
item_iter=0
asset_files = Dir.glob("*.{mp3,m4a,m4b}")
asset_files.sort.each do |file|
  puts "adding file: #{file}"
  file_short = file.gsub(/\.(mp3|m4a|m4b)$/, '')
  item_iter += 1
  file_num = file.match(/\A\d+-/) ? file.match(/\A(\d+)-/)[1] : item_iter

  probe = `ffprobe 2> /dev/null -show_format \"#{file}\"`

  item = {}
  raw = {}
  tag_regexp = /^(\w[\w:]+)=(.*)$/
  probe.split("\n").select { |x| x.match(tag_regexp) }.each do |tag|
    key, val = tag.match(tag_regexp)[1,2]
    raw[key.gsub(/^TAG:/, '_').to_sym] = val.to_s
  end
  raw[:_track] ||= "#{file_num}"

  item[:title] = "#{raw[:_track]}. #{(raw[:_title] || file_short).gsub(/\A\d+-/, '')}".gsub(/^\. /, '')
  item[:artist] = raw[:_artist] || raw[:_albumartist] || item[:title]
  item[:duration] = raw[:_duration_time] || raw[:duration].to_i
  item[:url] = "#{conf[:url_base]}/#{url_encode(file)}"
  item[:guid] = Digest::SHA256.file(file).hexdigest
  item[:mime] = MIMES[File.extname(file).downcase] || MIMES[:default]
  item[:size] = File.size(file).to_s
  item[:category] = raw[:_genre] || "Podcasts"

  item[:pub_date] = begin
    # Time.parse(raw[:_date]).rfc822
    # use fake pub_date to get sort order right
    (Time.at(0) + ((item_iter * 86400 * 365.24)/4)).rfc822
  rescue Exception => e
    puts "#{e}: faking the time"
    Time.now.rfc822
  end

  item[:desc_short] = raw[:_description] || raw[:_synopsis] || item[:artist] || ""
  item[:desc_long] = raw[:_synopsis] || raw[:_comment] || raw[:_description] || item[:artist] || ""

  item_content = <<-HTML
    <item>
      <title>#{item[:title].encode(:xml => :text)}</title>
      <description>#{item[:desc_long].encode(:xml => :text)}</description>
      <itunes:subtitle>#{item[:desc_short].encode(:xml => :text)}</itunes:subtitle>
      <itunes:summary>#{item[:desc_short].encode(:xml => :text)}</itunes:summary>
      <enclosure url=#{item[:url].encode(:xml => :attr)}
        length=#{item[:size].encode(:xml => :attr)}
        type=#{item[:mime].encode(:xml => :attr)} />
      <category>#{item[:category].encode(:xml => :text)}</category>
      <pubDate>#{item[:pub_date].encode(:xml => :text)}</pubDate>
      <guid isPermaLink="false">#{item[:guid].encode(:xml => :text)}</guid>
      <itunes:author>#{item[:artist].encode(:xml => :text)}</itunes:author>
      <itunes:duration>#{item[:duration].to_s.encode(:xml => :text)}</itunes:duration>
    </item>
HTML

  items_content << item_content
end

# Build the whole file
content = <<-HTML
<rss xmlns:itunes="http://www.itunes.com/dtds/podcast-1.0.dtd" version="2.0">
  <channel>
    <title>#{conf[:title].encode(:xml => :text)}</title>
    <link>#{conf[:url_homepage].encode(:xml => :text)}</link>
    <description>#{conf[:description].encode(:xml => :text)}</description>
    <pubDate>#{conf[:pub_date].encode(:xml => :text)}</pubDate>
    <itunes:image href=#{conf[:artwork].encode(:xml => :attr)} />
    <itunes:subtitle>#{conf[:description].to_s[0,254].encode(:xml => :text)}</itunes:subtitle>
    <itunes:summary>#{conf[:description].to_s[0,3999].encode(:xml => :text)}</itunes:summary>
#{items_content}
  </channel>
</rss>
HTML

# write it out
output_file = File.new("podcast.rss", 'w')
re = "[^\x09\x0A\x0D\x20-\uD7FF\uE000-\uFFFD\u10000-\u10FFFF]" # Regex for avoiding XML problems
content.gsub(re, "")
output_file.write(content)
output_file.close

# = Sample AppleScript to auto-run this script. =
# This AppleScript also touches the new file so that it's modification
# date (and thus the pubDate in the podcast) are the date/time that you
# put it in the folder.
#
# To install:
# - Open AppleScript Editor and copy-paste the below code (minus #'s)
# - Save the script to "/Library/Scripts/Folder Action Scripts"
# - Control-click the podcast folder, "Services > Folder Actions Setup"
#   and choose your script
#
#on adding folder items to this_folder after receiving added_items
# 	set the_folder to POSIX path of this_folder
# 	set the_folder_quoted to (the quoted form of the_folder as string)
#
# 	repeat with this_item in added_items
# 		set the_item to POSIX path of this_item
# 		set the_item_quoted to (the quoted form of the_item as string)
# 		do shell script "touch " & the_item_quoted
# 	end repeat
#
# 	tell application "Finder"
# 		display dialog "cd " & the_folder_quoted & ";./generate_personal_podcast.rb"
# 		do shell script "cd " & the_folder_quoted & ";./generate_personal_podcast.rb"
# 	end tell
#
# end adding folder items to
