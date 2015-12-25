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
require 'erb'

include ERB::Util

# Import configuration data
conf={}
conf_regexp = /^\s*(\w[\w\_]\w*)\s*=\s*(.*)\s*$/
IO.readlines("config.txt").select {|l| l.match(conf_regexp)}.each do |line|
  key, val =  line.match(conf_regexp)[1,2]
  conf[key.gsub(/^(podcast|public)_/, '').to_sym] = val.to_s
end

# Generated values
conf[:pub_date] = Time.now.to_s
conf[:url_homepage] ||= conf[:url_base]

# Build the items
items_content = ""
Dir.glob("*.{mp3,m4a}"). each do |file|
  puts "adding file: #{file}"
  file_short = file.gsub /\.(mp3|m4a)$/, ''
  file_ext = File.extname file

  probe = `ffprobe 2> /dev/null -show_format \"#{file}\"`

  item = {}
  raw = {}
  tag_regexp = /^(\w[\w:_]+)=(.+)$/
  probe.split("\n").select { |x| x.match(tag_regexp) }.each do |tag|
    key, val = tag.match(tag_regexp)[1,2]
    raw[key.gsub(/^TAG:/, '_').to_sym] = val.to_s
  end

  item[:title] = "#{raw[:_track]}. #{(raw[:_title] || file_short)}".gsub(/^\. /, '')

  # Aquiring source metadata
  #item_filename = File.basename(file, '').split('.')[0]

  #item_title_number = tags[:track] || ""
  #item_title_source = tags[:title] || ""
  item_text_artist = raw[:_artist] || ""
  item_text_albumartist = raw[:_albumartist] || ""
  item_text_description = raw[:_description] || ""
  item_text_synopsis = raw[:_synopsis] || ""
  item_text_comment = raw[:_comment] || ""
  item_duration_source = raw[:_duration_time] || ""
  item_pub_date_source = raw[:_date] || ""

  # Convert number to ordinal
  #if item_title_number != ""
  #  item_title_number += ". "
  #end

  # Get correct artist; defaulting to artist
  if item_text_artist == ""
      item_text_artist = item_text_albumartist
  elsif item_text_albumartist.include? item_text_artist
      item_text_artist = item_text_albumartist
  end

  # Figure out short text
  item_text_short_array = [item_text_description, item_text_synopsis]
  item_text_short = item_text_short_array.sort_by(&:length)[0].to_s
  if item_text_short == ""
    item_text_short = item_text_comment
    if item_text_short == ""
      item_text_short = item_text_artist
    end
  end

  # Eliminate duplicates for long text
  item_text_long_array = [item_text_artist, item_text_description, item_text_synopsis, item_text_comment]
  item_text_long_array = item_text_long_array.select {|e|item_text_long_array.grep(Regexp.new(e)).size == 1}
  # Make sure that no component of long text is nil
  item_text_long_array.each { |snil| snil = snil.to_s }
  # Combine long text and add line breaks
  item_text_long = ""
  item_text_long_array.each { |s| item_text_long += s + "\n"}
  item_text_long = item_text_long.chomp()

  # Figure out author
  item_author = item_text_artist
  if item_author == ""
    item_author = item_text_albumartist
  end

  # Figure out title base
  #if item_title_source == ""
  #  item_title_source = item_filename
  #end

  # Set remaining metadata without logic
  #item_title = item_title_number + item_title_source
  item_url = "#{conf[:url_base]}/#{url_encode(file)}"
  item_size_in_bytes = File.size(file).to_s
  item_duration = item_duration_source
  item_pub_date = begin
    Time.parse(item_pub_date_source).to_s
  rescue Exception => e
    Time.now.to_s
  end
  item_guid = item_url + url_encode(conf[:pub_date])

  item_content = <<-HTML
      <item>
          <title>#{item[:title]}</title>
          <description>#{item_text_long}</description>
          <itunes:subtitle>#{item_text_short}</itunes:subtitle>
          <itunes:summary>#{item_text_short}</itunes:summary>
          <enclosure url="#{item_url}" length="#{item_size_in_bytes}" type="audio/mpeg" />
          <category>Podcasts</category>
          <pubDate>#{item_pub_date}</pubDate>
          <guid>#{item_guid}</guid>
          <itunes:author>#{item_author}</itunes:author>
          <itunes:duration>#{item_duration}</itunes:duration>
      </item>
HTML

  items_content << item_content
end

# Build the whole file
content = <<-HTML
<rss xmlns:itunes="http://www.itunes.com/dtds/podcast-1.0.dtd" version="2.0">
    <channel>
        <title>#{conf[:title]}</title>
        <description>#{conf[:description]}</description>
        <pubDate>#{conf[:pub_date]}</pubDate>
        <itunes:image href="#{conf[:artwork]}"/>
        <itunes:subtitle>#{conf[:description].to_s[0,254]}</itunes:subtitle>
        <itunes:summary>#{conf[:description].to_s[0,3999]}</itunes:summary>
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
