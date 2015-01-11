#!/usr/bin/env ruby

require 'excon'
require 'csv'

rowlimit = 50
searchurl = "https://archive.org/advancedsearch.php?q=collection%3A%28GratefulDead+AND+etree+AND+stream_only%29++AND+NOT+post_text%3A%28released+OR+not+streamable+OR+commercially+OR+cannot%29&fl%5B%5D=date&fl%5B%5D=identifier&fl%5B%5D=publicdate&fl%5B%5D=title&sort%5B%5D=date+asc&sort%5B%5D=avg_rating+desc&sort%5B%5D=downloads+desc&rows=#{rowlimit}&page=1&callback=callback&save=yes&output=csv#raw"

idents = {}
resp = Excon.get searchurl
CSV.parse(resp.body) do |show|
  next if idents.key? show[0]
  idents[show[0]] = {:date => show[0], :ident => show[1],
    :pubdate => show[2], :title => show[3] }
end

idents.each do |k,v|
  puts "#{v[:title]}"
end
