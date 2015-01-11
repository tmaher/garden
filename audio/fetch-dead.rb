#!/usr/bin/env ruby

require 'excon'
require 'csv'

rowlimit = 5000
searchurl = "https://archive.org/advancedsearch.php?q=collection%3A%28GratefulDead+AND+etree+AND+stream_only%29++AND+NOT+post_text%3A%28released+OR+not+streamable+OR+commercially+OR+cannot%29&fl%5B%5D=date&fl%5B%5D=identifier&fl%5B%5D=publicdate&fl%5B%5D=title&fl%5B%5D=year&sort%5B%5D=date+asc&sort%5B%5D=avg_rating+desc&sort%5B%5D=downloads+desc&rows=#{rowlimit}&page=1&callback=callback&save=yes&output=csv#raw"

all_shows = {}
resp = Excon.get searchurl
CSV.parse(resp.body) do |rec|
  next if all_shows.key? rec[0]
  all_shows[rec[0]] = {:date => rec[0], :id => rec[1], :pubdate => rec[2],
    :title => rec[3].sub(/\AGrateful Dead Live at /, "").sub(/ on 19\d\d-\d\d-\d\d\z/,""),
    :year => rec[4]
    }
end

all_shows.delete("date")

all_shows.each do |date,show|
  m3u_url = "https://archive.org/download/#{show[:id]}/#{show[:id]}_vbr.m3u"
  filename = "#{show[:year]}/#{date.sub(/T00:00:00Z\z/,'')} #{show[:title]}.m3u"
  resp = Excon.get(m3u_url, {
    middlewares: Excon.defaults[:middlewares] +
      [Excon::Middleware::RedirectFollower]
    }
  )
  Dir.exist? show[:year] or Dir.mkdir show[:year]
  File.open(filename, 'w') {|f| f.write(resp.body) }
  puts "#{filename}"
end
