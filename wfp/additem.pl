#!/usr/local/bin/perl -w
# $Id$

use strict;
use POSIX;

my $TMPLMP3 =<<EOL
 <item>
  <title>The World's Fair Podcast, Episode __ENUM__: __TITLE__</title>
  <description>__EDESC__</description>
  <itunes:summary>__EDESC__</itunes:summary>
  <guid>http://www.worldsfairpodcast.com/itunes/WFP-__ENUM3__.mp3</guid>
  <enclosure url="http://www.worldsfairpodcast.com/itunes/WFP-__ENUM3__.mp3" length="__MP3LENGTH__" type="audio/mpeg" />
  <pubDate>__DATE__</pubDate>
  <itunes:duration>__LENGTH__</itunes:duration>
 </item>
EOL
;

my $TMPLM4A = <<EOL
<item>
  <title>The World's Fair Podcast, Episode __ENUM__: __TITLE__</title>
  <description>__EDESC__</description>
  <itunes:summary>__EDESC__</itunes:summary>
  <guid>http://www.worldsfairpodcast.com/itunes/WFP-__ENUM3__.m4a</guid>
  <enclosure url="http://www.worldsfairpodcast.com/itunes/WFP-__ENUM3__.m4a" length="__M4ALENGTH__" type="audio/x-m4a" />
  <pubDate>__DATE__</pubDate>
  <itunes:duration>__LENGTH__</itunes:duration>
 </item>

EOL
;

my $USAGE = <<EOL
$0 itunes -n EPNUM -t TITLE < DESCRIPTION
$0 mp3 -n EPNUM -t TITLE < DESCRIPTION

EOL
;

my ($mode, $enum, $title) = (undef, undef, undef);
while(defined($ARGV[0]) and $_ = shift(@ARGV)){
  (/^-h$/ or /^--help$/) and die $USAGE;
  (/^-n$/) and do { defined($ARGV[0]) or die $USAGE;
		    $enum = shift(@ARGV); next; };
  (/^-t$/) and do { defined($ARGV[0]) or die $USAGE;
		    $title = shift(@ARGV); next; };
  (/^itunes$/) and do { defined($mode) and die $USAGE;
			$mode = "itunes"; next; };
  (/^mp3$/) and do {defined($mode) and die $USAGE;
		    $mode = "mp3"; next; };
  die $USAGE;
}

(defined($mode) and defined($enum) and defined($title) ) or die $USAGE;
($enum =~ /^\d+$/) or die $USAGE;
my $enum3 = sprintf("%03d", $enum);

my $wfpdir = "/home/ursoc/worldsfairpodcast.com/";
my $idir = $wfpdir . "itunes/";
my $mp3dir =  $wfpdir . "mp3/";

my $m4afile = sprintf("%s/WFP-%s.m4a", $idir, $enum3);
my $mp3file = sprintf("%s/WFP-%s.mp3", $mp3dir, $enum3);
(-r $m4afile) or die sprintf("Sorry, no %s\n", $m4afile);
(-r $mp3file) or die sprintf("Sorry, no %s\n", $mp3file);

my $m4alength = (stat($m4afile))[7];
my $mp3length = (stat($mp3file))[7];

my $desc = "";
my $line = "";
while(defined($line = <>)){ $desc .= $line; }
$desc =~ s/^\s*//gm;
$desc =~ s/\s*$//gm;
chomp($desc);

my $fflength = `ffmpeg -i $m4afile 2>&1 | grep "Duration: "`;
$fflength =~ /Duration: (\d\d:\d\d:\d\d)\./;
my $length = $1;
$length =~ s/^00://;
$length =~ s/^0+//;

my $now = strftime("%a, %d %b %Y %H:%M:%S %z", localtime());
my $build_time = strftime("%a, %d %b %Y %H:%M:%S %z", localtime(time() + 60));

$TMPLMP3 =~ s/__ENUM__/$enum/g;
$TMPLMP3 =~ s/__EDESC__/$desc/g;
$TMPLMP3 =~ s/__ENUM3__/$enum3/g;
$TMPLMP3 =~ s/__MP3LENGTH__/$mp3length/g;
$TMPLMP3 =~ s/__DATE__/$now/g;
$TMPLMP3 =~ s/__LENGTH__/$length/g;
$TMPLMP3 =~ s/__TITLE__/$title/g;

$TMPLM4A =~ s/__ENUM__/$enum/g;
$TMPLM4A =~ s/__EDESC__/$desc/g;
$TMPLM4A =~ s/__ENUM3__/$enum3/g;
$TMPLM4A =~ s/__M4ALENGTH__/$m4alength/g;
$TMPLM4A =~ s/__DATE__/$now/g;
$TMPLM4A =~ s/__LENGTH__/$length/g;
$TMPLM4A =~ s/__TITLE__/$title/g;

print "\n\n";
printf(" <pubDate>%s</pubDate>\n", $build_time);
if($mode eq "mp3"){
  printf(" <lastBuildDate>%s</lastBuildDate>\n\n", $build_time);
  print $TMPLMP3;
} elsif($mode eq "itunes"){
  print "\n\n" . $TMPLM4A;
} else { die $USAGE; }
print "\n\n";

exit(0);
