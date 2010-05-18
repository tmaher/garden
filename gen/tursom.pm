# $Id$

# Copyright 2010, Tom Maher <tmaher\@watson.org>
#
# Licensed under the Apache License, Version 2.0 (the "License"); you
# may not use this file except in compliance with the License. You may
# obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0

# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
# implied. See the License for the specific language governing permissions
# and limitations und

package tursom;
use strict;

use AnyEvent;
use AnyEvent::HTTP;
use LWP::UserAgent;
use URI::Escape;
use HTML::Entities;
use Socket;
use POSIX qw(:errno_h);
use HTML::PullParser;
use HTML::TreeBuilder;
use HTTP::Cookies::Netscape;

require Exporter;

our @ISA=qw/Exporter/;
our @EXPORT = qw( &yt_get_video_id
               &yt_get_info
               &yt_fetch_vid
               &demonoid_get_id
               &kill_pidfile
               );

sub yt_get_video_id {
  my $full_msg = shift;

  chomp($full_msg);
  my @msgs = split(/\n/m, $full_msg);
  my $vid_id = undef;

  foreach my $msg (@msgs){
    $msg =~ /http:\/\/(www.|)youtube\.com\/.*watch\?([\w\d_\.\%\&\(\)\;=-]+)/;
    my $url_arg_string = defined($2) ? $2 : "";

    my @url_args = split(/\&/, $url_arg_string);
    my %kv = ();
    foreach my $kv (@url_args){
      my ($key, $value) = split(/=/, $kv);
      $kv{$key} = $value;
    }

    if(defined($kv{v}) and $kv{v} =~ /^[\w\d_\.-]+$/){
      $vid_id = $kv{v};
    }
  }
  return $vid_id if(defined($vid_id));

  return undef;

}

sub yt_get_info {
  my $vid = shift;

  my $lwp = LWP::UserAgent->new();
  my $info_url = sprintf("http://www.youtube.com/get_video_info?video_id=%s", $vid);

  my $resp = $lwp->get($info_url);

  if(!$resp->is_success()){
    return "ERROR: " . $resp->status_line();
  }

  my @kvs = split(/\&/, $resp->content());
  my %md = ();

  foreach my $kv (@kvs){
    my ($key, $val) = split(/\=/, $kv);
    $val =~ s/\+/ /g;
    $md{$key} = uri_unescape($val);
  }

  $md{title} =~ s,/,-,g;

  return \%md;
}

sub yt_fetch_vid {
  my $vid = shift;
  my $meta = shift;
  my $out_dir = shift;

  my $fetch_url = sprintf("http://www.youtube.com/get_video?video_id=%s&t=%s",
                          $vid, $meta->{token});

  $out_dir = "./" unless(defined($out_dir));
  $out_dir =~ s,/*$,/,;
  my $out_base = sprintf("%s%s.%s", $out_dir, $meta->{title}, $vid);

  my $lwp =  LWP::UserAgent->new();
  my $resp;

  # fmt CGI param integers corresond to...
  # 22: 720p High Def, h264 in mp4
  # 18: near-SD, h264 in mp4
  # 6: near-SD, FLV
  # <none>: default youtube quality
  my %fmt_type = (22 => "mp4", 18 => "mp4", 6 => "flv", -1 => "flv");
  my $out_file = "";
  foreach my $fmt (22, 18, 6, -1){
    my $fmt_val = ($fmt > 0) ? "&fmt=" . $fmt : "";
    my $file_suffix = $fmt_type{$fmt};

    $out_file = join(".", $out_base, ($fmt > 0) ? $fmt : "dft" , $file_suffix);

    if(-r $out_file){
      return "ERROR:Output file '" . $out_file . "' exists";
    }

    $resp = $lwp->get($fetch_url . $fmt_val,
                      ":content_file" => $out_file);
    last if($resp->is_success());
    sleep(10);
  }

  if(! $resp->is_success()){
    return "ERROR:lwp get on $fetch_url failed: '" . $resp->status_line . "'";
  }

  if(! -r $out_file){
    return "ERROR:fetch succeeded, but file wasn't saved!";
  }

  return "SUCCESS:saved to '" . $out_file . "', " . $resp->status_line;
}

sub demonoid_get_id {
  my $msg = shift;

  # http://www.demonoid.com/files/details/1057380/1719968/
  # http://www.demonoid.com/files/download/HTTP/1057380/1719968/
  my $id_num;
  if($msg =~ /http(s|):\/\/www\.demonoid\.com\/files\/(details|download\/HTTP)\/(\d+)/m){
    $id_num = $3;
  }

  if(!defined($id_num)){
    return "ERROR:can't extract demonoid ID number";
  }

  my $dcookie_file = "/var/tmp/dcookie";
  if(! -r $dcookie_file){
    return "ERROR:can't read dcookie file";
  }

  my $lwp = LWP::UserAgent->new(cookie_jar =>
                                HTTP::Cookies::Netscape->new(file =>
                                                             $dcookie_file));

  my $duri_base = "http://www.demonoid.com/files/";
  my $duri = sprintf("%sdetails/%s/", $duri_base, $id_num);
  my $resp = $lwp->get($duri);
  if(!$resp->is_success()){
    return "ERROR:fetching $duri:" . $resp->status_line();
  }

  my $data = $resp->decoded_content();
  my $p = HTML::TreeBuilder->new();
  $p->p_strict(1);
  $p->parse_content($data);

  my $root = $p->guts();
  my $title;
  foreach my $child ($root->content_list()){
    next unless($child->tag() =~ /^head$/i);
    foreach my $h_child ($child->content_list()){
      next unless($h_child->tag() =~ /^title$/i);
      my @arr = $h_child->content_list();
      next unless(defined($title = $arr[0]));
    }
  }
  return "ERROR:can't determine title of ${id_num}" unless(defined($title));

  $title = decode_entities($title);
  $title =~ s/^\s*(www\.|)demonoid\.com\s*-\s*//i;
  $title =~ s,/,-,g;

  return {title => $title, id => $id_num};
}

sub udp_ack {
  my $jid = shift;
  my $body = shift;
  my $udp_sock = shift;

  $udp_sock = "/var/run/homebot/sock" unless(defined($udp_sock));
  socket(SOCK_CLIENT, PF_UNIX, SOCK_DGRAM, 0) or return;
  my $us = sockaddr_un($udp_sock);

  my $msg = join("||", $jid, $body);

  send(SOCK_CLIENT, $msg, 0, $us);
  close(SOCK_CLIENT);

  return;
}

sub kill_pidfile {
  my $pidfile = shift;
  my $fh = new FileHandle;
  my $pid;

  $fh->open($pidfile) or return -1;
  if(!defined($pid = $fh->getline())){
    $fh->close();
    return -1;
  }

  chomp($pid);
  return -1 unless($pid =~ /^\d+$/);

  kill "TERM", $pid;

  return 1;
}

1;
