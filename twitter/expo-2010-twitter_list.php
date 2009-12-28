<?php
// $Id$

$error_msg =<<<ERROR
<html><head><title>OMG TWITTER ERROR</title></head>
<body><b>OMG TWITTER ERROR</b></body></html>
ERROR;

$user = "ExpoMuseum";
$list = "expo-2010";

$lock_handle = "";
$tmp_dir = "/home/tmaher/tmp/tursom.org";
$lockfile = $tmp_dir . "/_lk_" . $user . "_" . $list;

$cache = get_list_status($user, $list);
$tw_stats = $cache[0];
$tw_stats_ts = $cache[1];
$pl = json_decode($tw_stats);

if($pl === NULL or
   isset($pl->error)){
  error_log("json_decode error");
  header("HTTP/1.1 500 Internal Server Error");
  print($error_msg);
  exit(1);
}
function orig_url($tweet){
  $user = $tweet->user->screen_name;
  $id = $tweet->id;

  return "http://twitter.com/" . $user . "/statuses/" . $id;
}

function link_fix($tweet){
  $link_txt =
    '<IMG src="http://www.expomuseum.com/imagebucket/twitterlink1.gif" class="twitter_link" />';

  $pattern =
    '/((https?|s?ftp|ssh)\:\/\/[^"\s\<\>]*[^.,;\'">\:\s\<\>\)\]\!])/i';
  $prefix= "<a href=\"";
  $mid = '" ONCLICK="window.open(this.href, \'newwin\'); return false;">';
  $end = "</a>";
  $new_txt = preg_replace($pattern,
                          $prefix . '$1' . $mid . $link_txt . $end,
                          $tweet->text);

  return $new_txt;
}

function rel_time($tweet){
  $ts = strtotime($tweet->created_at);
  $ts = time() - $ts;
  if($ts<60){
    return"less than a minute ago";
  } else if($ts<120){
    return"about a minute ago";
  } else if($ts<(60*60)){
    return floor($ts / 60) . " minutes ago";
  } else if($ts<(120*60)){
    return"about an hour ago";
  } else if($ts<(24*60*60)){
    return "about " . floor($ts / 3600) . " hours ago";
  } else if($ts<(48*60*60)){
    return "1 day ago";
  } else {
    return floor($ts / 86400) . " days ago";
  }
}

function get_sh_lock(){
  global $tmp_dir, $lockfile, $lock_handle;

  if(!is_dir($tmp_dir)){
    //$retval = mkdir($tmp_dir, 0770);
    //if(!(is_dir($tmp_dir) and is_readable($tmp_dir))){
      error_log("Can't create $tmp_dir");
      exit(1);
      //}
  }
  if(!is_readable($lockfile)){
    $lock_handle = fopen($lockfile, "x");
    if(($lock_handle === FALSE) and (!is_readable($lockfile))){
      error_log("Can't create lockfile $lock_file");
      exit(1);
    }
    fclose($lock_handle);
  }
  $lock_handle = fopen($lockfile, "r+");
  if($lock_handle === FALSE){
    error_log("Can't open for reading lockfile $lock_file");
    exit(1);
  }
  $ret = flock($lock_handle, LOCK_SH);
  if($ret === FALSE){
    error_log("Can't acquire shared lock on $lock_file");
    exit(1);
  }
  return $lock_handle;
}

function get_ex_lock(){
  global $tmp_dir, $lockfile, $lock_handle;

  $ret = flock($lock_handle, LOCK_EX);
  if($ret === FALSE){
    error_log("Can't acquire ex lock on $lock_file");
    exit(1);
  }
  return $lock_handle;
}

function fetch_from_twitter($tw_user, $tw_list){
  $src = sprintf("http://api.twitter.com/1/%s/lists/%s/statuses.json",
		 $tw_user, $tw_list);

  $ch = curl_init($src);
  curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);

  $payload = curl_exec($ch);

  if($payload === false){
    error_log("error fetching $src : " . curl_error($ch));
    header("HTTP/1.1 500 Internal Server Error");
    print($error_msg);
    exit(1);
  }

  $i = curl_getinfo($ch);
  if(!preg_match("/application\/json/i", $i["content_type"])){
    error_log("error fetching $src, got " . $i["content_type"]);
    header("HTTP/1.1 500 Internal Server Error");
    print($error_msg);
    exit(1);
  }

  curl_close($ch);
  return $payload;
}

function get_list_status($tw_user, $tw_list){
  global $tmp_dir, $lockfile, $lock_handle;

  get_sh_lock();
  $cache_file = sprintf("%s/twitter_status_%s_%s.json",
			$tmp_dir, $tw_user, $tw_list);
  $cf_stat = @stat($cache_file);
  if(($cf_stat === FALSE) or ($cf_stat["mtime"] < (time() - 60))){
    get_ex_lock();
    $cf_stat = @stat($cache_file);
    if(($cf_stat === FALSE) or ($cf_stat["mtime"] < (time() - 60))){
      $cfh = @fopen($cache_file, "w+");
      if($cfh === FALSE){
	error_log("Can't create cache file $cache_file");
	exit(1);
      }
      $payload = fetch_from_twitter($tw_user, $tw_list);
      $ret = fwrite($cfh, $payload);
      if($ret === FALSE){
	error_log("Can't write to cache file $cache_file");
	exit(1);
      }
      fclose($cfh);
      $fetch_time = time();
      return array($payload, $fetch_time);
    }
  }
  $cf_stat = stat($cache_file);
  if($cf_stat === FALSE){
    // WTF?
    error_log("stat failed of cache file $cache_file");
    exit(1);
  }

  $fetch_time = $cf_stat["mtime"];
  $cfh = fopen($cache_file, "r");
  if($cfh === FALSE){
    error_log("Can't open-to-read cache file $cache_file");
    exit(1);
  }
  $payload = fread($cfh, filesize($cache_file));
  if($payload === FALSE){
    error_log("Can't read cache file $cache_file");
    exit(1);
  }
  return array($payload, $fetch_time);
}
?>


<html><head><title>
<?php print($user . "/" . $list . " twitter feed"); ?>
</title>
<style type="text/css">
<!--
  a {  font-family: Arial, Helvetica, sans-serif; color: #000099; text-decoration: none}
.style3 {
  font-size: x-small
     }
.style4 {font-family: Arial, Helvetica, sans-serif}

.twitter_link {
border: 0px;
 }

#twitter_div {
  font-family: arial, helvetica, sans-serif;
  width:290px;
  height:1400px;
  font-size:8pt;
  overflow:auto;
  background-image:url('http://www.expomuseum.com/imagebucket/twitback1.gif');
}

#twitter_update_list {
display:block;
  list-style-image: url("http://www.expomuseum.com/imagebucket/embullet2.gif");
  list-style-position: inside;
  margin-top: 5pt;
  padding-left: 5pt;
  padding-right: 5pt;
}
li {
  margin-top: 0px;
  margin-bottom: 10px;
}
body {
  background-image: url('http://www.expomuseum.com/imagebucket/tile01.gif');
  background-color: #FFFFFF;
}
-->
</style>


</head>
<body id="twitter_div">

<!--- <div id="twitter_div"> -->
<?php

  printf("Last Update: %s<hr />\n", date('r', $tw_stats_ts));
printf("<ul id=\"twitter_update_list\">\n");
foreach($pl as $tw){
  $from = sprintf("<a class=\"twitter_from\" href=\"%s\">%s</a>",
                  $tw->user->screen_name, $tw->user->screen_name);
  $body = sprintf("<div class=\"twitter_body\">%s</div>",
                  link_fix($tw));
  $time = sprintf("<a class=\"twitter_time\" href=\"%s\">%s</a>",
                  orig_url($tw), rel_time($tw));
  printf("<li>%s %s %s</li>\n",
         $from, $body, $time);
}

printf("</ul>\n");
?>
</div>

</body></html>