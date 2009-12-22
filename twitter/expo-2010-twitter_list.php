<?php
// $Id$
$user = "ExpoMuseum";
$list = "expo-2010";
$fmt = "json";
$src = sprintf("http://api.twitter.com/1/%s/lists/%s/statuses.%s",
               $user, $list, $fmt);
$ch = curl_init($src);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);

$payload = curl_exec($ch);

if($payload === false){
  error_log("error fetching $src : " . curl_error($ch));

  header("HTTP/1.1 500 Internal Server Error");
  exit(1);
}

curl_close($ch);

$pl = json_decode($payload);
if($pl === NULL){
  error_log("json_decode error");
  header("HTTP/1.1 500 Internal Server Error");
  exit(1);
}  

?>
<html><head><title>list test</title>
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
  background-image: url(imagebucket/tile01.gif);
  background-color: #FFFFFF;
}
-->
</style>


</head>
<body>
<?php


printf("<div id=\"twitter_div\">\n");
printf("%s<hr />\n", date('r'));
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

printf("</ul></div>\n");

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

?>
</body></html>