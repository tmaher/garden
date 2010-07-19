<?php
// $Id$

define('CREDS_DIR', "/home/tmaher/creds/");

function get_delim_file ($file){
  if(! @is_readable($file)){ return FALSE; }

  $h = fopen($file, "r");
  if($h == FALSE){ return FALSE; }

  $kv = array();
  while($line = fgets($h, 256)){
    $line = trim($line);
    list($key, $val) = split(":", $line, 2);
    if(!isset($key) or !isset($val)) { next; }
    $kv[$key] = $val;
  }

  return $kv;
}

function get_secret ( $file, $key_name) {
  $kv = get_delim_file(CREDS_DIR . $file);
  if(!is_array($kv)) { return FALSE; }
  if(!isset($kv[$key_name])) { return FALSE; }
  return $kv[$key_name];
}

?>