<?php

define('CREDS_DIR', "/home/tmaher/creds/");

function dh_db_conn (){
  $db_host = "mysql.tursom.org";
  $db_name = "tursom_db";
  $db_user = "dbuser";


  $db_cred_path = "/home/tmaher/creds/mysql_creds";
  $auth = get_delim_file($db_cred_path);
  if($auth == FALSE){
    header("HTTP/1.1 500 Internal Server Error");
    printf("can't get auth creds!\n");
    exit(0);
  }

  $db_pw = $auth[$db_user];

  if(!isset($db_user) or !isset($db_pw)){
    header("HTTP/1.1 500 Internal Server Error");
    printf("Oh noes!  We couldn't connect to db\n");
    exit(0);
  }

  $m = new mysqli($db_host, $db_user, $db_pw, $db_name);

  if (mysqli_connect_error()) {
    header("HTTP/1.1 500 Internal Server Error");
    die('Connect Error (' . mysqli_connect_errno() . ') '
	. mysqli_connect_error() );
  }

  return $m;
  
}

function get_delim_file ($file){
  $h = fopen($file, "r");
  if($h == FALSE){
    return FALSE;
  }

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