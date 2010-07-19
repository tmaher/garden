<?php
// $Id$

require("tursom/secrets.inc");

function dh_db_conn (){
  $db_host = "mysql.tursom.org";
  $db_name = "tursom_db";
  $db_user = "dbuser";

  $db_pw = get_secret("mysql_creds", $db_user);
  
  if($db_pw === FALSE){
    header("HTTP/1.1 500 Internal Server Error");
    printf("can't get auth creds!\n");
    exit(0);
  }

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


?>