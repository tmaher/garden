<?php

if(!isset($_REQUEST['hostname']) or
   !isset($_REQUEST['sent_ts']) or
   !isset($_REQUEST['n']) or
   !isset($_REQUEST['h'])){
  die( "go away");
}
$host = $_REQUEST['hostname'];

$hash_msg = implode("|", array($host,
			       $_REQUEST['sent_ts'],
			       $_REQUEST['n']));

require("tursom/dh_db.inc");

$m = dh_db_conn();

$host_keys = get_delim_file("/home/tmaher/creds/checkin_hosts");
if(!isset($host_keys[$host])){
  header("HTTP/1.1 418 I'm a teapot");
  header("Content-type: text/plain");
  die("go away - unknown host");
}

$hash_real = hash_hmac("sha1", $hash_msg, $host_keys[$host]);
if(strcmp($hash_real, $_REQUEST['h']) != 0){
  header("HTTP/1.1 418 I'm a teapot");
  header("Content-type: text/plain");
  die("go away - sig fail\n");
}


{
$ins_stmt = <<<__EOS
  INSERT INTO checkin_log (sent_ts, host_id, ip)
  VALUES
  ( ? , (select id from checkin_hosts where name = ? ) , ? )
__EOS
  ;
}
header("Content-type: text/plain");

$s = $m->prepare($ins_stmt);
$s->bind_param('sss', date('Y-m-d H:i:s', $_REQUEST['sent_ts']),
	       $host,
	       $_SERVER['REMOTE_ADDR']);
$s->execute();
$rows = $s->affected_rows;

$m->close();

print($rows);
exit(0);

$s = $m->prepare("select id, name from checkin_hosts");
$s->execute();
$s->bind_result($host_id, $name);
$hosts = array();
while($s->fetch()){
  
}
$m->close();

?>