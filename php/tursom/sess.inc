<?php
// $Id$

define('SIGN_IN_URI', "https://tursom.org/login.php");
define('SIG_LIFETIME', 86400);  // 24 hours

$user_email = cookie_sig_validate();
if($user_email === FALSE){
  header("Location: " . SIGN_IN_URI);
  exit(1);
}

init_mapping();

session_name("S");
session_set_cookie_params(86400, "/", "tursom.org", TRUE, TRUE);
session_id();

session_start();

function se_init_mapping (){
  global $e_chars, $s_chars;

  // @ ! # $ % & ' * + - / = ? ^ _ ` { | } ~
  $e_to_s_arr = array("@" => ",a",
		    "!" => ",b",
		    "#" => ",h",
		    "$" => ",D",
		    "%" => ",p",
		    "&" => ",m",
		    "'" => ",q",
		    "*" => ",s",
		    "+" => ",P",
		    "/" => ",f",
		    "=" => ",e",
		    "?" => ",q",
		    "^" => ",c",
		    "_" => ",u",
		    "`" => ",b",
		    "{" => ",r",
		    "|" => ",B",
		    "}" => ",R",
		    "~" => ",t"
		    );

  foreach($e_to_s_arr as $key => $val){
    $e_chars[] = $key;
    $s_chars[] = $val;
  }
}

function session_to_email($sess){
  global $e_chars, $s_chars;
  return str_replace($s_chars, $e_chars, $sess);
}

function email_to_session($email){
  global $s_chars, $e_chars;
  return str_replace($e_chars, $s_chars, $email);
}

function cookie_sig_gen($oid_email = NULL){
  if($oid_email === NULL){ return FALSE; }

  $email = filter_var($oid_email, FILTER_SANITIZE_EMAIL);
  if(strcmp($email, $oid_email) !== 0){ return FALSE; }
  if(!filter_var($email, FILTER_VALIDATE_EMAIL)){ return FALSE; }

  $ts = time();
  $sig_data = implode("|", array($email, $ts));
  $secret = get_secret("session", "session");
  if($secret === FALSE){ return FALSE; }

  $sig = hash_hmac("sha256", $sig_data, $secret);

  $raw_string = implode("|", array($sig_data, $sig));
  return urlsafe_b64encode($raw_string);
}

function cookie_sig_validate($cookie_data = NULL) {
  if($cookie_data === NULL){ return FALSE; }

  $sig_string = urlsafe_b64decode($cookie_data);
  if($sig_string === FALSE){ return FALSE; }

  $sig_vals = preg_split("/\|/", $sig_string, 3);
  if(count($sig_vals) != 3) { return FALSE; }

  $email = filter_var($sig_vals[0], FILTER_SANITIZE_EMAIL);
  if(strcmp($email, $sig_vals[0]) !== 0){ return FALSE; }
  if(!filter_var($email, FILTER_VALIDATE_EMAIL)){ return FALSE; }

  $ts = $sig_vals[1];
  $curr_ts = time();
  if(!(is_int($ts) and $ts < $curr_ts and $ts > ($curr_ts - SIG_LIFETIME))){
    return FALSE; }

  $sig_data = implode("|", array($email, $ts));
  $secret = get_secret("session", "session");
  if($secret === FALSE){ return FALSE; }
  $real_sig = hash_hmac("sha256", $sig_data, $secret);

  if(strcmp($real_sig, $sig_vals[2]) !== 0){ return FALSE; }
  
  return $email;
}

function urlsafe_b64encode($string) {
  return str_replace(array('+','/','='), array('.','_','-'),
		     base64_encode($string));
}

function urlsafe_b64decode($string) {
  return base64_decode(str_replace(array('.','_','-'),
				   array('+','/','='),$string),
		       TRUE);
}


?>