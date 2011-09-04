#!/usr/local/bin/node

var https = require('https');
var sys = require('sys');
var fs = require('fs');
var oauth = require('oauth');

var opt = {
    // https://api.twitter.com/1/statuses/user_timeline.json?user_id=34335782
    host: "api.twitter.com",
    path: '/1/statuses/user_timeline.json?user_id=34335782&include_rts=false',
};

function do_req(p){
    p.forEach(function (t){
        console.log("«" + t.created_at + "; " + t.id + "»\n" + t.text + "\n\n");
    });
    process.stdout.write("\n");
}

var req = https.get(opt, function(res){
    //console.log("statusCode: ", res.statusCode);
    // console.log("headers: ", res.headers);

    var payload = "";
    res.on('data', function(d) { payload += d; });
    res.on('end', function(){ do_req(JSON.parse(payload)); });
});
req.end();

