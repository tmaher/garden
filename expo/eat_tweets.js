#!/opt/node/bin/node

var fs = require('fs');
var m  = require("tweet_markup");
var bw = require('blogger_write');
var gt = require('get_tweets');
var sprintf = require('sprintf').sprintf;
var err_log = function(){ process.stderr.write(sprintf.apply(null, arguments)); };

var oldest_ts = new Date().getTime() - (86400 * 1000 * 1);
var screen_name = 'ExpoMuseum';
var now = new Date();
var date_title = sprintf("@ExpoMueum Twitter: %s",
                         now.toLocaleDateString());

gt.get_all_limit(screen_name, oldest_ts, function(tweets){
    if(tweets.length < 1){
        err_log("no tweets found today, so not publishing\n");
        return;
    }
    var body = tweets.reduce(function(a,b){ return a+m.markup(b);}, "");
    bw.post(5, date_title, body);
});

/*
fs.readFile("/tmp/tweets.json", function (err, data){
    if (err) throw err;
    var tweets = JSON.parse(data);
    err_log("%d tweets to publish\n", tweets.length);
    
    //var body = tweets.reduce(function(a,b){ return a+m.markup(b);}, "");
    //bw.post(1, date_title, body);
});
*/
