#!/usr/local/bin/node

var https = require('https');

var screen_name="ExpoMuseum";

var options = {
    host: 'api.twitter.com',
    path: '/1/statuses/user_timeline.json?screen_name=' + screen_name
};

options = {
    host: "www.tursom.org",
    path: "/sample.js"
};

var tweets;
var req = https.get(options, function(res) {
    var all_data;
    res.on("data", function(d){ all_data += d;  });

    var finished = function(){
        JSON.parse(all_data);
        process.stdout.write(all_data);
    };
    
    res.on("end", finished);
    res.on("close", finished);
});

req.end();
