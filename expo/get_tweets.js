#!/opt/node/bin/node

var https = require('https');

var digest_tweets = function (t){
    var creation_out = new Date(Date.parse(t["created_at"])).toString();
    var out = creation_out + "\n" + t["text"] + "\n\n";

    return out;
};

function process_tweets(all_tweets){
    for(t in all_tweets){
        console.log(digest_tweets(all_tweets[t]));
    }
}

function get_all_since_limit(screen_name, limit) {
    var all_tweets = [];

    var maxfetch = 6;
    var currfetch = 0;
    var options = { host: 'api.twitter.com' };
    var base_path = "/1/statuses/user_timeline.json" +
        "?screen_name=" + screen_name +
        "&trim_user=true" +
        "&include_rts=false";
    var currpage = 1;

    function get_all_helper(){
        options["path"] = base_path + "&page=" + currpage++;
        var req = https.get(options, function(res) {
            var all_chunks = "";
            res.on("data", function(c){ all_chunks += c;  });
            
            var finished = function(){
                currfetch += 1;
                var these_tweets = JSON.parse(all_chunks);
                if(these_tweets.length < 1){ return; }

                var oldest_tweet = these_tweets.pop();
                var oldest_ts = Date.parse(oldest_tweet["created_at"]);
                these_tweets.push(oldest_tweet);
                if((oldest_ts > limit) &&
                   (these_tweets.length > 0) &&
                   (currfetch < maxfetch)){
                    all_tweets = all_tweets.concat(these_tweets);
                    get_all_helper();
                } else {
                    console.log("completed in %d requests", currfetch);
                    console.log("%d (oldest)\n%d (limit)", oldest_ts, limit);
                    
                    var is_oldest;
                    do {
                        is_oldest = these_tweets.pop();
                        o_ts = Date.parse(is_oldest["created_at"]);
                    } while(o_ts <= limit && these_tweets.length >= 1);

                    if(o_ts > limit){ these_tweets.push(is_oldest); }

                    all_tweets = all_tweets.concat(these_tweets);
                    process_tweets(all_tweets);
                }
            };
    
            res.on("end", finished);
            res.on("close", finished);
        });
        
        req.end();
    }

    get_all_helper();
}

var oldest_ts_for_me = new Date().getTime() - (86400 * 1000 * 7);
get_all_since_limit("ExpoMuseum", oldest_ts_for_me);
