#!/opt/node/bin/node

var https = require('https');
var mysql = require('mysql');
var querystring = require('querystring');
var m = require('tweet_markup');

var get_creds = require('get_creds').get_creds;

var db_creds = get_creds('mysql');
var google_creds = get_creds('google_oauth');

client = mysql.createClient({
    user: "dbuser",
    password: db_creds['dbuser'],
    host: "127.0.0.1",
    database: "tursom_db"
});


client.query("select * from oauthcreds where id=1", function(err, ra){
    if(err){
        console.log("DB Query ERROR: " + err.message); return;
    }
    var r = ra[0];
    var ts = Math.floor((new Date().getTime()) / 1000);
    blog_payload = "ArgleBargle at " + new Date(ts * 1000).toString();
    if(ts > r['expires']){
        update_access_token(client, r, post_blog, blog_payload);
    } else {
        post_blog(r['access_token'], blog_payload);
    }
});


var post_blog = function(token, payload){
    console.log("token: " + token);
    var opt = {
        host: 'www.blogger.com',
        path: '/feeds/default/blogs?alt=json',
        headers: {
            'GData-Version' : "2",
            'Authorization' : 'Bearer ' + token
        }
    };
    var req = https.get(opt, function(res){
        var d = ""; res.on("data", function(c){ d+=c; });
        var fin = function(){
            d = JSON.parse(d);
            var t = d['feed']['entry'][0]['id']['$t'];
            var ids = /user-(\d+)\.blog-(\d+)/i(t);
            var usernum = ids[1];
            var blognum = ids[2];
            console.log("user: '"+usernum+"', blog: '"+blognum+"'");
            
        };
        res.on("end", fin);
        res.on("close", fin);
    });
    req.end();

    client.end();
}



var update_access_token = function(client, r, pb_callback, pb_payload){
    var post_data = querystring.stringify({
        'client_id': google_creds["client_id"],
        'client_secret': google_creds["client_secret"],
        'refresh_token': r['refresh_token'],
        'grant_type': 'refresh_token'
    });

    var opt = {
        host: "accounts.google.com",
        path: "/o/oauth2/token",
        method: "POST",
        headers: {  
            'Content-Type': 'application/x-www-form-urlencoded',  
            'Content-Length': post_data.length  
        }
    };
    var req = https.request(opt, function(res){
        res.setEncoding('utf8');  
        var all_chunks = "";
        res.on('data', function (c){ all_chunks += c; });
        var finished = function () {
            r = JSON.parse(all_chunks);
            var ts = Math.floor((new Date().getTime()) / 1000);
            var expires = ts + r['expires_in'] - 30;
            var qs = "UPDATE oauthcreds " +
                "SET access_token = ?, expires = ? WHERE id = 1";
            client.query(qs, [ r['access_token'], expires ], function(e, re){
                if(e){ console.log("DB error: " + e.message); }
                console.log('Inserted: ' + re.affectedRows + ' row.');
                console.log('Id inserted: ' + re.insertId);
            });
            pb_callback(r['access_token'], pb_payload);
        };
        res.on("end", finished);
        res.on("close", finished);
    });        
    req.write(post_data);
    req.end();

}
