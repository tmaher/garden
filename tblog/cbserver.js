#!/usr/local/bin/node


var http = require('http');
var https = require('https');
var url = require('url');
var creds = require('./getcreds.js');

var obj2form = function(o){
    var a = new Array;
    for(var p in o){
        a.push(encodeURIComponent(p) + "=" + encodeURIComponent(o[p]));
    }
    return a.join("&");
};

var oauth_scopes = ["http://www.blogger.com/feeds/",
                    "https://www.google.com/m8/feeds/"
                   ];

var oauth_dance_start_url = "https://accounts.google.com/o/oauth2/auth?" +
    obj2form({"client_id": creds.google.client_id,
              "redirect_uri": creds.google.redirect_uri,
              "response_type": "code",
              "scope": oauth_scopes.join(" ")});

console.log(oauth_dance_start_url);

var handle_token_response = function(t_resp){
    var t = JSON.parse(t_resp);
    console.log("refresh\n" + t.refresh_token + "\naccess\n" + t.access_token);
};

var get_refresh_token = function(code){
    var opt = { host: "accounts.google.com",
                path: "/o/oauth2/token",
                headers: {"Content-type": "application/x-www-form-urlencoded"},
                method: "POST" };
    var req = https.request(opt, function(res){
        console.log("statusCode: ", res.statusCode);
    });
    req.on("response", function(res){
        var p = "";
        res.on('data', function(d) { p += d; });
        res.on('end', function(){ handle_token_response(p); });
        res.on('close', function(){ handle_token_response(p); });
    });
    req.on('error', function(e){ console.log("request fail"); throw e; });

    req.end(obj2form({"code": code,
                      "client_id": creds.google.client_id,
                      "client_secret": creds.google.client_secret,
                      "redirect_uri": creds.google.redirect_uri,
                      "grant_type": "authorization_code"}));
};

var server = http.createServer(function (request, response) {
    var payload = "";
    request.on('data', function(chunk) { payload += chunk; });
    request.on('end', function() {
        if(request.url.match(/^\/favicon\.ico$/i)){ return; }
        console.log("« INCOMING: " + 
                    request.socket.remoteAddress + ":" + 
                    request.socket.remotePort + " »");
        console.log(request.method + " " + request.url +
                    " HTTP/" + request.httpVersion + "\n");
        var pu = url.parse(request.url, true);
        if(pu.pathname == "/oauth2callback" && pu.query.code.length){
            get_refresh_token(pu.query.code);
        }
    });
    response.writeHead(200, {"Content-Type": "text/html"});
    response.write("<h1>stuff</h1>");
    response.write("<a href=\"" + oauth_dance_start_url + "\">" +
                   oauth_dance_start_url + "</a>");
    response.end();
    
});

server.listen(12345);
console.log("Server running...");

