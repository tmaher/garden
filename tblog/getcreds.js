var fs = require('fs');

var creds_file = process.env.HOME + "/private/tblog.json";

var creds = JSON.parse(fs.readFileSync(creds_file));
try { creds.google.client_id.length; }
catch(e) { throw "google client_id not found in " + creds_file; }

for(var provider in creds){
    exports[provider] = creds[provider];
}
