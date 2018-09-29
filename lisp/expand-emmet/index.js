const { expand } = require('@emmetio/expand-abbreviation');

const http = require('http');
http.createServer((request, response) => {
  let body = [];
  request.on('data', (chunk) => {
    body.push(chunk);
  }).on('end', () => {
    body = Buffer.concat(body).toString();
    response.end(JSON.stringify(expand(body)));
  });
}).listen(13850);
