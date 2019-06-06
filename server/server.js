var jsonServer = require('json-server')
var fs = require('fs')

var server = jsonServer.create()

server.all("*", function(req, res, next) {
  res.header("Access-Control-Allow-Origin", "*");
  return next();
});

server.get('/posts', function (req, res) {
  var db = JSON.parse(fs.readFileSync('db.json'))
  res.jsonp(db.posts)
})

server.get('/comments', function (req, res) {
  var db = JSON.parse(fs.readFileSync('db.json'))
  res.jsonp(db.comments)
})

var middlewares = jsonServer.defaults()
server.use(middlewares)

var router = jsonServer.router('db.json')
server.use(router)

server.listen(3000, function () {
  console.log('Serving on -----> http://localhost:3000 <-----')
})
