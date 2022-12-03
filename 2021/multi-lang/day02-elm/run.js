var Elm = require('./main').Elm;
var main = Elm.Main.init();
var fs = require('fs');

var input = fs.readFileSync('inputs/02.input', 'utf8');

main.ports.get.send(input);
main.ports.put.subscribe(console.log);
