require('./style.css');
const { Elm } = require('./Main.elm');

var app = Elm.Main.init({
  node: document.getElementById('elm')
});

app.ports.saveUser.subscribe(function(data) {
  localStorage.setItem("User", JSON.stringify(data));
});

app.ports.getUser.subscribe(function() {
  const data = JSON.parse(localStorage.getItem("User"));
  app.ports.restoreUser.send(data);
});
