document.addEventListener("DOMContentLoaded", function() {

  var root = document.body;
  var key = 'elm-game-of-life-board';
  var game, app;

  try {
    game = JSON.parse(localStorage.getItem(key));
    app = Elm.Main.embed(root, game);
  } catch (e) {
    console.warn(e);
    root.innerHTML = '';
    app = Elm.Main.embed(root, null);
  }

  app.ports.saveBoard.subscribe(function (game) {
    localStorage.setItem(key, JSON.stringify(game));
    alert('board saved!');
  });

});
