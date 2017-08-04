document.addEventListener("DOMContentLoaded", function() {

  var key = 'elm-game-of-life-board';
  var game, app;

  try {
    game = JSON.parse(localStorage.getItem(key));
    app = Elm.Main.fullscreen(game);
  } catch (e) {
    console.warn(e);
    document.body.innerHTML = '';
    app = Elm.Main.fullscreen(null);
  }

  app.ports.saveBoard.subscribe(function (game) {
    localStorage.setItem(key, JSON.stringify(game));
    alert('board saved!');
  });

});
