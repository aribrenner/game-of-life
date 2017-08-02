document.addEventListener("DOMContentLoaded", function() {

  var key = 'elm-game-of-life-board';
  var game, app;

  try {
    game = JSON.parse(localStorage.getItem(key));
  } catch (e) {
    console.warn(e);
  }

  game = game || {board: [], iOffset: 0, jOffset: 0}
  app = Elm.Main.fullscreen(game);

  app.ports.saveBoard.subscribe(function (game) {
    localStorage.setItem(key, JSON.stringify(game));
    alert('board saved!');
  });

});
