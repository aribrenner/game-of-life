document.addEventListener("DOMContentLoaded", function() {

  var key = 'elm-game-of-life-board';
  var board, app;

  try {
    board = JSON.parse(localStorage.getItem(key));
  } catch (e) {
    console.warn(e);
  }

  app = Elm.Main.fullscreen({board: board || []});

  app.ports.saveBoard.subscribe(function (str) {
    localStorage.setItem(key, str);
    alert('board saved!');
  });

});
