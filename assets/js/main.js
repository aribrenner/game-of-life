document.addEventListener("DOMContentLoaded", function(event) {

  var key = 'elm-game-of-life-board';
  var board;

  try {
    board = JSON.parse(localStorage.getItem(key));
  } catch (e) {
    console.warn(e);
  }

  var app = Elm.Main.fullscreen({board: board || []});

  app.ports.saveBoard.subscribe(function (str) {
    localStorage.setItem(key, str);
    alert('board saved!');
  });

});
