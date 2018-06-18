git checkout gh-pages &&
elm-make src/main.elm --output dist/elm.js &&
git add dist &&
git commit -m deploy &&
git push origin gh-pages &&
git checkout master
