module Stylesheet exposing (stylesheet)

{-| stylesheet function for css files
@docs stylesheet from https://gist.github.com/coreytrampe/a120fac4959db7852c0f
-}

import Html exposing (Html, node)
import Html.Attributes exposing (attribute)

{-| stylesheet function for css files
@docs stylesheet from https://gist.github.com/coreytrampe/a120fac4959db7852c0f
-}
stylesheet : String -> Html msg
stylesheet href =
  let
    tag = "link"
    attrs =
        [ attribute "rel"       "stylesheet"
        , attribute "property"  "stylesheet"
        , attribute "href"      href
        ]
    children = []
  in
    node tag attrs children
