# elm-multi-input [![Build Status](https://travis-ci.org/elm-lang/core.svg?branch=master)](https://travis-ci.org/larribas/elm-multi-input)

A multi-value input for Elm


## [Try it out](https://larribas.github.io/elm-multi-input/)

![alt text](https://github.com/larribas/elm-multi-input/raw/master/demo/preview.gif "Animated preview for the component")

## How to use it

Install the package:

```
elm-package install larribas/elm-multi-input
```

Here's an example of a minimal integration scenario for inputting multiple emails. I also recommend that you download the default sylesheet at `styles/multi-input.css`.


```elm
module Main exposing (main)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import MultiInput


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { emails : List String
    , inputState : MultiInput.State
    }


type Msg
    = MultiInputMsg MultiInput.Msg


init : ( Model, Cmd Msg )
init =
    ( { emails = []
      , inputState = MultiInput.init "multi-email-input"
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MultiInputMsg subMsg ->
            let
                ( nextState, nextEmails, nextCmd ) =
                    MultiInput.update updateConfig subMsg model.inputState model.emails
            in
            ( { model | emails = nextEmails, inputState = nextState }, Cmd.map MultiInputMsg nextCmd )


updateConfig : MultiInput.UpdateConfig
updateConfig =
    { separators = [ "\n", "\t", " ", "," ]
    }


view : Model -> Html Msg
view model =
    MultiInput.view
        viewConfig
        []
        model.emails
        model.inputState


viewConfig : MultiInput.ViewConfig
viewConfig
        { placeholder = "Write email here"
        , toOuterMsg = MultiInputMsg
        , isValid = Regex.find (Regex.AtMost 1) (Regex.regex ".+@.+\\..+") >> List.isEmpty >> not
        }

```


## Contribute

Any contributions or feedback are welcome!
