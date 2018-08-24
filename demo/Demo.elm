module Demo exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import MultiInput
import Regex exposing (Regex)


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Example =
    { items : List String
    , state : MultiInput.State
    }


type alias Model =
    { tags : Example
    , emails : Example
    , customStyleTags : Example
    }


type ExampleMsg
    = MultiInputMsg MultiInput.Msg
    | Reset


type Msg
    = TagsMsg ExampleMsg
    | EmailsMsg ExampleMsg
    | CustomStyleTagsMsg ExampleMsg


init : ( Model, Cmd Msg )
init =
    ( { tags =
            { items = [ "dogs", "cats", "TIGER!" ]
            , state = MultiInput.init tagsId
            }
      , emails =
            { items = [ "valid@email.com", "another@email.com", "invalid" ]
            , state = MultiInput.init emailsId
            }
      , customStyleTags =
            { items = [ "#5A6378", "#60B5CC", "not-a-color" ]
            , state = MultiInput.init customStyleTagsId
            }
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


updateExample : ExampleMsg -> MultiInput.UpdateConfig -> Example -> (MultiInput.Msg -> Msg) -> String -> ( Example, Cmd Msg )
updateExample exampleMsg updateConf example toOuterMsg id =
    case exampleMsg of
        MultiInputMsg msg ->
            let
                ( nextState, nextItems, nextCmd ) =
                    MultiInput.update updateConf msg example.state example.items
            in
            ( { example | items = nextItems, state = nextState }, Cmd.map toOuterMsg nextCmd )

        Reset ->
            ( { example | items = [], state = MultiInput.init id }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TagsMsg exampleMsg ->
            let
                ( example, cmd ) =
                    updateExample exampleMsg { separators = defaultSeparators } model.tags (TagsMsg << MultiInputMsg) tagsId
            in
            ( { model | tags = example }, cmd )

        EmailsMsg exampleMsg ->
            let
                ( example, cmd ) =
                    updateExample exampleMsg { separators = defaultSeparators } model.emails (EmailsMsg << MultiInputMsg) emailsId
            in
            ( { model | emails = example }, cmd )

        CustomStyleTagsMsg exampleMsg ->
            let
                ( example, cmd ) =
                    updateExample exampleMsg { separators = defaultSeparators } model.customStyleTags (CustomStyleTagsMsg << MultiInputMsg) customStyleTagsId
            in
            ( { model | customStyleTags = example }, cmd )


view : Model -> Html Msg
view model =
    Html.div [ Attr.class "example-list" ]
        [ viewTagsExample model
        , viewEmailsExample model
        , viewCustomStyleTagsExample model
        ]


viewTagsExample : Model -> Html Msg
viewTagsExample model =
    Html.div [ Attr.class "example tags" ]
        [ Html.h2 [] [ Html.text "Tags" ]
        , MultiInput.view
            { placeholder = "Write here"
            , toOuterMsg = MultiInputMsg >> TagsMsg
            , isValid = matches "^[a-z0-9]+(?:-[a-z0-9]+)*$"
            }
            []
            model.tags.items
            model.tags.state
        , Html.button [ Attr.class "reset", Ev.onClick <| TagsMsg Reset ] [ Html.text "Reset" ]
        ]


viewEmailsExample : Model -> Html Msg
viewEmailsExample model =
    let
        isValid =
            matches ".+@.+\\..+"

        validEmails =
            List.filter isValid model.emails.items

        nValidEmails =
            List.length validEmails

        maxValidEmails =
            10
    in
    Html.div [ Attr.class "example emails" ]
        [ Html.h2 [] [ Html.text "Emails" ]
        , Html.p [ Attr.class "explanation" ] [ Html.text "You can also add some extra functionality to the default component. Here's an idea:" ]
        , MultiInput.view
            { placeholder = "Write an email here", toOuterMsg = MultiInputMsg >> EmailsMsg, isValid = isValid }
            []
            model.emails.items
            model.emails.state
        , Html.p [ Attr.class "counter" ] [ Html.text <| "You've introduced (" ++ String.fromInt nValidEmails ++ "/" ++ String.fromInt maxValidEmails ++ ") valid emails" ]
        , Html.button [ Attr.class "reset", Ev.onClick <| EmailsMsg Reset ] [ Html.text "Reset" ]
        ]


viewCustomStyleTagsExample : Model -> Html Msg
viewCustomStyleTagsExample model =
    Html.div [ Attr.class "example custom-style-tags" ]
        [ Html.h2 [] [ Html.text "Custom-Style Tags" ]
        , MultiInput.view
            { placeholder = "Write here", toOuterMsg = MultiInputMsg >> CustomStyleTagsMsg, isValid = matches "^#[a-fA-F0-9]{6}$" }
            []
            model.customStyleTags.items
            model.customStyleTags.state
        , Html.button [ Attr.class "reset", Ev.onClick <| CustomStyleTagsMsg Reset ] [ Html.text "Reset" ]
        ]


tagsId : String
tagsId =
    "tags-input"


emailsId : String
emailsId =
    "emails-input"


customStyleTagsId : String
customStyleTagsId =
    "custom-style-tags-input"


defaultSeparators : List String
defaultSeparators =
    [ "\n", "\t", " ", "," ]


matches : String -> String -> Bool
matches regex =
    let
        validRegex =
            Regex.fromString regex
                |> Maybe.withDefault Regex.never
    in
    Regex.findAtMost 1 validRegex >> List.isEmpty >> not
