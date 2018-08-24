module MultiInput exposing
    ( ViewConfig, UpdateConfig
    , Msg(..), State, init, update, view
    )

{-| A component to input multiple items and display/manage them comfortably.

You can completely customize the type of items it accepts or the way different items are split up. Examples are an input for multiple item (as in an item client's FROM field), or a tag input (as in Github's repository topics). It allows pasting in bulk, removing existing items and ammending the last typed item.

For a better feel of what you can do with this component, visit the [demo here](https://larribas.github.io/elm-multi-input/)


# Custom Configuration

@docs ViewConfig, UpdateConfig


# Main workflow

@docs Msg, State, init, update, view

-}

import Browser.Dom as Dom
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Json.Decode as Json
import Regex exposing (Regex)
import Set
import String
import Task


{-| Internal messages to manage the component's state.
-}
type Msg
    = FocusElement
    | TextareaFocused
    | TextareaBlurred String
    | KeyDown Int
    | RemoveItem String
    | InputChanged String


{-| Component's internal state.
-}
type alias State =
    { nextItem : String
    , id : String
    }


{-| Specific settings for the component's update function.

You can specify a list of strings that act as separators for the different items.

    { separators = [ "\n", "\t", ",", " " ] }

-}
type alias UpdateConfig =
    { separators : List String
    }


{-| Specific settings for the component's view function.

`isValid` determines whether a typed item is correct (and give visual feedback to the user)
`toOuterMsg` turns the internal messages for the component into messages from the outer page/component

    { placeholder = "Write your email here"
    , isValid = \x -> String.contains "@"
    , toOuterMsg = MultiInputMsg
    }

-}
type alias ViewConfig msg =
    { placeholder : String
    , isValid : String -> Bool
    , toOuterMsg : Msg -> msg
    }


{-| Initialize the component's state.

It needs a unique ID supplied by the user, in case there are several inputs like this on the same document. By default, we begin with an empty textarea.

-}
init : String -> State
init id =
    { nextItem = ""
    , id = id
    }


{-| Updates the component's state and a supplied list of items.

Given a particular change on the input (e.g. a series of items have been pasted, the component has lost focus, a special key has been pressed...) it will update the list of distinct items and the current state of the component.

-}
update : UpdateConfig -> Msg -> State -> List String -> ( State, List String, Cmd Msg )
update conf msg state items =
    let
        nextItemIsEmpty =
            state.nextItem == ""

        noChanges =
            ( state, items, Cmd.none )

        refocus =
            Task.attempt (\_ -> TextareaFocused) (Dom.focus state.id)
    in
    case msg of
        FocusElement ->
            ( state, items, refocus )

        KeyDown key ->
            case toSpecialKey key of
                Tab ->
                    if nextItemIsEmpty then
                        noChanges

                    else
                        ( { state | nextItem = "" }, dropDuplicates (items ++ [ state.nextItem ]), refocus )

                Backspace ->
                    if nextItemIsEmpty then
                        case items |> List.reverse |> List.head of
                            Just previousEmail ->
                                ( { state | nextItem = previousEmail }, items |> List.take (List.length items - 1), refocus )

                            Nothing ->
                                noChanges

                    else
                        noChanges

                Other ->
                    noChanges

        InputChanged text ->
            let
                separatorRegex =
                    conf.separators
                        |> String.join "|"
                        |> Regex.fromString
                        |> Maybe.withDefault Regex.never

                allItems =
                    text |> Regex.split separatorRegex

                ( newItems, nextItem ) =
                    ( allItems |> List.take (List.length allItems - 1) |> List.filter (not << String.isEmpty)
                    , allItems |> List.drop (List.length allItems - 1) |> List.head |> Maybe.withDefault ""
                    )
            in
            ( { state | nextItem = nextItem }, dropDuplicates (items ++ newItems), refocus )

        RemoveItem item ->
            ( state, List.filter ((/=) item) items, Cmd.none )

        TextareaFocused ->
            noChanges

        TextareaBlurred item ->
            if item /= "" then
                ( { state | nextItem = "" }, dropDuplicates (items ++ [ item ]), Cmd.none )

            else
                noChanges


{-| Renders the component visually.

       MultiInput.view MultiInputMsg [] "Write a placeholder here" model.inputItems model.inputItemsState

See README for actual examples.

-}
view : ViewConfig msg -> List (Html.Attribute msg) -> List String -> State -> Html msg
view conf customAttributes items state =
    Html.div [ Attr.class "multi-input-container" ]
        [ Html.ul [ Attr.class "multi-input-list", Ev.onClick (conf.toOuterMsg FocusElement) ]
            ((items |> List.map (viewItem conf state))
                ++ [ Html.li [ Attr.class "multi-input-list-item" ] [ viewExpandingTextArea conf customAttributes state ]
                   ]
            )
        ]


{-| Renders an expanding text area (that is, a textarea element inspired by [this article](https://alistapart.com/article/expanding-text-areas-made-elegant)) used to hold the next item
-}
viewExpandingTextArea : ViewConfig msg -> List (Html.Attribute msg) -> State -> Html msg
viewExpandingTextArea conf customAttributes state =
    Html.div [ Attr.class "multi-input-expanding-area" ]
        [ Html.pre []
            [ Html.span []
                [ Html.text <|
                    if state.nextItem /= "" then
                        state.nextItem

                    else
                        conf.placeholder
                ]
            , Html.br [] []
            ]
        , Html.textarea
            ([ Attr.value state.nextItem
             , Attr.placeholder conf.placeholder
             , Attr.rows 1
             , Attr.id state.id
             , Ev.onInput (conf.toOuterMsg << InputChanged)
             , Ev.onBlur (conf.toOuterMsg <| TextareaBlurred state.nextItem)
             , onKeyDown (conf.toOuterMsg << KeyDown)
             ]
                ++ customAttributes
            )
            []
        ]


{-| Describes a separate item (usually visualized as a capsule)
-}
viewItem : ViewConfig msg -> State -> String -> Html msg
viewItem conf state item =
    Html.li
        [ Attr.classList
            [ ( "multi-input-token", True )
            , ( "multi-input-token-invalid", not (conf.isValid item) )
            ]
        ]
        [ Html.p []
            [ Html.text item
            , Html.i [ Attr.class "multi-input-delete-button", Ev.onClick (conf.toOuterMsg <| RemoveItem item) ] [ Html.text "" ]
            ]
        ]


type SpecialKey
    = Tab
    | Backspace
    | Other


toSpecialKey : Int -> SpecialKey
toSpecialKey keyCode =
    case keyCode of
        8 ->
            Backspace

        9 ->
            Tab

        _ ->
            Other


onKeyDown : (Int -> msg) -> Html.Attribute msg
onKeyDown toMsg =
    Ev.on "keydown" <| Json.map toMsg Ev.keyCode


{-| Drop the duplicates in a list. It preserves the original order, keeping only the first
-}
dropDuplicates : List comparable -> List comparable
dropDuplicates list =
    let
        step next ( set, acc ) =
            if Set.member next set then
                ( set, acc )

            else
                ( Set.insert next set, next :: acc )
    in
    List.foldl step ( Set.empty, [] ) list |> Tuple.second |> List.reverse
