module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Messages exposing (..)
import Models exposing (Model)
import Puzzle exposing (Board, LinearBoard, Piece(..), num, isMove, isBlank, isCorrect, clearBoard, toList)
import List exposing (sortWith)
import Maybe.Extra exposing (values)

view : Model -> Html Msg
view ({ board, blankXY } as model) =
    let
        boardList =
            toList board
    in
        div [ class "container" ] <| viewBoard model boardList


viewBoard : Model -> LinearBoard -> List (Html Msg)
viewBoard { board, blankXY } boardList =
    let
        correctStr xy piece =
            if isCorrect xy piece clearBoard then
                "correct"
            else
                "incorrect"

        moveStr xy =
            if isMove blankXY xy then
                "is-move"
            else
                "is-not-move"

        comparePieces x y = case (x, y) of 
            ((_, Blank), (_, Blank)) -> EQ 
            ((_, Num n), (_, Blank)) -> GT
            ((_, Blank), (_, Num n)) -> LT
            ((_, Num n), (_, Num m)) -> compare n m 

    in
        Maybe.Extra.values <| List.map
            (\( xy, piece ) -> 
                let 
                    (x, y) = xy 
                    w = 25
                    h = 25
                in
                    if isBlank xy board then
                        Nothing
                    else
                        Just (a [ 
                            class <| "button piece " ++ moveStr xy, onClick <| Move xy piece,
                            style [("left", toString (w * x) ++ "%"), ("top", toString (h * y) ++ "%")] ]
                            [ span [ class "icon is-medium" ]
                                [ p [ class <| correctStr xy piece ++ " num" ] [ text <| toString <| num piece ]
                                ]
                            ])
            )
            <| List.sortWith comparePieces boardList
