module Main exposing (main)

import Array exposing (Array)
import Html exposing (Html)


type alias Digit =
    { top : Bool
    , mid : (Bool, Bool, Bool)
    , btm : (Bool, Bool, Bool)
    }


digits : Array Digit
digits = Array.fromList
    [ Digit  -- 0
          True
          (True, False, True)
          (True, True, True)
    , Digit  -- 1
          False
          (False, False, True)
          (False, False, True)
    , Digit  -- 2
          True
          (False, True, True)
          (True, True, False)
    , Digit  -- 3
          True
          (False, True, True)
          (False, True, True)
    , Digit  -- 4
          False
          (True, True, True)
          (False, False, True)
    , Digit  -- 5
          True
          (True, True, False)
          (False, True, True)
    , Digit  -- 6
          True
          (True, True, False)
          (True, True, True)
    , Digit  -- 7
          True
          (False, False, True)
          (False, False, True)
    , Digit  -- 8
          True
          (True, True, True)
          (True, True, True)
    , Digit  -- 9
          True
          (True, True, True)
          (False, True, True)
    ]


empty : Int -> List String
empty h = List.repeat (2 * h + 1) ""


showDigit : (Int, Int) -> Int -> List String
showDigit (w, h) d =
    let
        topRow f =
            [ String.concat
                [" ", String.repeat w (if f then "_" else " "), " "]
            ]
        row (l, m, r) =
            let
                subrow c =
                    String.concat
                        [ if l then "|" else " "
                        , String.repeat w c
                        , if r then "|" else " "
                        ]
            in List.append
                (List.repeat (h - 1) <| subrow " ")
                [subrow <| if m then "_" else " "]
        toList {top, mid, btm} =
            List.concat
                [ topRow top
                , row mid
                , row btm
                ]
    in
        Maybe.withDefault (empty h)
            <| Maybe.map toList
            <| Array.get d digits


show : (Int, Int) -> List Int -> List String
show (w, h) =
    List.foldr (List.map2 String.append) (empty h)
        << List.map (showDigit (w, h))


main : Html Never
main =
    Html.pre []
        [ Html.text
              <| String.join "\n"
              <| show (2, 2)
              <| [3, 1, 4, 1, 5, 9, 2, 6]
        ]
