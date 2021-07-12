module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

main =
  Browser.sandbox { init = init, update = update, view = view }

--MODEL

type Operation = Add | Minus | Multiply


type Model  
    = Oneoperand Int
    | TwoOperand Int Operation Int

init : Model
init = 
    Oneoperand 0

--UPDATE

type Msg
    = Number Int
    | Op Operation
    | Equal
    | Clear
    

update : Msg -> Model -> Model
update msg model = 
    case msg of
        Number number -> 
          updateDisplay number model
        Clear ->
            init
        Op Add ->
            operator Add model    
        Op Minus ->
            operator Minus model
        Op Multiply ->
            operator Multiply model
        Equal ->
            equal model
            
        
        

updateDisplay : Int -> Model -> Model
updateDisplay number model =
    case model of
       Oneoperand x ->
         Oneoperand ((x*10) + number)
       TwoOperand x operation y ->
         TwoOperand x operation ((y*10) + number) 

--muda de estado
operator : Operation -> Model -> Model
operator newoperation model =
    case model of
       Oneoperand x -> 
         TwoOperand x newoperation 0
       TwoOperand x op y ->
         TwoOperand (applyOperation op x y) newoperation 0   --fica no mesmo estado até apertar igual?

equal : Model -> Model
equal model =
    case model of
       Oneoperand x ->
         Oneoperand x
       TwoOperand x op y ->
         Oneoperand (applyOperation op x y)

applyOperation : Operation -> Int -> Int -> Int
applyOperation op x y =
   case op of
     Add ->
       (x + y)
     Minus ->
       (x - y)   
     Multiply ->
       (x * y)


          
           


--VIEW

calculatorButton : Msg -> String -> Html Msg
calculatorButton msg buttonText = 
    button
        [ class "button",
          onClick msg
        ]
        [ span [] [ text buttonText ]]

calculatorButtonWide : Msg -> String -> Html Msg
calculatorButtonWide msg buttonText = 
    button
        [ class "button wide",
          onClick msg
        ]
        [ span [] [ text buttonText ]]

stylesheet : Html a
stylesheet = 
    let 
        tag =  
            "link"
        
        attrs = 
            [attribute "Rel" "stylesheet",
             attribute "property" "stylesheet",
             attribute "href" "styles.css"
            ]

        child = 
            []

    in 
        node tag attrs child

view : Model -> Html Msg
view model =
    div [class "tudo"]  [ h1 [] [text "Calculadora v1"],
      div [class "calculator"]
          [ stylesheet,
            div [class "row"]
              [ div [class "col-xs-12"]
                  [div [class "display"]
                      [div [class "display-text"]
                          [text (displayState model)]
                      ]
                  , div [ class "buttons" ]
                    [ calculatorButtonWide Clear "C",
                      calculatorButton (Number 7) "7",
                      calculatorButton (Number 8) "8",
                      calculatorButton (Number 9) "9",
                      calculatorButton Clear "÷",
                      calculatorButton (Number 4) "4",
                      calculatorButton (Number 5) "5",
                      calculatorButton (Number 6) "6",
                      calculatorButton (Op Multiply) "X",
                      calculatorButton (Number 1) "1",
                      calculatorButton (Number 2) "2",
                      calculatorButton (Number 3) "3",
                      calculatorButton (Op Minus) "-",
                      calculatorButton Clear "0",
                      calculatorButton Clear ",",
                      calculatorButton Equal "=",
                      calculatorButton (Op Add) "+"                   
                    ] 
                  ]
              ]         
          ], 
      div [class "teste"] [text (modelTotext model)]   
    ]


displayState : Model -> String
displayState model =
    case model of
      Oneoperand x -> 
        String.fromInt(x)
      TwoOperand x operation y ->
        String.fromInt(y)

modelTotext : Model -> String
modelTotext model =
  case model of 
      Oneoperand x -> 
        "Oneoperand: " ++ String.fromInt(x)
      TwoOperand x operation y ->
        "TwoOperand: " ++ String.fromInt(x) ++ " " ++ operationTotext(operation) ++ " " ++ String.fromInt(y)  

operationTotext : Operation -> String
operationTotext op =
   case op of
     Add ->
       "Add"
     Minus ->
       "Minus"  
     Multiply ->
       "Multiply"