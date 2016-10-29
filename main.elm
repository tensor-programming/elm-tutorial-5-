module Main exposing (..)

import Html.App exposing (program)
import Window
import Time exposing (Time)
import Random
import Keyboard
import Task
import Html
import Html.Attributes exposing (style)
import Svg exposing (Svg, Attribute, svg, rect, defs, filter, feGaussianBlur, feMerge, feMergeNode)
import Svg.Attributes exposing (width, height, viewBox, x, y, rx, fill, id, stdDeviation, result, in')


main =
    program { init = ( init, initCmds ), update = update, view = render, subscriptions = subscriptions }


type alias Game =
    { direction : Direction
    , dimensions : Window.Size
    , snake : Snake
    , isDead : Bool
    , fruit : Maybe Block
    , ateFruit : Bool
    , paused : Bool
    }


type alias Block =
    { x : Int
    , y : Int
    }


type alias Snake =
    List Block


type Direction
    = Left
    | Right
    | Up
    | Down


type ArrowKey
    = NoKey
    | Space
    | LeftKey
    | RightKey
    | UpKey
    | DownKey


type alias FruitSpawn =
    { position : ( Int, Int )
    , chance : Int
    }


type Msg
    = ArrowPressed ArrowKey
    | SizeUpdated Window.Size
    | Tick Time
    | MaybeSpawnFruit FruitSpawn


initSnake : Snake
initSnake =
    [ Block 25 25
    , Block 24 25
    , Block 23 25
    ]


init : Game
init =
    { direction = Right
    , dimensions = Window.Size 0 0
    , snake = initSnake
    , isDead = False
    , fruit = Nothing
    , ateFruit = False
    , paused = False
    }


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        ArrowPressed Space ->
            ( { game | paused = not game.paused }, Cmd.none )

        ArrowPressed arrow ->
            ( updateDirection arrow game, Cmd.none )

        SizeUpdated dimensions ->
            ( { game | dimensions = dimensions }, Cmd.none )

        Tick time ->
            updateGame game

        MaybeSpawnFruit spawn ->
            if spawn.chance == 0 then
                ( spawnFruit game spawn, Cmd.none )
            else
                ( game, Cmd.none )


spawnFruit : Game -> FruitSpawn -> Game
spawnFruit game spawn =
    let
        ( x, y ) =
            spawn.position
    in
        { game | fruit = Just { x = x, y = y } }


updateGame : Game -> ( Game, Cmd Msg )
updateGame game =
    if game.isDead || game.paused then
        ( game, Cmd.none )
    else
        ( game, Cmd.none )
            |> checkIfOutOfBounds
            |> checkIfEatenSelf
            |> checkIfAteFruit
            |> updateSnake
            |> updateFruit


checkIfEatenSelf : ( Game, Cmd Msg ) -> ( Game, Cmd Msg )
checkIfEatenSelf ( game, cmd ) =
    let
        head =
            snakeHead game.snake

        tail =
            List.drop 1 game.snake

        isDead =
            game.isDead || List.any (samePosition head) tail
    in
        ( { game | isDead = isDead }, cmd )


checkIfAteFruit : ( Game, Cmd Msg ) -> ( Game, Cmd Msg )
checkIfAteFruit ( game, cmd ) =
    let
        head =
            snakeHead game.snake
    in
        case game.fruit of
            Nothing ->
                ( { game | ateFruit = False }, cmd )

            Just fruit ->
                ( { game | ateFruit = samePosition head fruit }, cmd )


samePosition : Block -> Block -> Bool
samePosition a b =
    a.x == b.x && a.y == b.y


checkIfOutOfBounds : ( Game, Cmd Msg ) -> ( Game, Cmd Msg )
checkIfOutOfBounds ( game, cmd ) =
    let
        head =
            snakeHead game.snake

        isDead =
            (head.x == 0 && game.direction == Left)
                || (head.y == 0 && game.direction == Up)
                || (head.x == 49 && game.direction == Right)
                || (head.y == 49 && game.direction == Down)
    in
        ( { game | isDead = isDead }, cmd )


snakeHead : Snake -> Block
snakeHead snake =
    List.head snake
        |> Maybe.withDefault { x = 0, y = 0 }


updateFruit : ( Game, Cmd Msg ) -> ( Game, Cmd Msg )
updateFruit ( game, cmd ) =
    case game.fruit of
        Nothing ->
            let
                chance =
                    Random.int 0 9

                x =
                    Random.int 0 49

                y =
                    Random.int 0 49

                pos =
                    Random.pair x y
            in
                ( game, Random.generate MaybeSpawnFruit makeFruitSpawnGenerator )

        Just fruit ->
            if game.ateFruit then
                ( { game | fruit = Nothing }, cmd )
            else
                ( game, cmd )


makeFruitSpawnGenerator : Random.Generator FruitSpawn
makeFruitSpawnGenerator =
    let
        spawnPosition =
            Random.pair (Random.int 0 49) (Random.int 0 49)

        spawnChance =
            Random.int 0 9
    in
        Random.map2 (\pos chance -> { position = pos, chance = chance }) spawnPosition spawnChance


updateSnake : ( Game, Cmd Msg ) -> ( Game, Cmd Msg )
updateSnake ( game, cmd ) =
    let
        head =
            snakeHead game.snake

        head' =
            case game.direction of
                Up ->
                    { head | y = head.y - 1 }

                Down ->
                    { head | y = head.y + 1 }

                Left ->
                    { head | x = head.x - 1 }

                Right ->
                    { head | x = head.x + 1 }

        tailPositions =
            if game.ateFruit then
                game.snake
            else
                List.take ((List.length game.snake) - 1) game.snake

        tailXs =
            List.map .x tailPositions

        tailYs =
            List.map .y tailPositions

        tail' =
            List.map2 Block tailXs tailYs
    in
        if game.isDead then
            ( game, cmd )
        else
            ( { game | snake = head' :: tail' }, cmd )


updateDirection : ArrowKey -> Game -> Game
updateDirection key game =
    let
        { direction } =
            game

        direction' =
            if key == LeftKey && direction /= Right then
                Left
            else if key == RightKey && direction /= Left then
                Right
            else if key == UpKey && direction /= Down then
                Up
            else if key == DownKey && direction /= Up then
                Down
            else
                direction
    in
        { game | direction = direction' }


subscriptions : Game -> Sub Msg
subscriptions model =
    Sub.batch [ arrowChanged, windowDimensionsChanged, tick ]


initCmds : Cmd Msg
initCmds =
    Task.perform SizeUpdated SizeUpdated Window.size


windowDimensionsChanged : Sub Msg
windowDimensionsChanged =
    Window.resizes SizeUpdated


tick : Sub Msg
tick =
    Time.every (100 * Time.millisecond) Tick


arrowChanged : Sub Msg
arrowChanged =
    Keyboard.downs toArrowChanged


toArrowChanged : Keyboard.KeyCode -> Msg
toArrowChanged code =
    case code of
        32 ->
            ArrowPressed Space

        37 ->
            ArrowPressed LeftKey

        38 ->
            ArrowPressed UpKey

        39 ->
            ArrowPressed RightKey

        40 ->
            ArrowPressed DownKey

        default ->
            ArrowPressed NoKey


size : String
size =
    "100"


backgroundColor : Attribute Msg
backgroundColor =
    fill "#333333"


render : Game -> Html.Html Msg
render game =
    let
        ( scaledWidth, scaledHeight ) =
            scale game.dimensions

        parentStyle =
            style [ ( "margin", "0 auto" ), ( "display", "block" ) ]
    in
        svg
            [ width scaledWidth, height scaledHeight, viewBox "0 0 50 50", parentStyle ]
            ([ renderBackground ]
                ++ renderSnake game.snake
                ++ renderFruit game.fruit
            )


renderBackground : Svg Msg
renderBackground =
    rect [ x "0", y "0", width size, height size, backgroundColor ] []


renderSnake : Snake -> List (Svg Msg)
renderSnake snake =
    List.map renderBlock snake


renderBlock : Block -> Svg Msg
renderBlock block =
    let
        ( strX, strY ) =
            ( toString block.x, toString block.y )
    in
        rect [ x strX, y strY, width "1", height "1", fill "red", rx "0.2" ] []


renderFruit : Maybe Block -> List (Svg Msg)
renderFruit fruit =
    case fruit of
        Nothing ->
            []

        Just fruit ->
            [ renderBlock fruit ]


scale : Window.Size -> ( String, String )
scale size =
    let
        toPixelStr =
            \i -> round i |> toString

        ( fWidth, fHeight ) =
            ( toFloat size.width, toFloat size.height )

        ( scaledX, scaledY ) =
            if fWidth > fHeight then
                ( fHeight / fWidth, 1.0 )
            else
                ( 1.0, fWidth / fHeight )
    in
        ( toPixelStr (fWidth * scaledX), toPixelStr (fHeight * scaledY) )
