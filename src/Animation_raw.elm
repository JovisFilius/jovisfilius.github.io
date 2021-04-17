module Animation_raw exposing (..)

-- import Animator exposing (Timeline)
import Html exposing (Html)
import Element exposing
    ( Element
    , width
    , height
    , px
    , layout
    , html
    , behindContent
    , inFront
    , below
    , alignLeft
    , alignRight
    , centerY
    , padding
    , spacing
    , row
    , column
    , moveRight
    , moveDown
    , none
    , text
    , fill
    , el
    , rgb
    , rgb255
    , htmlAttribute
    )
import Svg exposing (Svg, svg, circle, rect)
import Svg.Attributes as S
import Element.Background as Background
import Element.Font as Font
import Element.Border as Border
import Element.Input as Input
import Html.Attributes as H
import Browser
import Browser.Events as E
import Browser.Dom
import Json.Decode as D
import Task
import Time


-- MAIN


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


init : () -> (Model, Cmd Msg)
init _ =
    ( { window = Window 800 600
      , c = Position 500 200
      , particleCloud =
          [ Particle (Position 188 185) (Velocity 0 0) [] 60 5
          , Particle (Position 153 163) (Velocity 0 0) [] 60 5
          , Particle (Position 451 963) (Velocity 0 0) [] 60 5
          , Particle (Position 153 363) (Velocity 0 0) [] 60 5
          , Particle (Position 265 860) (Velocity 0 0) [] 60 5
          , Particle (Position 86 201) (Velocity 0 0) [] 60 5
          , Particle (Position 62 50) (Velocity 0 0) [] 60 5
          , Particle (Position 652 150) (Velocity 0 0) [] 60 5
          , Particle (Position 365 260) (Velocity 0 0) [] 60 5
          , Particle (Position 586 20) (Velocity 0 0) [] 60 5
          , Particle (Position 362 650) (Velocity 0 0) [] 60 5
          , Particle (Position 952 150) (Velocity 0 0) [] 60 5
          , Particle (Position 265 260) (Velocity 0 0) [] 60 5
          , Particle (Position 86 201) (Velocity 0 0) [] 60 5
          , Particle (Position 162 550) (Velocity 0 0) [] 60 5
          , Particle (Position 252 150) (Velocity 0 0) [] 60 5
          ]
      , visibility = E.Visible
      , followMe = False
      }
    , Browser.Dom.getViewport
        |> Task.attempt
            (\res ->
                case res of
                    Ok viewport ->
                        Resized
                            (round viewport.scene.width)
                            (round viewport.scene.height)

                    Err err ->
                        Resized
                            800
                            600
            )
    )


type alias Model =
    { window : Window
    , c : Position
    , particleCloud : List Particle
    , visibility : E.Visibility
    , followMe : Bool
    }


type alias Window =
    { width : Int
    , height : Int
    }




type alias Particle =
    { p : Position
    , v : Velocity
    , tail : List Position
    , m : Float
    , size : Int
    }


type alias Position =
    { x : Float
    , y : Float
    }


type alias Velocity =
    { x : Float
    , y : Float
    }



-- UPDATE


type Msg
    = Tick Time.Posix
    | Resized Int Int
    | VisibilityChanged E.Visibility
    | MouseMove Float Float
    | ToggleMouse


update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    let
        center : Position
        center = model.c

        updateParticle : Position -> Particle -> Particle
        updateParticle c p =
            moveParticle <| updateParticleVelocity c p
            -- c p |> updateParticleVelocity |> moveParticle
    in
    case msg of
        Tick newTime ->
            ( { model | particleCloud =
                List.map (updateParticle center) model.particleCloud }
            , Cmd.none
            )

        Resized width height ->
            ( { model | window = Window width height }
            , Cmd.none
            )

        VisibilityChanged vis ->
            ( { model | visibility = vis }
            , Cmd.none
            )

        MouseMove x y ->
            -- ( model
            ( { model | c = Position x y }
            , Cmd.none
            )

        ToggleMouse ->
            ( { model | followMe = not model.followMe }
            , Cmd.none
            )

updateParticleVelocity : Position -> Particle -> Particle
updateParticleVelocity center particle =
    let
        x = particle.p.x - center.x
        y = particle.p.y - center.y
        vx = clip 20 <| particle.v.x + (force x) / particle.m
        vy = clip 20 <| particle.v.y + (force y) / particle.m
        -- Define a suitable force field, based on the distance of a particle
        -- to some reference point (i.e. the force field is radial)
        force : Float -> Float
        force u = 
            -- F = -k uu
            if u > 0
            then max (-0.01 * u^2) -100
            else min (0.01 * u^2) 100
            -- max (dist - 25/dist^2) -7

        clip : Float -> Float -> Float
        clip bound input =
            min bound input |> max (-1*bound)

        -- accelerate : (Float, Float) -> Particle -> Particle
        -- accelerate fx fy particle =
        --     { particle
        --     | vx =
        --         if 


        -- calcSpeed : Velocity -> Float
        -- calcSpeed velocity =
        --     sqrt velocity.x^2 + velocity.y^2

        -- clipVelocity : Float -> Velocity -> Velocity
        -- clipVelocity clipVal velocity =
        --     let
        --         speed = calcSpeed velocity
        --         scaleFactor =
        --             if speed > 0
        --             then clipVal / speed
        --             else 1
        --     in
        --         if speed > clipVal
        --         then Velocity (velocity.x * scaleFactor) (velocity.y * scaleFactor)
        --         else velocity

        -- fx = force(center.x - particle.p.x)
        -- fy = force(center.y - particle.p.y)
        -- vx = particle.v.x + fx / particle.m
        -- vy = particle.v.y + fy / particle.m
        -- v = clipVelocity 20 <| Velocity vx vy
        -- -- v = Velocity vx vy

    in
        { particle | v = Velocity vx vy }


moveParticle : Particle -> Particle
moveParticle particle =
    let
        move : Position -> Velocity -> Position
        move p v =
            { p
            | x = p.x+v.x
            , y = p.y+v.y
            }
    in
    { particle | p = move particle.p particle.v }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.visibility of
        E.Visible ->
            -- Only handle animation logic when the page is visible (i.e. the
            -- browser tab is in view)
            Sub.batch 
            [ E.onResize Resized
            , E.onAnimationFrame Tick
            , E.onVisibilityChange VisibilityChanged
            , if model.followMe
                then E.onMouseMove (D.map2 MouseMove (D.field "pageX" D.float)
                    (D.field "pageY" D.float))
                else Sub.none
            ]

        E.Hidden ->
            E.onVisibilityChange VisibilityChanged



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Particle Cloud"
    , body =
        [ layout 
            []
            ( column
                [ width fill ]
                [ row
                    [ width fill
                    , Background.color <| rgb 0.2 0.8 0.8
                    -- , padding 30
                    , behindContent
                        <| html <|
                            svg
                                [ S.viewBox ("0 0 " ++ String.fromInt
                                model.window.width ++ " " ++ String.fromInt
                                model.window.height)
                                -- , S.fill "255 0 0"
                                , H.style "position" "fixed"
                                , H.style "top" "0"
                                , H.style "left" "0"
                                , S.width "100%"
                                , S.height "100%"
                                ]
                                <|
                                List.map viewParticle model.particleCloud                                
                                ++
                                [ rect
                                    [ S.width "100%"
                                    , S.height "100%"
                                    , S.opacity "0"
                                    -- , S.fill "#FF0000"
                                    ]
                                    []
                                -- , viewCloudCenter model.c
                                ]
                            ]
                            <|
                            [ el
                                [ Font.size 32
                                , padding 10
                                ]
                                ( text "This is a W.I.P." )
                            , Input.checkbox
                                [ ] <|
                                { onChange = always ToggleMouse
                                , label =
                                    Input.labelRight
                                    [alignRight]
                                    ( text "Activate mouse following")
                                , checked = model.followMe
                                , icon =
                                    toggleWidget
                                }
                            ]
                            -- ++ (List.map viewParticleStats model.particleCloud)
                ]
            )
        ]
    }


viewCloudCenter : Position -> Svg msg
viewCloudCenter position =
    circle
        [ S.cx <| String.fromFloat position.x
        , S.cy <| String.fromFloat position.y
        , S.r "3"
        -- , height <| px 3
        , S.color "0 0 0"
        -- , moveRight position.x
        -- , moveDown position.y
        -- , below 
                -- [ width <| px 3
                -- , height <| px 3
                -- , Background.color <| rgb 0.3 0.3 0.3
                -- ]
                -- ( none )
        ]
        []
        -- ( none )


viewParticle : Particle -> Svg msg
viewParticle particle =
    rect
        [ S.width <| String.fromInt particle.size
        , S.height <| String.fromInt particle.size
        , S.fill "#000"
        , S.x <| String.fromFloat (particle.p.x - toFloat particle.size/2.0)
        , S.y <| String.fromFloat (particle.p.y - toFloat particle.size/2.0)
        -- , moveRight particle.p.x
        -- , moveDown particle.p.y
        ]
        []
        -- ( text <| ("(" ++ (String.fromInt <| round particle.p.x) ++ ", " ++
        -- (String.fromInt <| round particle.p.y) ++ ")(" ++ (String.fromInt <| round
        -- particle.v.x) ++ ", " ++ (String.fromInt <| round particle.v.y) ++ ")" )) 


viewParticleStats : Particle -> Element msg
viewParticleStats particle = 
    text <| "Position: " ++ (String.fromInt <| round particle.p.x) ++ ", " ++
    (String.fromInt <| round particle.p.y) ++ "; Velocity: " ++ (String.fromInt <| round
    particle.v.x) ++ ", " ++ (String.fromInt <| round particle.v.y)


toggleWidget : Bool -> Element msg
toggleWidget checked =
    let
        pad =
            3
        toggleHeight = 28
        toggleWidth = 60

        onColor =
            rgb255 39 203 139
        offColor =
            rgb255 187 187 187
        sliderColor =
            rgb255 255 255 255

        sliderSize =
            toggleHeight - 2 * pad

        translation =
            (toggleWidth - sliderSize - pad)
                |> String.fromInt
    in
    el
        [ Background.color <|
            if checked
            then onColor
            else offColor
        , width <| px toggleWidth
        , height <| px toggleHeight
        , Border.rounded 14
        , alignRight
        , inFront <|
            el [ height fill
               ] <|
                el
                    [ Background.color sliderColor
                    , Border.rounded <| sliderSize // 2
                    , width <| px sliderSize
                    , height <| px sliderSize
                    , centerY
                    , moveRight pad
                    , htmlAttribute <|
                        H.style "transition" ".4s"
                    , htmlAttribute <|
                        if checked then
                            H.style "transform" <| "translateX(" ++ translation
                            ++ "px)"
                        else
                            H.class ""
                    ]
                <|
                    ( none)
        ]
    ( none )
