module Animation exposing (..)

import Animator exposing (Timeline)
import Html exposing (Html)
import Element exposing
    ( Element
    , width
    , height
    , px
    , layout
    , padding
    , row
    , column
    , moveRight
    , moveDown
    , none
    , text
    , fill
    , el
    , rgb
    )
import Element.Background as Background
import Html.Attributes as Attr
import Browser
import Browser.Events as E
import Browser.Dom
import Task
import Time


-- MAIN


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions =
            \model ->
                animator
                    |> Animator.toSubscription Tick model
        }



-- MODEL


init : () -> (Model, Cmd Msg)
init _ =
    ( { window = Window 800 600
      , c = Animator.init <| Position 100 100
      , particleCloud = Animator.init
          <|
              [ Particle (Position 150 150) (Velocity 3 3) [] 3 5 ]
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
    , c : Timeline Position
    , particleCloud : Timeline (List Particle)
    }


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.watching
            .c
            (\c model ->
                { model | c = c }
            )


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


update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
        Tick newTime ->
            ( model
                |> Animator.update newTime animator
            , Cmd.none
            )

        Resized width height ->
            ( { model | window = Window width height }
            , Cmd.none
            )


updateParticleVelocity : Particle -> Model -> Particle
updateParticleVelocity particle model =
    let
        center = Animator.current model.c
        fx = force(center.x - particle.p.x)
        fy = force(center.y - particle.p.y)
        vx = particle.v.x + fx / particle.m
        vy = particle.v.y + fy / particle.m
        v = Velocity vx vy
    in
        { particle | v = v }


{-| Define a suitable force field, based on the distance of a particle to some
reference point (i.e. the force field is radial)
-}
force : Float -> Float
force dist = 
    dist



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
    [ E.onResize Resized
    , E.onAnimationFrame Tick
    , E.onVisibilityChange VisibilityChanged
    ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Particle Cloud"
    , body =
        [ Html.h1 [] [ Html.text "This is a W.I.P." ]
        , Html.div
            [ Attr.class "viewport"
            ]
            [ viewCloudCenter model.c
            , layout
                [ width fill
                , Background.color <| rgb 0.2 0.8 0.8
                -- , padding 30
                ]
                ( row
                    []
                    <|
                    List.map viewParticle (Animator.current model.particleCloud)
                    ++
                    [ text "bar"
                    ]
                )
            ]
        ]
    }


viewCloudCenter : Timeline Position -> Html Msg
viewCloudCenter position =
    Html.div
        []
        [] 

viewParticle : Particle -> Element msg
viewParticle particle =
    el
        [ width <| px particle.size
        , height <| px particle.size
        , Background.color <| rgb 0.8 0.8 0.2
        , moveRight particle.p.x
        , moveDown particle.p.y
        ]
        ( none )


