module Main exposing (Choice, Game, Model, Msg(..), Question, Outcome, RequestData(..), Source, Tree(..), apiEndpointUrl, choiceDecoder, init, loadApplication, main, newGame, questionDecoder, outcomeDecoder, sourceDecoder, subscriptions, treeDecoder, update, updateGame, updateLoad, updateNoOp, view, viewChoice, viewError, viewGame, viewLoading, viewQuestion, viewOutcome, viewSource, viewTree)

import Browser
import Http
import Json.Decode as D
import Navigator
import Time
import Url.Builder as Url
import Element as El exposing (Element)
import Element.Border as Border
import Element.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Element.Font as Font
import Element.Background as Background
import Html exposing (Html)
import Styles
import Set exposing (Set)
import Icons


-- MODEL


type Tree
    = Node Question
    | Leaf Outcome


type alias Question =
    { text : String
    , choices : List Choice
    }


type alias Source =
    { name : String
    , url : String
    }


type Outcome
    = Recommendation RecommendationData
    | Drawer DrawerData


type alias RecommendationData =
    { title : String
    , availableOn : List Source
    }
    
type alias DrawerData =
    { drawer : Maybe Int
    }

type alias Drawer =
    Maybe Int


type alias Choice =
    { text : String
    , result : Tree
    }


type alias Game =
    { base : Tree
    , navigator : Navigator.Navigator Tree
    , idleTime : Int
    , activeChoices : Set String
    }

type OutcomeType
    = RecommendationType
    | DrawerType


newGame : Tree -> Game
newGame tree =
    { base = tree
    , navigator = Navigator.new tree
    , idleTime = 0
    , activeChoices = Set.empty
    }


type RequestData a
    = Loading
    | Success a
    | Failure Http.Error


type alias Model =
    RequestData Game



-- JSON

    
questionDecoder : OutcomeType -> D.Decoder Question
questionDecoder outcomeType =
    D.map2 Question
        (D.field "text" D.string)
        (D.field "choices" (D.list (D.lazy (\_ -> (choiceDecoder outcomeType)))))


sourceDecoder : D.Decoder Source
sourceDecoder =
    D.map2 Source
        (D.field "name" D.string)
        (D.field "url" D.string)


recommendationDecoder : D.Decoder Outcome
recommendationDecoder =
    D.map Recommendation (
        D.map2 RecommendationData
            (D.field "title" D.string)
            (D.field "available_on" (D.list sourceDecoder)))

drawerDecoder : D.Decoder Outcome
drawerDecoder =
    D.map Drawer (
        D.map DrawerData
            (D.maybe <| D.field "drawer" D.int))

outcomeDecoder : OutcomeType -> D.Decoder Outcome
outcomeDecoder outcomeType =
    case outcomeType of
        RecommendationType ->
            recommendationDecoder
        
        DrawerType ->
            drawerDecoder


choiceDecoder : OutcomeType -> D.Decoder Choice
choiceDecoder outcomeType =
    D.map2 Choice
        (D.field "text" D.string)
        (D.field "result" (treeDecoder outcomeType))


treeDecoder : OutcomeType -> D.Decoder Tree
treeDecoder outcomeType =
    D.oneOf [ D.map Node (questionDecoder outcomeType), D.map Leaf (outcomeDecoder outcomeType) ]



-- UPDATE


type Msg
    = Load (Result Http.Error Tree)
    | Select Choice
    | Reset
    | Back
    | Forward
    | Tick Time.Posix
    | Highlight Choice
    | Unhighlight Choice


updateNoOp : Model -> ( Model, Cmd Msg )
updateNoOp model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loading ->
            updateLoad msg model

        Success game ->
            updateGame msg game

        Failure _ ->
            updateNoOp model


updateLoad : Msg -> Model -> ( Model, Cmd Msg )
updateLoad msg model =
    case msg of
        Load result ->
            case result of
                Ok tree ->
                    ( Success (newGame tree), Cmd.none )

                Err error ->
                    ( Failure error, Cmd.none )

        _ ->
            updateNoOp model


updateGame : Msg -> Game -> ( Model, Cmd Msg )
updateGame msg game =
    case msg of
        Select choice ->
            ( Success ({ game | navigator = Navigator.push choice.result game.navigator, activeChoices = game.activeChoices |> Set.remove choice.text} |> resetIdleTime), Cmd.none )

        Reset ->
            ( Success (resetGame game), Cmd.none )

        Back ->
            ( Success ({ game | navigator = Navigator.back game.navigator } |> resetIdleTime), Cmd.none )

        Forward ->
            ( Success ({ game | navigator = Navigator.forward game.navigator} |> resetIdleTime), Cmd.none )
        
        Tick _ ->
            if game.idleTime > 60 then
                ( Success (resetGame game), Cmd.none )
            else
                ( Success { game | idleTime = game.idleTime + 1}, Cmd.none )

        Highlight choice ->
            ( Success ({ game | activeChoices = game.activeChoices |> Set.insert choice.text} |> resetIdleTime), Cmd.none )
        
        Unhighlight choice ->
            ( Success ({ game | activeChoices = game.activeChoices |> Set.remove choice.text} |> resetIdleTime), Cmd.none )
        
        Load _ ->
            updateNoOp (Success game)


resetGame : Game -> Game
resetGame game = { game | navigator = Navigator.new game.base } |> resetIdleTime

resetIdleTime : Game -> Game
resetIdleTime game = { game | idleTime = 0 }


-- VIEW


view : Model -> Html Msg
view model = El.layout 
    [ Background.color Styles.backgroundColor
    , Font.color Styles.headerColor
    , Font.size 25
    , Styles.bold
    , Styles.font
    ] (viewModel model)


viewModel : Model -> Element Msg
viewModel model =
    case model of
        Loading ->
            viewLoading

        Success game ->
            viewGame game

        Failure error ->
            viewError error


viewLoading : Element msg
viewLoading =
    El.text "Loading"


viewGame : Game -> Element Msg
viewGame game =
    El.row [El.paddingXY 25 50]
        [ El.column 
            [ El.width (El.fill |> El.minimum 100 |> El.maximum 100)
            , El.alignTop
            , El.paddingXY 20 50
            , El.spacing 50
            , El.centerX
            ] 
            [
                (if Navigator.canGoBack game.navigator then
                    El.el [ onClick Back ] (El.html Icons.rotateCcw)
    
                 else
                    El.none
                )
            ,
                (if Navigator.canGoForward game.navigator then
                    El.el [ onClick Forward ] (El.html Icons.rotateCw)
    
                 else
                    El.none
                )
            ] 
        , viewTree (Navigator.getCurrent game.navigator) game.activeChoices
        ]


viewTree : Tree -> Set String -> Element Msg
viewTree tree activeChoices =
    case tree of
        Node question ->
            viewQuestion question activeChoices

        Leaf outcome ->
            viewOutcome outcome


viewQuestion : Question -> Set String -> Element Msg
viewQuestion question activeChoices =
    El.column [El.padding 10, El.spacing 10]
        [ El.el [] (bigText question.text)
        , El.column [El.spacing 10] (List.map2 viewChoice question.choices (List.repeat (List.length question.choices) activeChoices))
        ]


viewChoice : Choice -> Set String -> Element Msg
viewChoice choice activeChoices =
    El.el [ onClick (Select choice)
       , onMouseEnter (Highlight choice)
       , onMouseLeave (Unhighlight choice)
       , Font.color (if Set.member choice.text activeChoices then Styles.activeColor else Styles.inactiveColor)
       , Border.color (if Set.member choice.text activeChoices then Styles.activeColor else Styles.inactiveColor)
       , Border.solid
       , Border.width 1
       , El.width (El.fill |> El.minimum 750 |> El.maximum 750)
       , El.padding 25
       ] (El.text choice.text)


viewOutcome : Outcome -> Element msg
viewOutcome recommendation =
    case recommendation of
        Recommendation info ->
            viewRecommendationData info
        
        Drawer drawer ->
            viewDrawer drawer


viewRecommendationData : RecommendationData -> Element msg
viewRecommendationData info =
    El.column []
        [ El.el [] (El.text <| "You should watch " ++ info.title)
        ]


viewDrawer : DrawerData -> Element msg
viewDrawer drawerData =
    El.column []
        [ El.el [] (El.text <| "Open drawer #" ++ representDrawer drawerData)
        ]

representDrawer : DrawerData -> String
representDrawer drawerData =
    case drawerData.drawer of
        Just drawer ->
            String.fromInt drawer
        
        Nothing ->
            "None"

viewSource : Source -> Element msg
viewSource source =
    El.el [] (El.newTabLink [] { url = source.url, label = (El.text source.name) })


viewError : Http.Error -> Element msg
viewError error =
    case error of
        Http.Timeout ->
            El.text "The request timed out."

        Http.BadUrl message ->
            El.text ("URL error: " ++ message)

        Http.NetworkError ->
            El.text "Network Error."

        Http.BadStatus response ->
            El.text "Bad Status."

        Http.BadPayload message response ->
            El.text ("Bad payload " ++ message)

bigText : String -> Element msg
bigText message = El.el [ Font.size 30 ] (El.text message)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- HTTP


apiEndpointUrl : String
apiEndpointUrl =
    Url.crossOrigin "https://animechicago-cyoa-content.herokuapp.com/game/data" [] []


loadApplication : OutcomeType -> Cmd Msg
loadApplication outcomeType =
    Http.send Load (Http.get apiEndpointUrl (treeDecoder outcomeType))



-- ENTRY POINT


init : D.Value -> ( Model, Cmd Msg )
init flags =
    case getOutcomeType flags of
        Ok outcomeType ->
            ( Loading
            , loadApplication outcomeType
            )
        
        Err message ->
            ( Failure (Http.BadUrl message)
            , Cmd.none
            )

type alias Flags =
    { outcomeType : String
    }

flagsDecoder : D.Decoder Flags
flagsDecoder = D.map Flags (D.field "outcomeType" D.string)

getOutcomeType : D.Value -> Result String OutcomeType
getOutcomeType json = 
    case D.decodeValue flagsDecoder json of
        Ok flags ->
            case flags.outcomeType of
                "drawer" ->
                    Ok DrawerType
                
                "recommendation" ->
                    Ok RecommendationType
                
                _ ->
                    Err ("unknown outcome type: " ++ flags.outcomeType)
        
        Err error ->
            Err (D.errorToString error)


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
