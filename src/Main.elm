module Main exposing (Choice, Game, Model, Msg(..), Question, Outcome, Source, Tree(..), choiceDecoder, init, main, newGame, questionDecoder, outcomeDecoder, sourceDecoder, subscriptions, treeDecoder, update, updateGame, updateNoOp, view, viewChoice, viewError, viewGame, viewLoading, viewQuestion, viewOutcome, viewSource, viewTree)

import Browser
import Http
import Json.Decode as D
import Json.Encode as E
import Navigator
import Time
import Url.Builder as Url
import Element as El exposing (Element)
import Element.Border as Border
import Element.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Element.Font as Font
import Element.Input as Input
import Element.Background as Background
import Html exposing (Html)
import Html.Attributes
import Html.Events
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
    , emailAddress : Maybe String
    , mailSendState : MailSendState
    }

type MailSendState
    = MailUnsent
    | MailSending
    | MailSentSuccessfully
    | MailSentWithError

type OutcomeType
    = RecommendationType
    | DrawerType


newGame : Tree -> Game
newGame tree =
    { base = tree
    , navigator = Navigator.new tree
    , idleTime = 0
    , activeChoices = Set.empty
    , emailAddress = Nothing
    , mailSendState = MailUnsent
    }


type alias Model =
    Result String Game


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
    = Select Choice
    | Reset
    | Back
    | Forward
    | Tick Time.Posix
    | Highlight Choice
    | Unhighlight Choice
    | SendMail SendMailRequest
    | GotSendMailResponse (Result Http.Error SendMailResponse)
    | UserTypedEmailAddress String

type alias SendMailRequest =
    { to : String
    , recommendation : String
    , source : String
    }

sendMailRequestEncoder : SendMailRequest -> E.Value
sendMailRequestEncoder smr =
    E.object
        [ ( "to", E.string smr.to )
        , ( "recommendation", E.string smr.recommendation )
        , ( "source", E.string smr.source )
        ]

type alias SendMailResponse =
    {}

sendMailResponseDecoder : D.Decoder SendMailResponse
sendMailResponseDecoder =
    D.succeed {}

updateNoOp : Model -> ( Model, Cmd Msg )
updateNoOp model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Ok game ->
            updateGame msg game

        Err _ ->
            updateNoOp model

updateGame : Msg -> Game -> ( Model, Cmd Msg )
updateGame msg game =
    case msg of
        Select choice ->
            ( Ok ({ game | navigator = Navigator.push choice.result game.navigator, activeChoices = game.activeChoices |> Set.remove choice.text} |> resetIdleTime), Cmd.none )

        Reset ->
            ( Ok (resetGame game), Cmd.none )

        Back ->
            ( Ok ({ game | navigator = Navigator.back game.navigator } |> resetIdleTime), Cmd.none )

        Forward ->
            ( Ok ({ game | navigator = Navigator.forward game.navigator} |> resetIdleTime), Cmd.none )
        
        Tick _ ->
            if game.idleTime > 60 then
                ( Ok (resetGame game), Cmd.none )
            else
                ( Ok { game | idleTime = game.idleTime + 1}, Cmd.none )

        Highlight choice ->
            ( Ok ({ game | activeChoices = game.activeChoices |> Set.insert choice.text} |> resetIdleTime), Cmd.none )
        
        Unhighlight choice ->
            ( Ok ({ game | activeChoices = game.activeChoices |> Set.remove choice.text} |> resetIdleTime), Cmd.none )
        
        SendMail smr ->
            let
                newModel = Ok { game | mailSendState = MailSending }
                sendMailMsg =
                    Http.post
                        { url = "https://animechicago-cyoa-content.herokuapp.com/game/mail/"
                        , body = Http.jsonBody (sendMailRequestEncoder smr)
                        , expect = Http.expectJson GotSendMailResponse sendMailResponseDecoder
                        }
            
            in
            ( newModel, sendMailMsg )
        
        GotSendMailResponse res ->
            let
                newModel =
                    case res of
                        Ok resp ->
                            Ok { game | mailSendState = MailSentSuccessfully }
                        
                        Err _ ->
                            Ok { game | mailSendState = MailSentWithError }
            
            in
            ( newModel, Cmd.none )
        
        UserTypedEmailAddress addr ->
            ( Ok { game | emailAddress = Just addr }, Cmd.none )


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
    , Font.bold
    , Styles.font
    ] (viewModel model)


viewModel : Model -> Element Msg
viewModel model =
    case model of
        Ok game ->
            viewGame game

        Err error ->
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
        , viewTree game (Navigator.getCurrent game.navigator) game.activeChoices
        ]


viewTree : Game -> Tree -> Set String -> Element Msg
viewTree g tree activeChoices =
    case tree of
        Node question ->
            viewQuestion question activeChoices

        Leaf outcome ->
            viewOutcome g outcome


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


viewOutcome : Game -> Outcome -> Element Msg
viewOutcome g recommendation =
    case recommendation of
        Recommendation info ->
            viewRecommendationData g info
        
        Drawer drawer ->
            viewDrawer drawer


viewRecommendationData : Game -> RecommendationData -> Element Msg
viewRecommendationData g info =
    El.column
        [ El.spacing 90
        ]
        [ El.row
            []
            [ El.el
                []
                ( El.text "You should watch " )
            , El.el
                [ Font.italic
                ]
                ( El.text info.title )
            
            , El.el
                []
                ( El.text "." )
            ]
        , emailWidget g info
        ]

emailWidget : Game -> RecommendationData -> Element Msg
emailWidget g info =
    let
        secondaryColor = Styles.widgetColor
    in
    El.column
        [ Border.color secondaryColor
        , Border.width 1
        , El.width <| El.px 500
        ]
        [ El.column
            [ Background.color secondaryColor
            , El.paddingXY 30 15
            , El.alignLeft
            , El.width El.fill
            ]
            [ El.el
                [ Font.bold
                , Font.size 14 
                ]
                ( El.text "SEND YOURSELF A REMINDER!" )
            ]
        , El.column
            [ El.padding 30
            , El.spacing 30
            ]
            [ Input.email
                [ Font.alignLeft
                , Font.size 16
                , El.spacing 15
                , El.width <| El.px 350
                , Border.rounded 5
                , El.padding 15
                , Font.regular
                , Font.color <| El.rgb255 0 0 0
                ]
                { onChange = UserTypedEmailAddress
                , text = g.emailAddress |> Maybe.withDefault ""
                , placeholder = Just <|
                    Input.placeholder
                        [ Font.color <| El.rgb255 150 150 150
                        ]
                        ( El.text "name@email.address" )
                , label =
                    Input.labelAbove
                        [ El.alignLeft
                        , Font.size 20
                        ]
                        ( El.text "Your Email Address" )
                }
            , Input.button
                [ Background.color secondaryColor
                , Border.rounded 5
                , El.paddingXY 30 15
                , Font.size 20
                ]
                { onPress = Just (SendMail
                    { to = g.emailAddress |> Maybe.withDefault ""
                    , recommendation = info.title
                    , source = info.availableOn |> List.head |> Maybe.map (\s -> s.name) |> Maybe.withDefault "Nowhere"
                    }
                )
                , label = El.text "Send Email"
                }
            ]
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


viewError : String -> Element msg
viewError message =
    El.text message


bigText : String -> Element msg
bigText message = El.el [ Font.size 30 ] (El.text message)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick


-- ENTRY POINT


init : D.Value -> ( Model, Cmd Msg )
init json =
    case getTree json of
        Ok tree ->
            ( Ok (newGame tree)
            , Cmd.none
            )
        
        Err message ->
            ( Err message
            , Cmd.none
            )

decodeOutcomeTypeString : String -> D.Decoder OutcomeType
decodeOutcomeTypeString string =
    case string of
        "drawer" ->
            D.succeed DrawerType
        
        "recommendation" ->
            D.succeed RecommendationType
        
        _ ->
            D.fail <| "Unrecognized outcome type: " ++ string


outcomeTypeDecoder : D.Decoder OutcomeType
outcomeTypeDecoder =
    D.field "outcomeType" D.string
        |> D.andThen decodeOutcomeTypeString

getTree : D.Value -> Result String Tree
getTree json =
    json
        |> D.decodeValue outcomeTypeDecoder
        |> Result.andThen (\outcomeType -> D.decodeValue (D.field "data" (treeDecoder outcomeType)) json)
        |> Result.mapError D.errorToString


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
