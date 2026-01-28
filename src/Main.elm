port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode
import Kinto
import Url



---- MODEL ----


type alias Model =
    { navKey : Browser.Navigation.Key
    , pizzas : KintoData (List Pizza)
    , newPizza : NewPizza
    , newPizzaKintoData : KintoData Pizza
    , loginForm : LoginForm
    , deletePizzaList : List Pizza -- List of pizzas being deleted
    , errorList : List String
    }


type alias Pizza =
    { id : String
    , last_modified : Int
    , price : Float
    , participants : List Participant
    , date : String
    }


type alias NewPizza =
    { date : String
    , price : Float
    , participants : List Participant
    }


emptyNewPizza : NewPizza
emptyNewPizza =
    { date = ""
    , price = 0
    , participants = []
    }


type alias Participant =
    { name : String
    , half : Bool
    , paid : Bool
    }


type alias LoginForm =
    { serverURL : String
    , username : String
    , password : String
    }


emptyLoginForm : LoginForm
emptyLoginForm =
    { serverURL = ""
    , username = ""
    , password = ""
    }


type alias Flags =
    { sessionData : Encode.Value
    , newPizzaData : Encode.Value
    , serverURL : String
    }


init : Flags -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags _ key =
    let
        emptyLoginFormWithServerURL =
            { emptyLoginForm | serverURL = flags.serverURL }

        loginForm =
            -- Decode a string from the value (the stringified session data)
            Decode.decodeValue Decode.string flags.sessionData
                -- Decode a loginForm from the value
                |> Result.andThen (Decode.decodeString decodeSessionData)
                |> Result.withDefault emptyLoginFormWithServerURL

        newPizza =
            -- Decode a string from the value (the stringified newPizza data)
            Decode.decodeValue Decode.string flags.newPizzaData
                -- Decode a newPizza from the value
                |> Result.andThen (Decode.decodeString decodePizzaData)
                |> Result.withDefault emptyNewPizza

        model =
            { navKey = key
            , pizzas = NotRequested
            , newPizza = newPizza
            , newPizzaKintoData = NotRequested
            , loginForm = loginForm
            , deletePizzaList = []
            , errorList = []
            }
    in
    useLogin model



---- UPDATE ----


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | UpdatePizza NewPizza
    | AddPizza
    | PizzaAdded (Result Kinto.Error Pizza)
    | DeletePizza Pizza
    | PizzaDeleted Pizza (Result Kinto.Error DeletedPizza)
    | PizzasFetched (Result Kinto.Error (Kinto.Pager Pizza))
    | UpdateLoginForm LoginForm
    | UseLogin
    | Logout
    | DiscardError Int


isLoginFormComplete : LoginForm -> Bool
isLoginFormComplete loginForm =
    loginForm.serverURL /= "" && loginForm.username /= "" && loginForm.password /= ""


useLogin : Model -> ( Model, Cmd Msg )
useLogin model =
    if isLoginFormComplete model.loginForm then
        let
            client =
                Kinto.client model.loginForm.serverURL (Kinto.Basic model.loginForm.username model.loginForm.password)
        in
        ( { model | pizzas = Requested }
        , Cmd.batch
            [ getPizzaList client
            , saveSession <| encodeSessionData model.loginForm
            ]
        )

    else
        ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Browser.Navigation.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Browser.Navigation.load href )

        UrlChanged _ ->
            ( model, Cmd.none )

        -- LOGINFORM --
        UpdateLoginForm loginForm ->
            ( { model | loginForm = loginForm }
            , saveServerURL <| Encode.string loginForm.serverURL
            )

        UseLogin ->
            useLogin model

        PizzasFetched (Ok pizzasPager) ->
            ( { model | pizzas = Received pizzasPager.objects }, Cmd.none )

        PizzasFetched (Err err) ->
            ( { model
                | pizzas = Failed err
                , errorList = Kinto.errorToString err :: model.errorList
              }
            , Cmd.none
            )

        -- LOGGEDIN --
        UpdatePizza newPizza ->
            ( { model | newPizza = newPizza }
            , savePizza <| encodePizzaData newPizza
            )

        AddPizza ->
            let
                data =
                    encodeData
                        model.newPizza.price
                        model.newPizza.participants
                        model.newPizza.date

                client =
                    Kinto.client model.loginForm.serverURL (Kinto.Basic model.loginForm.username model.loginForm.password)
            in
            ( { model | newPizzaKintoData = Requested }
            , client
                |> Kinto.create recordResource data
                |> Kinto.send PizzaAdded
            )

        PizzaAdded (Ok pizza) ->
            let
                pizzas =
                    case model.pizzas of
                        Received pizzaList ->
                            pizza
                                :: pizzaList
                                |> List.sortBy .date
                                |> List.reverse
                                |> Received

                        _ ->
                            model.pizzas

                newPizza =
                    model.newPizza
            in
            ( { model
                | pizzas = pizzas

                -- We're going straight back to "NotRequested" as we added the pizza to the list
                , newPizzaKintoData = NotRequested
                , newPizza = newPizza
              }
            , savePizza <| encodePizzaData newPizza
            )

        PizzaAdded (Err err) ->
            ( { model
                | newPizzaKintoData = Failed err
                , errorList = Kinto.errorToString err :: model.errorList
              }
            , Cmd.none
            )

        DeletePizza pizza ->
            let
                client =
                    Kinto.client model.loginForm.serverURL (Kinto.Basic model.loginForm.username model.loginForm.password)
            in
            ( { model | deletePizzaList = pizza :: model.deletePizzaList }
            , deletePizza client pizza
            )

        PizzaDeleted pizza (Ok _) ->
            let
                pizzas =
                    case model.pizzas of
                        Received pizzaList ->
                            pizzaList
                                |> List.filter ((/=) pizza)
                                |> Received

                        _ ->
                            model.pizzas

                deletePizzaList =
                    model.deletePizzaList
                        |> List.filter ((/=) pizza)
            in
            ( { model
                | pizzas = pizzas
                , deletePizzaList = deletePizzaList
              }
            , Cmd.none
            )

        PizzaDeleted pizza (Err err) ->
            let
                deletePizzaList =
                    model.deletePizzaList
                        |> List.filter ((/=) pizza)
            in
            ( { model
                | deletePizzaList = deletePizzaList
                , errorList = Kinto.errorToString err :: model.errorList
              }
            , Cmd.none
            )

        Logout ->
            ( { model | pizzas = NotRequested, loginForm = { emptyLoginForm | serverURL = model.loginForm.serverURL } }, logoutSession () )

        DiscardError index ->
            ( { model | errorList = List.take index model.errorList ++ List.drop (index + 1) model.errorList }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Paie ta pizza !"
    , body =
        [ Html.div [ Html.Attributes.class "container" ]
            [ viewErrorList model.errorList
            , case model.pizzas of
                Received pizzas ->
                    viewPizzaList pizzas model

                _ ->
                    viewLoginForm model
            ]
        , viewGithubLink
        ]
    }


viewLoginForm : Model -> Html.Html Msg
viewLoginForm ({ loginForm } as model) =
    let
        formComplete =
            isLoginFormComplete loginForm

        buttonState =
            if formComplete then
                case model.pizzas of
                    Requested ->
                        Loading

                    _ ->
                        NotLoading

            else
                Disabled

        button =
            loadingButton "Use these credentials" buttonState
    in
    Html.form
        [ Html.Events.onSubmit UseLogin
        ]
        [ Html.fieldset []
            [ Html.legend [] [ Html.text "Kinto credentials" ]
            , Html.div [ Html.Attributes.class "input-field" ]
                [ Html.label []
                    [ Html.text "Server URL"
                    , Html.input
                        [ Html.Attributes.type_ "text"
                        , Html.Attributes.name "serverURL"
                        , Html.Attributes.value loginForm.serverURL
                        , Html.Events.onInput <| \serverURL -> UpdateLoginForm { loginForm | serverURL = serverURL }
                        ]
                        []
                    ]
                ]
            , Html.div [ Html.Attributes.class "input-field" ]
                [ Html.label []
                    [ Html.text "Username"
                    , Html.input
                        [ Html.Attributes.type_ "text"
                        , Html.Attributes.name "username"
                        , Html.Attributes.value loginForm.username
                        , Html.Events.onInput <| \username -> UpdateLoginForm { loginForm | username = username }
                        ]
                        []
                    ]
                ]
            , Html.div [ Html.Attributes.class "input-field" ]
                [ Html.label []
                    [ Html.text "Password"
                    , Html.input
                        [ Html.Attributes.type_ "password"
                        , Html.Attributes.value loginForm.password
                        , Html.Events.onInput <| \password -> UpdateLoginForm { loginForm | password = password }
                        ]
                        []
                    ]
                ]
            , Html.div [ Html.Attributes.class "input-field" ]
                [ button
                ]
            ]
        ]


viewPizzaList : List Pizza -> Model -> Html.Html Msg
viewPizzaList pizzas ({ newPizza } as model) =
    Html.div []
        [ viewHeader model.loginForm
        , Html.h1 []
            [ Html.text "Commandes de pizza : "
            , Html.span [ Html.Attributes.class "badge" ] [ Html.text <| String.fromInt <| List.length pizzas ]
            ]
        , Html.form
            [ Html.Events.onSubmit AddPizza ]
            [ Html.table [ Html.Attributes.style "width" "100%" ]
                [ Html.thead []
                    [ Html.th [] [ Html.text "Date" ]
                    , Html.th []
                        [ Html.text "Prix de la commande"
                        ]
                    , Html.th []
                        [ Html.text "Participant.e.s (ajouter `/2` pour les étudiants)"
                        ]
                    , Html.th [] [ Html.text "Actions" ]
                    ]
                , Html.tbody []
                    (Html.tr []
                        [ Html.td []
                            [ Html.input
                                [ Html.Attributes.type_ "date"
                                , Html.Attributes.name "date"
                                , Html.Events.onInput <| \date -> UpdatePizza { newPizza | date = date }
                                , Html.Attributes.value newPizza.date
                                ]
                                []
                            ]
                        , Html.td []
                            [ Html.input
                                [ Html.Attributes.type_ "text"
                                , Html.Attributes.name "price"
                                , Html.Events.onInput <| \price -> UpdatePizza { newPizza | price = String.toFloat price |> Maybe.withDefault 0 }
                                , Html.Attributes.value <| String.fromFloat newPizza.price
                                ]
                                []
                            ]
                        , Html.td []
                            [ Html.textarea
                                [ Html.Attributes.name "participants"
                                , Html.Events.onInput <| \participants -> UpdatePizza { newPizza | participants = stringToParticipants participants }
                                , Html.Attributes.value <| participantsToString newPizza.participants
                                , Html.Attributes.style "white-space" "pre-wrap"
                                ]
                                []
                            ]
                        , Html.td []
                            [ loadingButton "Add this pizza" <|
                                case model.newPizzaKintoData of
                                    Requested ->
                                        Loading

                                    _ ->
                                        NotLoading
                            ]
                        ]
                        :: (pizzas
                                |> List.map
                                    (\pizza ->
                                        Html.tr []
                                            [ Html.td []
                                                [ Html.text pizza.date
                                                ]
                                            , Html.td [ Html.Attributes.style "white-space" "pre-wrap" ] [ Html.text <| String.fromFloat pizza.price ++ "€" ]
                                            , Html.td [ Html.Attributes.style "white-space" "pre-wrap" ] [ viewParticipants pizza.participants pizza.price ]
                                            , Html.td []
                                                [ loadingActionButton "Remove this pizza" pizza model.deletePizzaList DeletePizza
                                                ]
                                            ]
                                    )
                           )
                    )
                ]
            ]
        ]


viewHeader : { a | serverURL : String, username : String, password : String } -> Html.Html Msg
viewHeader loginForm =
    Html.header
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "position" "fixed"
        , Html.Attributes.style "top" "0"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "right" "0"
        , Html.Attributes.style "padding" ".2em 1.15em"
        , Html.Attributes.style "z-index" "999"
        , Html.Attributes.style "text-align" "left"
        , Html.Attributes.style "align-items" "center"
        ]
        [ viewDownloadData loginForm
        , viewUserInfo loginForm
        ]


viewDownloadData : { a | serverURL : String, username : String, password : String } -> Html.Html Msg
viewDownloadData { serverURL, username, password } =
    let
        url =
            Url.fromString serverURL

        urlWithCredentials =
            url
                |> Maybe.map
                    (\parsedURL ->
                        { parsedURL | host = username ++ ":" ++ password ++ "@" ++ parsedURL.host }
                            |> Url.toString
                            |> (\newURL -> newURL ++ "buckets/default/collections/paie-ta-pizza/records?_sort=-date")
                    )
                |> Maybe.withDefault ""
    in
    Html.a
        [ Html.Attributes.href urlWithCredentials
        , Html.Attributes.download "save.json"
        , Html.Attributes.target "_blank"
        , Html.Attributes.style "color" "#111"
        ]
        [ Html.text "⇓ Download the data" ]


viewUserInfo : { a | serverURL : String, username : String } -> Html.Html Msg
viewUserInfo { serverURL, username } =
    Html.div
        [ Html.Attributes.style "text-align" "right"
        , Html.Attributes.style "flex-grow" "2"
        ]
        [ Html.text "Connected as "
        , Html.strong [] [ Html.text username ]
        , Html.text " on "
        , Html.strong [] [ Html.text serverURL ]
        , Html.text " "
        , Html.button
            [ Html.Events.onClick Logout
            ]
            [ Html.text "logout" ]
        ]


viewGithubLink : Html.Html Msg
viewGithubLink =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        ]
        [ Html.a
            [ Html.Attributes.target "_blank"
            , Html.Attributes.href "https://github.com/magopian/paie-ta-pizza"
            , Html.Attributes.style "display" "block"
            , Html.Attributes.style "text-align" "center"
            , Html.Attributes.style "color" "#000"
            ]
            [ Html.text "Github" ]
        ]


viewErrorList : List String -> Html.Html Msg
viewErrorList errorList =
    Html.ul
        [ Html.Attributes.style "list-style-type" "none"
        , Html.Attributes.style "padding-left" "0"
        , Html.Attributes.style "padding-top" "3em"
        ]
        (errorList
            |> List.indexedMap
                (\index error ->
                    Html.li
                        [ Html.Attributes.class "alert alert-danger"
                        ]
                        [ Html.a
                            [ Html.Attributes.class "float-right"
                            , Html.Attributes.style "font-weight" "normal"
                            , Html.Attributes.style "text-decoration" "none"
                            , Html.Attributes.style "cursor" "pointer"
                            , Html.Events.onClick <| DiscardError index
                            ]
                            [ Html.text "x" ]
                        , Html.text error
                        ]
                )
        )


viewParticipants : List Participant -> Float -> Html.Html Msg
viewParticipants participants price =
    Html.ul
        []
        (participants
            |> List.map
                (\({ name, half, paid } as participant) ->
                    Html.li []
                        [ Html.text name
                        , Html.text " doit : "
                        , price
                            |> computePriceForParticipant participants participant
                            |> String.fromFloat
                            |> Html.text
                        , Html.text <|
                            if half then
                                " (demi portion)"

                            else
                                ""
                        , Html.label []
                            [ Html.text "Payé : "
                            , Html.input
                                [ Html.Attributes.type_ "checkbox"
                                , Html.Attributes.checked paid
                                ]
                                []
                            ]
                        ]
                )
        )


computePriceForParticipant : List Participant -> Participant -> Float -> Float
computePriceForParticipant participants participant price =
    let
        parts : Int
        parts =
            participants
                |> List.map
                    (\{ half } ->
                        if half then
                            1

                        else
                            2
                    )
                |> List.sum
    in
    price
        / toFloat parts
        |> (*) 100
        -- Prepare for rounding to the nearest cent
        |> round
        |> toFloat
        |> (\pricePerPartInCents -> pricePerPartInCents / 100)
        |> (\pricePerPart ->
                if participant.half then
                    pricePerPart

                else
                    pricePerPart * 2
           )


type ButtonState
    = Disabled
    | Loading
    | NotLoading


loadingButton : String -> ButtonState -> Html.Html Msg
loadingButton label buttonState =
    let
        loadingAttrs =
            case buttonState of
                Disabled ->
                    [ Html.Attributes.type_ "submit"
                    , Html.Attributes.disabled True
                    ]

                Loading ->
                    [ Html.Attributes.type_ "submit"
                    , Html.Attributes.class "secondary"
                    , Html.Attributes.disabled True
                    ]

                NotLoading ->
                    [ Html.Attributes.type_ "submit"
                    ]
    in
    Html.button
        loadingAttrs
        [ Html.text label ]


loadingActionButton : String -> Pizza -> List Pizza -> (Pizza -> Msg) -> Html.Html Msg
loadingActionButton label entry updatingPizzaList onClickMessage =
    let
        loadingAttrs =
            if List.member entry updatingPizzaList then
                [ Html.Attributes.style "opacity" "0.5"
                , Html.Attributes.class "button button-danger button-loader"
                ]

            else
                [ Html.Events.onClick <| onClickMessage entry
                , Html.Attributes.class "button button-danger"
                ]
    in
    Html.button
        (Html.Attributes.type_ "button" :: loadingAttrs)
        [ Html.text label ]


stringToParticipants : String -> List Participant
stringToParticipants participants =
    participants
        |> String.split "\n"
        |> List.map
            (\name ->
                { name = name, half = String.endsWith "/2" name, paid = False }
            )


participantsToString : List Participant -> String
participantsToString participants =
    participants
        |> List.map .name
        |> String.join "\n"



---- DECODERS ----


type KintoData a
    = NotRequested
    | Requested
    | Received a
    | Failed Kinto.Error



-- Pizza --


decodePizza : Decode.Decoder Pizza
decodePizza =
    Decode.map5 Pizza
        (Decode.field "id" Decode.string)
        (Decode.field "last_modified" Decode.int)
        (Decode.field "price" Decode.float)
        (Decode.field "participants" (Decode.list decodeParticipant))
        (Decode.field "date" Decode.string)


decodeParticipant : Decode.Decoder Participant
decodeParticipant =
    Decode.map3 Participant
        (Decode.field "name" Decode.string)
        (Decode.field "half" Decode.bool)
        (Decode.field "paid" Decode.bool)


encodeData : Float -> List Participant -> String -> Encode.Value
encodeData price participants date =
    Encode.object
        [ ( "price", Encode.float price )
        , ( "participants", Encode.list encodeParticipant participants )
        , ( "date", Encode.string date )
        ]


encodeParticipant : Participant -> Encode.Value
encodeParticipant { name, half, paid } =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "half", Encode.bool half )
        , ( "paid", Encode.bool paid )
        ]


recordResource : Kinto.Resource Pizza
recordResource =
    Kinto.recordResource "default" "paie-ta-pizza" decodePizza


getPizzaList : Kinto.Client -> Cmd Msg
getPizzaList client =
    client
        |> Kinto.getList recordResource
        |> Kinto.sort [ "-date" ]
        |> Kinto.send PizzasFetched



-- Deleted Pizza --


type alias DeletedPizza =
    { id : String
    , last_modified : Int
    , deleted : Bool
    }


deletedRecordResource : Kinto.Resource DeletedPizza
deletedRecordResource =
    Kinto.recordResource "default" "paie-ta-pizza" decodeDeletedPizza


decodeDeletedPizza : Decode.Decoder DeletedPizza
decodeDeletedPizza =
    Decode.map3 DeletedPizza
        (Decode.field "id" Decode.string)
        (Decode.field "last_modified" Decode.int)
        (Decode.field "deleted" Decode.bool)


deletePizza : Kinto.Client -> Pizza -> Cmd Msg
deletePizza client entry =
    client
        |> Kinto.delete deletedRecordResource entry.id
        |> Kinto.send (PizzaDeleted entry)



-- Session Data --


encodeSessionData : LoginForm -> Encode.Value
encodeSessionData loginForm =
    Encode.object
        [ ( "serverURL", Encode.string loginForm.serverURL )
        , ( "username", Encode.string loginForm.username )
        , ( "password", Encode.string loginForm.password )
        ]


decodeSessionData : Decode.Decoder LoginForm
decodeSessionData =
    Decode.map3
        LoginForm
        (Decode.field "serverURL" Decode.string)
        (Decode.field "username" Decode.string)
        (Decode.field "password" Decode.string)



-- Pizza Data --


encodePizzaData : NewPizza -> Encode.Value
encodePizzaData newPizza =
    Encode.object
        [ ( "date", Encode.string newPizza.date )
        , ( "price", Encode.float newPizza.price )
        , ( "participants", Encode.list encodeParticipant newPizza.participants )
        ]


decodePizzaData : Decode.Decoder NewPizza
decodePizzaData =
    Decode.map3
        NewPizza
        (Decode.field "date" Decode.string)
        (Decode.field "price" Decode.float)
        (Decode.field "participants" (Decode.list decodeParticipant))



---- PORTS ----


port saveSession : Encode.Value -> Cmd msg


port logoutSession : () -> Cmd msg


port savePizza : Encode.Value -> Cmd msg


port saveServerURL : Encode.Value -> Cmd msg



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
