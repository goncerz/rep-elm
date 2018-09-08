module Main exposing (..)

import Html exposing (..)
import Html.Attributes as HAtt exposing (..)
import Html.Events as HEv exposing (..)
import Keyboard
import Mouse
import WebSocket
import Markdown
import Svg
import Date exposing (Date)
import Json.Decode as Json

import String exposing (..)
import List exposing (..)
import Regex exposing (..)
import Task exposing (..)




main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }




-- MODEL


type alias Person =
  { name : String
  , surname : String
  , birthdate : String
  , phone : String
  , email : String
  }


type alias Model =
  { person : Person
  , msgs : List String
  , records : List Person
  , date : Maybe Date
  }






-- INIT


init : ( Model, Cmd Msg )
init =
  ( Model
    (Person "" "" "" "" "")
    []
    []
    Nothing
  , Cmd.none
  )






-- UPDATE


type Msg
    = InputName String
    | InputSurname String
    | InputBirthdate String
    | InputPhone String
    | InputEmail String
    | Send
    | NewMessage String
    | NoOp
    | RequestDate
    | ReceiveDate Date


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    InputName newInput ->
      let
        oldPerson =
          model.person

        newPerson =
          { oldPerson | name = newInput }
      in
        ( { model | person = newPerson }, Cmd.none )
    
    InputSurname newInput ->
      let
        oldPerson =
          model.person

        newPerson =
          { oldPerson | surname = newInput }
      in
        ( { model | person = newPerson }, Cmd.none )  

    InputBirthdate newInput ->
      let
        oldPerson =
          model.person

        newPerson =
          { oldPerson | birthdate = newInput }
      in
        ( { model | person = newPerson }, Cmd.none )

    InputPhone newInput ->
      let
        oldPerson =
          model.person

        newPerson =
          { oldPerson | phone = newInput }
      in
        ( { model | person = newPerson }, Cmd.none )

    InputEmail newInput ->
      let
        oldPerson =
          model.person

        newPerson =
          { oldPerson | email = newInput }
      in
        ( { model | person = newPerson }, Cmd.none )
    
    Send ->
      if ( ( viewValidationName model == ( div [ style [("color", "green")] ] [ text "OK" ] ) )
         && ( viewValidationSurname model == ( div [ style [("color", "green")] ] [ text "OK" ] ) )
         && ( viewValidationBirthdate model == ( div [ style [("color", "green")] ] [ text "OK" ] ) )
         && ( viewValidationPhone model == ( div [ style [("color", "green")] ] [ text "OK" ] ) )
         && ( viewValidationEmail model == ( div [ style [("color", "green")] ] [ text "OK" ] ) )
         ) then
           ( { model | person = { name = "", surname = "", birthdate = "", phone = "", email = "" } }
           , WebSocket.send echoServer ( changeString ( toString model.person ) )
           )
      else
        ( model, Cmd.none )

    NewMessage str ->
      ( { model | msgs = (str :: model.msgs) }, Cmd.none )

    NoOp ->
      ( model, Cmd.none )

    RequestDate ->
      ( model, Task.perform ReceiveDate Date.now )

    ReceiveDate date ->
      ( { model | date = Just date } , Cmd.none )


changeString : String -> String
changeString strng =
  Regex.replace All ( regex "([}\"{])|(name = )|( surname = )|( birthdate = )|( phone = )|( email = )" ) ( \_ -> "" ) strng






-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ WebSocket.listen echoServer NewMessage
    ]

echoServer : String
echoServer =
  "wss://echo.websocket.org"






-- VIEW


minDate : String
minDate = "1900-01-01"


view : Model -> Html Msg
view model =
  div []
    [ label [] [ text "Imie" ]
    , br [] []
    , input [ onInput InputName, type_ "text", name "imie", placeholder "Imię", attribute "size" "50", attribute "required" "",
      pattern "^[ĄĆĘŁŃÓŚŹŻA-Z][ąćęłńóśźża-z]+", value model.person.name ] []
    , br [] []
    , viewValidationName model
    , label [] [ text "Nazwisko" ]
    , br [] []
    , input [ onInput InputSurname, type_ "text", name "nazw", placeholder "Nazwisko / Nazwisko1-Nazwisko2-Nazwisko3",
      attribute "size" "50", attribute "required" "",
      pattern "^[ĄĆĘŁŃÓŚŹŻA-Z][ąćęłńóśźża-z]+(-[ĄĆĘŁŃÓŚŹŻA-Z][ąćęłńóśźża-z]+)*", value model.person.surname ] []
    , br [] []
    , viewValidationSurname model
    , label [] [ text "Data urodzenia" ]
    , br [] []
    , input [ onInput InputBirthdate, onFocus RequestDate, type_ "date", name "dur", placeholder "",
      HAtt.min minDate, attribute "required" ""
      ] []
    , br [] []
    , viewValidationBirthdate model
    , label [] [ text "Telefon" ]
    , br [] []
    , input [ onInput InputPhone, type_ "text", name "tlf", placeholder "0123456789", attribute "size" "50", attribute "required" "",
      pattern "[0-9]+", value model.person.phone ] []
    , br [] []
    , viewValidationPhone model
    , label [] [ text "Email" ]
    , br [] []
    , input [ onInput InputEmail, type_ "email", name "eml", placeholder "identyfikator@nazwa.domenowa.serwera", attribute "size" "50", attribute "required" "",
      value model.person.email ] []
    , br [] []
    , viewValidationEmail model
    , br [] []
    , button [ onClick Send ] [ text "Zapisz" ]
    , br [] []
    , br [] []
--  , text (getFormattedDate model.date)
    , br [] []
    , br [] []
    , table [ attribute "border" "1" ]
      ( header :: ( viewPers ( List.reverse ( model.msgs ) ) ) )
    ]


viewPers : List String -> List (Html msg)
viewPers lst =
  List.map ( tr [] ) ( List.map ( List.map stringToCell ) ( List.map stringToList lst ) )


stringToList : String ->  List String
stringToList str =
  String.split "," str


stringToCell : String -> Html msg
stringToCell str =
  td [] [ text str ]


header : Html msg
header =
  tr []
  [ th [] [ text "Imię" ]
  , th [] [ text "Nazwisko" ]
  , th [] [ text "Data urodzenia" ]
  , th [] [ text "Telefon" ]
  , th [] [ text "Email" ]
  ]






-- VALIDATIONS


{--}
viewValidationName : Model -> Html msg
viewValidationName model =
  let
    (color, message) =
      if String.isEmpty model.person.name then
        ("black", "")
      else if not (Regex.contains (regex "^[ĄĆĘŁŃÓŚŹŻA-Z]") model.person.name) then
        ("red", "Imię musi zaczynać się od wielkiej litery.")
      else if String.length model.person.name < 2 then
        ("red", "Imię musi składać się z co najmniej dwóch liter.")
      else if Regex.contains (regex "[^ĄĆĘŁŃÓŚŹŻąćęłńóśźżA-Za-z]") model.person.name then
        ("red", "Imię może zawierać tylko litery.")
      else if (Regex.contains (regex "(^[ĄĆĘŁŃÓŚŹŻA-Z])([ąćęłńóśźża-z]*[ĄĆĘŁŃÓŚŹŻA-Z])+") model.person.name) then
        ("red", "Tylko pierwsza litera imienia może być wielka.")
      else
        ("green", "OK")
  in
    div [ style [("color", color)] ] [ text message ]
--}


{--}
viewValidationSurname : Model -> Html msg
viewValidationSurname model =
  let
    (color, message) =
      if String.isEmpty model.person.surname then
        ("black", "")
      else if not (Regex.contains (regex "^[ĄĆĘŁŃÓŚŹŻA-Z]") model.person.surname) then
        ("red", "Nazwisko musi zaczynać się od wielkiej litery.")
      else if String.length model.person.surname < 2 then
        ("red", "Nazwisko musi składać się z co najmniej dwóch liter.")
      else if Regex.contains (regex "[^-ĄĆĘŁŃÓŚŹŻąćęłńóśźżA-Za-z]") model.person.surname then
        ("red", "Nazwisko może zawierać tylko litery i znak pauzy.")
      else if (Regex.contains (regex "-$") model.person.surname) then
        ("red", "Nazwisko nie może kończyć się znakiem pauzy.")
      else if (Regex.contains (regex "[-]{2,}") model.person.surname) then
        ("red", "W nazwisku znak pauzy nie może występować dwa lub więcej razy z kolei.")
      else if (Regex.contains (regex "[ĄĆĘŁŃÓŚŹŻA-Z]$") model.person.surname) then
        ("red", "Nazwisko nie może kończyć się wielką literą.")
      else if (Regex.contains (regex "-[ąćęłńóśźża-z]") model.person.surname) then
        ("red", "Każdy człon nazwiska musi zaczynać się od wielkiej litery.")
      else
        ("green", "OK")
  in
    div [ style [("color", color)] ] [ text message ]
--}


{--}
viewValidationBirthdate : Model -> Html msg
viewValidationBirthdate model =
  let
    (color, message) =
      if String.isEmpty model.person.birthdate then
        ("red", "Nie wprowadzono daty urodzenia.")
      else if model.person.birthdate > (getFormattedDate model.date) then
        ("red", "Data urodzenia nie może być datą przyszłą.")
      else if model.person.birthdate < minDate then
        ("red", "Data urodzenia nie może być datą sprzed " ++ minDate ++ ".")
      else
        ("green", "OK")
  in
    div [ style [("color", color)] ] [ text message ]
--}


getFormattedDate : Maybe Date -> String
getFormattedDate date =
  case date of
    Just d ->
      toString (Date.year d) ++ "-" ++ formatMonth d ++ "-" ++ formatDay d -- toString d

    Nothing ->
      ""


formatMonth : Date -> String
formatMonth d =
  if toString (Date.month d) == "Jan" then
    "01"
  else if toString (Date.month d) == "Feb" then
    "02"
  else if toString (Date.month d) == "Mar" then
    "03"
  else if toString (Date.month d) == "Apr" then
    "04"
  else if toString (Date.month d) == "May" then
    "05"
  else if toString (Date.month d) == "Jun" then
    "06"
  else if toString (Date.month d) == "Jul" then
    "07"
  else if toString (Date.month d) == "Aug" then
    "08"
  else if toString (Date.month d) == "Sep" then
    "09"
  else if toString (Date.month d) == "Oct" then
    "10"
  else if toString (Date.month d) == "Nov" then
    "11"
  else if toString (Date.month d) == "Dec" then
    "12"
  else
    ""


formatDay : Date -> String
formatDay d =
  if (Date.day d) < 10 then
    "0" ++ (toString (Date.day d))
  else
    toString (Date.day d)


{--}
viewValidationPhone : Model -> Html msg
viewValidationPhone model =
  let
    (color, message) =
      if String.isEmpty model.person.phone then
        ("black", "")
      else if Regex.contains (regex "[^0-9]") model.person.phone then
        ("red", "Telefon musi składać się tylko z cyfr.")
      else
        ("green", "OK")
  in
    div [ style [("color", color)] ] [ text message ]
--}


{--}
viewValidationEmail : Model -> Html msg
viewValidationEmail model =
  let
    (color, message) =
      if String.isEmpty model.person.email then
        ("black", "")
      else if not (Regex.contains (regex "^[_A-Za-z0-9-]+([._A-Za-z0-9-])*@[A-Za-z0-9]+(.[A-Za-z0-9-]{1,})*.([A-Za-z]{2,})$") model.person.email) then
        ("red", "Nieprawidłowy adres email.")
      else if not (List.length (String.split "@" model.person.email) == 2) then
        ("red", "Adres email musi zawierać tylko jeden znak @ .")
      else if not ( String.contains "." ( String.concat ( drop 1 ( String.split "@" model.person.email ) ) ) ) then
        ("red", "Nazwa domenowa serwera musi zawierać kropkę.")
      else if String.length model.person.email > 255 then
        ("red", "Adres email może składać się maksymalnie z 255 znaków.")
      else
        ("green", "OK")
  in
    div [ style [("color", color)] ] [ text message ]
--}
