module View.WizardNewEventPage exposing (newWizardNewEventScreen)

import Data.Hashtag exposing (Hashtag(..))
import Data.Location exposing (Country(..), Latitude(..), Location(..), Longitude(..), ZipCode(..))
import Data.Schedule as Schedule
import Data.Url exposing (Url(..))
import Element exposing (Element, alignLeft, alignRight, centerX, column, el, fill, height, maximum, padding, paragraph, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (labelHidden)
import State.AppState exposing (AppState)
import State.Cache as Cache exposing (Cache)
import State.FormState exposing (Audience(..), FormState, LocationType(..), NewEventWizardState, ReportPeriod(..))
import Update.Msg exposing (Msg(..))
import Utils.DateUtils as DateUtils exposing (toLocalDate, toLocalTime)
import Utils.MaybeUtils as MaybeUtils
import Utils.TextUtils as TextUtils
import View.Icons as Icons
import View.Style exposing (dateSpinner, hashtagStyle, intSpinner, leftGap, options, placeholderStyle, size, timeSpinner, titledTextStyle, userStyle)
import View.Theme as Theme exposing (foreground, textFieldBackground, textFieldForeground)


newWizardNewEventScreen: AppState -> Element Msg
newWizardNewEventScreen state = column [
    centerX
    , width  <| maximum 600 fill
    , height <| maximum 700 fill
    , spacing 10
    , padding 10
    , Border.rounded 20 ]
   (form state)

form : AppState -> List (Element Msg)
form state =
    let wizardStateMaybeDates = state.forms.newEventWizard
        startDate = wizardStateMaybeDates.start
                |> Maybe.withDefault (state.timestamp |> DateUtils.plusDays 7)
        endDate = wizardStateMaybeDates.end
                |> Maybe.withDefault (startDate |> DateUtils.plusHours 3)
                |> DateUtils.max (DateUtils.plusHours 1 startDate)
        wizardState = {wizardStateMaybeDates| start = Just startDate, end = Just endDate }
        maxParticipants = wizardState.maxParticipants
        posting = wizardState.posting
        isCorrect = check wizardState |> hasError |> not
        postButtonColor = if isCorrect then Theme.enabledButton else Theme.disabledButton
    in
    [ (row [padding 5, spacing 10, width fill] [
        el [Font.color Theme.enabledButton] (Icons.event Icons.large)
        , titledTextStyle "Create a new Event" wizardDescription 10])
    , row [spacing 10] [
        Icons.calendar Icons.normal
            |> el [Font.color Theme.background]
        , "Starts on" |> text |> el [Font.size 12, width <| px 55]
        , dateSpinner (startDate)
            (\localDate -> FillingNewEventWizard {wizardState|
                start = DateUtils.toUTCTimestampFromTime localDate (toLocalTime startDate) }
                    |> Just)
        , "at" |> text |> el [Font.size 12]
        , timeSpinner (startDate)
            (\localTime -> FillingNewEventWizard {wizardState|
                start = localTime
                    |> DateUtils.toUTCTimestampFromTime (toLocalDate startDate) }
                    |> Just)
     ]
     , row [spacing 10] [
        Icons.calendar Icons.normal
            |> el [Font.color Theme.background]
        , "Ends on" |> text |> el [Font.size 12, width <| px 55]
        , dateSpinner (endDate)
             (\localDate -> FillingNewEventWizard {wizardState|
                end = DateUtils.toUTCTimestampFromTime localDate (toLocalTime endDate) }
                    |> Just)
        , "at" |> text |> el [Font.size 12]
        , timeSpinner (endDate)
            (\localTime -> FillingNewEventWizard {wizardState|
                end = localTime
                    |> DateUtils.toUTCTimestampFromTime (toLocalDate endDate) }
                    |> Just)
      ]
     , row [spacing 10] [
        Icons.user Icons.normal
            |> el [Font.color Theme.background]
        , "Participants" |> text |> el [Font.size 12, width <| px 55]
        , intSpinner 0 100 1 maxParticipants
             (\n -> FillingNewEventWizard {wizardState|
                maxParticipants = n } |> Just)
      ]
      , row [spacing 10] [
            Icons.location Icons.normal
                |> el [Font.color Theme.background]
            , "Location" |> text |> el [Font.size 12, width <| px 55]
            , options [("Online", LocatedOnline), ("Map link", MapLink), ("Address", Physical)]
                (wizardState.selectedLocationType)
                (\opt -> FillingNewEventWizard {wizardState| selectedLocationType = opt})
      ]
    , renderLocationForm wizardState |> leftGap 55
    , (Input.multiline [width fill
        , height <| maximum 400 fill
        , Font.color Theme.textFieldForeground
        , Background.color Theme.textFieldBackground] {
        onChange = (updateDescription state.forms.newEventWizard)
        , text = (state.forms.newEventWizard.description |> Maybe.withDefault "")
        , placeholder = placeholderStyle "Enter your Event description. Be precise and concise !"
        , label = labelHidden "Event description"
        , spellcheck = True
    })
    , makeHashtagBar state.forms.newEventWizard
    , makeUserBar state.cache state.forms.newEventWizard
    , (Input.button [alignRight, Border.width 2, Border.rounded 5, padding 5, Font.color postButtonColor] {
        onPress = if isCorrect then Just (PostNewEvent wizardState) else Nothing
        , label = (text (if posting then "Posting ..." else "Post your event !"))
    })
 ]

renderLocationForm: NewEventWizardState -> Element Msg
renderLocationForm state = case state.selectedLocationType of
    LocatedOnline  -> renderOnLineLocationForm state
    MapLink        -> renderMapUrlLocationForm state
    Physical       -> renderAddressLocationForm state

renderOnLineLocationForm: NewEventWizardState -> Element Msg
renderOnLineLocationForm state =
    let loc = case state.location of
                Just (Online (Url url)) -> url
                _                       -> ""
    in
    column [width fill, spacing 3] [
        titledTextStyle "Event URL" "Enter the URL for your event" 10
        , (Input.text [width <| px 450, Background.color textFieldBackground, Font.color textFieldForeground] {
            onChange = (\txt -> updateLocation state (Online (Url txt)))
            , text = loc
            , placeholder = placeholderStyle "Url for the event"
            , label = labelHidden "Event online location"
        }) |> size 10
    ]

renderMapUrlLocationForm: NewEventWizardState -> Element Msg
renderMapUrlLocationForm state =
    let loc = case state.location of
                Just (MapUrl (Url url)) -> url
                _                       -> ""
    in
    column [width fill, spacing 3] [
        titledTextStyle "Map link" "Enter the google map or open street map URL " 10
        , (Input.text [width <| px 450, Background.color textFieldBackground, Font.color textFieldForeground] {
            onChange = (\txt -> updateLocation state (Online (Url txt)))
            , text = loc
            , placeholder = placeholderStyle "Map Url for the event"
            , label = labelHidden "Event map link"
        }) |> size 10
    ]

renderAddressLocationForm: NewEventWizardState -> Element Msg
renderAddressLocationForm state =
    let (street, zip, country) = case state.location of
                                 Just (Address strt zp (Country cntry)) -> (strt, zp, cntry)
                                 _                                      -> ( Nothing, Nothing, "World")
        zipStr = case zip of
            Just (ZipCode zp) -> zp
            Nothing           -> ""
    in
    column [width fill, spacing 3] [
        titledTextStyle "Event Address" "Enter the address of your event, with street, zip/post code and country" 10
        , row [width fill, spacing 5] [
            "Street" |> text |> el [width <| px 70]
            ,(Input.text [width <| px 450, Background.color textFieldBackground, Font.color textFieldForeground] {
            onChange = (\txt -> updateLocation state (Address (MaybeUtils.maybeString txt) zip (Country country)))
            , text = street |> Maybe.withDefault ""
            , placeholder = placeholderStyle "Full street name"
            , label = labelHidden "Event street"
        })] |> size 10
        , row [width fill, spacing 5] [
            "Zip/Post code" |> text |> el [width <| px 70]
            ,(Input.text [width fill, height fill, Background.color textFieldBackground, Font.color textFieldForeground] {
            onChange = (\txt -> updateLocation state (Address street (MaybeUtils.maybeString txt |> Maybe.map ZipCode) (Country country)))
            , text = zipStr
            , placeholder = placeholderStyle "Zip code"
            , label = labelHidden "Event zip code"
             })
             , "Country" |> text
             ,(Input.text [width fill, height fill, Background.color textFieldBackground, Font.color textFieldForeground] {
                         onChange =  (\txt -> updateLocation state (Address street zip (Country txt)))
                         , text = country
                         , placeholder = placeholderStyle "Country"
                         , label = labelHidden "Event street"
             })] |> size 10
    ]


makeHashtagBar: NewEventWizardState -> Element Msg
makeHashtagBar state = paragraph [alignLeft
        , spacing 10
        , Font.color foreground
        , Font.italic
        , Font.size 12] [row [spacing 5]
    (state.description |> Maybe.withDefault ""
      |> TextUtils.hashtagsFrom
      |> List.sortBy (\(Hashtag x) -> x)
      |> List.map hashtagStyle)]

makeUserBar: Cache -> NewEventWizardState -> Element Msg
makeUserBar cache state = paragraph [alignLeft
        , spacing 10
        , Font.color foreground
        , Font.italic
        , Font.size 12] [row [spacing 5]
    (state.description |> Maybe.withDefault ""
      |> TextUtils.userPseudosFrom
      |> List.map (\pseudo -> userStyle pseudo (Cache.getUserByPseudo cache pseudo)))]

updateDescription: NewEventWizardState -> String -> Msg
updateDescription state desc = FillingNewEventWizard {state| description = Just desc }

updateLocation: NewEventWizardState -> Location -> Msg
updateLocation state location = FillingNewEventWizard {state| location = Just location }


check: NewEventWizardState -> Result String ()
check state = if state |> getDescription |> String.isEmpty then Err "Please describe the event precisely"
    else if state |> hasHashtags |> not then Err "Enter hashtags to help others to find your challenge"
    else if state.location |> MaybeUtils.isEmpty then Err "You need to enter a location for your event"
    else if state.start |> MaybeUtils.isEmpty then Err "You need a start date/time for your event"
    else if state.end |> MaybeUtils.isEmpty then Err "You need an end date/time for your event"
    else if not (checkDates state) then Err "The event should end after the start date"
    else Ok ()

checkDates: NewEventWizardState -> Bool
checkDates state = case (state.end, state.start) of
    (Just a , Just b) -> Schedule.after a b
    _                 -> False


getDescription: NewEventWizardState -> String
getDescription state = state.description |> Maybe.withDefault ""

hasHashtags: NewEventWizardState -> Bool
hasHashtags state = state |> getDescription |> TextUtils.hashtagsFrom |> List.isEmpty |> not

hasError: Result err ok -> Bool
hasError res = case res of
    Err _ -> True
    Ok _  -> False

wizardDescription =
    """You can use the # symbol in your text to provide hashtags, the @ symbol to reference a user pseudo.
    Your post must contain at least one hashtag."""
