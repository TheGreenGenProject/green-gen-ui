module View.AppView exposing (viewApp)


import Data.Challenge exposing (ChallengeId)
import Data.Event exposing (EventId)
import Data.User exposing (UserId)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Element exposing (..)
import State.AppState exposing (AppState, AuthError, Display(..))
import State.FeedState
import Update.Msg exposing (Msg(..))
import View.ChallengeDetailsView exposing (challengeDetailsScreen)
import View.ChallengeScreen exposing (challengeScreen)
import View.EventDetailsView exposing (eventDetailsScreen)
import View.EventScreen exposing (eventScreen)
import View.UIStyle as UIStyle exposing (UIStyle)
import View.WizardNewEventPage exposing (newWizardNewEventScreen)
import View.WizardRepostPage exposing (newWizardRepostScreen)
import View.FeedScreen exposing (feedScreen)
import View.Icons as Icons
import View.LoggedOffScreen exposing (logoffScreen)
import View.LoginScreen exposing (loginFailedScreen, loginScreen)
import View.NewPostScreen exposing (newPostScreen)
import View.NotificationScreen exposing (notificationScreen)
import View.PinnedScreen exposing (pinnedScreen)
import View.RegistrationScreen exposing (registrationScreen)
import View.SearchScreen exposing (searchScreen)
import View.Style exposing(..)
import View.Theme
import View.WallScreen exposing (wallScreen)
import View.WizardNewChallengePage exposing (newWizardNewChallengeScreen)
import View.WizardNewFreeTextPage exposing (newWizardNewFreeTextScreen)
import View.WizardNewPollPage exposing (newWizardNewPollScreen)
import View.WizardNewTipPage exposing (newWizardNewTipScreen)


viewApp: AppState -> Html Msg
viewApp state = Element.layout [width fill]
    (column [ width fill
              , height fill
              , padding 5
              , spacing 10
              , Font.color state.uiStyle.theme.appForeground
              , Background.color state.uiStyle.theme.appBackground]
            [ menuBar state, (displayCurrentPage state)])

menuBar : AppState -> Element Msg
menuBar state =
    let isUnloggedScreen = List.member state.display [
            State.AppState.LoginPage
            , State.AppState.RegistrationPage
            , State.AppState.LoggedOffPage
            , State.AppState.BlockedPage]
        isSearch = state.display==State.AppState.SearchPage
        isMobile = UIStyle.isMobile state.device
        show el = if isSearch && isMobile then Element.none else el
    in if isUnloggedScreen
       then  el [ width fill, padding 10, Background.color state.uiStyle.theme.background, Border.rounded 20 ] (appTitle state)
       else row [ width fill, padding 10, spacing 5, Background.color state.uiStyle.theme.background, Border.rounded 20 ]
            [el [alignLeft] (backButton state)
            , el [width fill] (if isSearch then searchBar state else appTitle state)
            , el [alignRight] (wallTab state)         |> show
            , el [alignRight] (feedTab state)
            , el [alignRight] (challengeTab state)    |> show
            , el [alignRight] (eventTab state)        |> show
            , el [alignRight] (pinnedTab state)       |> show
            , el [alignRight] (searchTab state)       |> show
            , el [alignRight] (notificationTab state) |> show
            , el [alignRight] (newTab state)          |> show
        ]

displayCurrentPage: AppState -> Element Msg
displayCurrentPage state = case state.display of
    State.AppState.LoginPage               -> displayLoginPage state
    State.AppState.LoginFailedPage err     -> displayLoginFailedPage err state
    State.AppState.RegistrationPage        -> displayRegistrationPage state
    State.AppState.LoggedOffPage           -> displayLogoffPage state
    State.AppState.BlockedPage             -> displayBlocked state
    State.AppState.WallPage                -> displayWall state
    State.AppState.UserPage userId         -> displayUserWall state userId
    State.AppState.PseudoPage pseudo       -> displayUserWallByPseudo state pseudo
    State.AppState.FeedPage                -> displayFeed state
    State.AppState.EventPage               -> displayEvent state
    State.AppState.PinnedPostPage          -> displayPinnedPost state
    State.AppState.SearchPage              -> displaySearch state
    State.AppState.NotificationPage        -> displayNotification state
    State.AppState.NewPostPage             -> displayNewPost state
    State.AppState.WizardNewChallengePage  -> displayWizardNewChallengePost state
    State.AppState.WizardNewEventPage      -> displayWizardNewEvent state
    State.AppState.WizardNewPollPage       -> displayWizardNewPollPost state
    State.AppState.WizardNewTipPage        -> displayWizardNewTipPost state
    State.AppState.WizardRepostPage        -> displayWizardNewRepost state
    State.AppState.WizardNewFreePostPage   -> displayWizardNewFreeTextPost state
    State.AppState.ChallengePage           -> displayChallenge state
    State.AppState.ChallengeDetailsPage id -> displayChallengeDetails state id
    State.AppState.EventDetailsPage id     -> displayEventDetails state id


displayLoginPage: AppState -> Element Msg
displayLoginPage = loginScreen

displayLoginFailedPage: AuthError -> AppState -> Element Msg
displayLoginFailedPage = loginFailedScreen

displayLogoffPage: AppState -> Element Msg
displayLogoffPage = logoffScreen

displayRegistrationPage: AppState -> Element Msg
displayRegistrationPage = registrationScreen

displayWall: AppState -> Element Msg
displayWall = wallScreen

displayUserWall: AppState -> UserId -> Element Msg
displayUserWall state _ = wallScreen state

displayUserWallByPseudo: AppState -> String -> Element Msg
displayUserWallByPseudo state _ = wallScreen state

displayFeed: AppState -> Element Msg
displayFeed = feedScreen

displayEvent: AppState -> Element Msg
displayEvent = eventScreen

displayPinnedPost: AppState -> Element Msg
displayPinnedPost = pinnedScreen

displaySearch: AppState -> Element Msg
displaySearch = searchScreen

displayNotification: AppState -> Element Msg
displayNotification = notificationScreen

displayNewPost: AppState -> Element Msg
displayNewPost = newPostScreen

displayWizardNewFreeTextPost: AppState -> Element Msg
displayWizardNewFreeTextPost = newWizardNewFreeTextScreen

displayWizardNewChallengePost: AppState -> Element Msg
displayWizardNewChallengePost = newWizardNewChallengeScreen

displayWizardNewTipPost: AppState -> Element Msg
displayWizardNewTipPost = newWizardNewTipScreen

displayWizardNewRepost: AppState -> Element Msg
displayWizardNewRepost = newWizardRepostScreen

displayWizardNewPollPost: AppState -> Element Msg
displayWizardNewPollPost = newWizardNewPollScreen

displayWizardNewEvent: AppState -> Element Msg
displayWizardNewEvent = newWizardNewEventScreen

displayBlocked: AppState -> Element Msg
displayBlocked state = (text "Your user has been blocked !")

displayChallenge: AppState -> Element Msg
displayChallenge = challengeScreen

displayChallengeDetails: AppState -> ChallengeId -> Element Msg
displayChallengeDetails = challengeDetailsScreen

displayEventDetails: AppState -> EventId -> Element Msg
displayEventDetails = eventDetailsScreen


-- We really should have a picture with a proper logo here ...
logo : UIStyle -> Element Msg
logo ui =  text ">> GG"
    |> el [centerX, centerY]
    |> el [width <| px 80
           , height <| px 40
           , Border.width 2
           , Border.rounded 6
           , Background.color ui.theme.background
           , Font.color ui.theme.foreground]

appTitle: AppState -> Element Msg
appTitle state =
    if UIStyle.isMobile state.device
    then Element.none
    else let ui = state.uiStyle in
        row [width fill, Background.color ui.theme.background] [
            el [width <| px 20] (horizontalSeparator 1 ui.theme.foreground)
            , text "GreenGen >>"
                |> el [alignLeft]
                |> el [Border.width 1
                    , Border.rounded 5
                    , Border.color ui.theme.appBackground
                    , padding 5
                    , Background.color ui.theme.appBackground
                    , Font.color ui.theme.appTitleForeground
                    , Font.italic
                    , Font.family [ Font.typeface "Open Sans", Font.sansSerif]]
            , horizontalSeparator 1 ui.theme.foreground
         ]


backButton : AppState -> Element Msg
backButton state = tabIconButton state.uiStyle (Icons.back state.uiStyle.normal) Back

wallTab : AppState -> Element Msg
wallTab state = screenTabIcon state WallPage (Icons.wall state.uiStyle.normal) (RefreshWall)

feedTab : AppState -> Element Msg
feedTab state =
    screenTabIconWithRefresh state FeedPage state.feed.newPostsAvailable (Icons.feed state.uiStyle.normal) (RefreshFeed)

challengeTab : AppState -> Element Msg
challengeTab state =
    screenTabIcon state ChallengePage (Icons.challenge state.uiStyle.normal) (DisplayPage ChallengePage)

eventTab : AppState -> Element Msg
eventTab state = screenTabIcon state EventPage (Icons.event state.uiStyle.normal) (DisplayPage EventPage)

pinnedTab : AppState -> Element Msg
pinnedTab state = screenTabIcon state PinnedPostPage (Icons.pinned state.uiStyle.normal) (RefreshPinnedPosts)

searchTab : AppState -> Element Msg
searchTab state = screenTabIcon state SearchPage (Icons.search state.uiStyle.normal) (DisplayPage SearchPage)

notificationTab : AppState -> Element Msg
notificationTab state =
    screenTabIconWithRefresh state NotificationPage state.notifications.unread (Icons.notifications state.uiStyle.normal) (RefreshNotifications)

newTab : AppState -> Element Msg
newTab state = screenTabButton state NewPostPage "+" (DisplayPage NewPostPage)