module View.AppView exposing (viewApp)


import Data.Challenge exposing (ChallengeId)
import Data.User exposing (UserId)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Element exposing (..)
import State.AppState exposing (AppState, AuthError, Display(..))
import Update.Msg exposing (Msg(..))
import View.ChallengeDetailsView exposing (challengeDetailsScreen)
import View.ChallengeScreen exposing (challengeScreen)
import View.FeedScreen exposing (feedScreen)
import View.Icons as Icons
import View.LoggedOffScreen exposing (logoffScreen)
import View.LoginScreen exposing (loginFailedScreen, loginScreen)
import View.NewPostScreen exposing (newPostScreen)
import View.NotificationScreen exposing (notificationScreen)
import View.PinnedScreen exposing (pinnedScreen)
import View.SearchScreen exposing (searchScreen)
import View.Style exposing(..)
import View.Theme exposing (background, foreground)
import View.WallScreen exposing (wallScreen)
import View.WizardNewChallengePage exposing (newWizardNewChallengeScreen)
import View.WizardNewFreeTextPage exposing (newWizardNewFreeTextScreen)
import View.WizardNewPollPage exposing (newWizardNewPollScreen)
import View.WizardNewTipPage exposing (newWizardNewTipScreen)


viewApp: AppState -> Html Msg
viewApp state = Element.layout [width fill, height fill]
    (column [ width fill, height fill, padding 5, spacing 10 ]
            [ menuBar state, displayCurrentPage state])

menuBar : AppState -> Element Msg
menuBar state =
    row [ width fill, padding 10, spacing 5, Background.color background, Border.rounded 20 ]
        [el [alignLeft] (backButton state)
        , el [width fill] (if state.display==State.AppState.SearchPage then searchBar state else appTitle)
        , el [alignRight] (wallTab state)
        , el [alignRight] (feedTab state)
        , el [alignRight] (challengeTab state)
        , el [alignRight] (pinnedTab state)
        , el [alignRight] (searchTab state)
        , el [alignRight] (notificationTab state)
        , el [alignRight] (newTab state)
    ]

displayCurrentPage: AppState -> Element Msg
displayCurrentPage state = case state.display of
    State.AppState.LoginPage               -> displayLoginPage state
    State.AppState.LoginFailedPage err     -> displayLoginFailedPage err state
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
    State.AppState.WizardNewFreePostPage   -> displayWizardNewFreeTextPost state
    State.AppState.ChallengePage           -> displayChallenge state
    State.AppState.ChallengeDetailsPage id -> displayChallengeDetails state id


displayLoginPage: AppState -> Element Msg
displayLoginPage = loginScreen

displayLoginFailedPage: AuthError -> AppState -> Element Msg
displayLoginFailedPage = loginFailedScreen

displayLogoffPage: AppState -> Element Msg
displayLogoffPage = logoffScreen

displayWall: AppState -> Element Msg
displayWall = wallScreen

displayUserWall: AppState -> UserId -> Element Msg
displayUserWall state _ = wallScreen state

displayUserWallByPseudo: AppState -> String -> Element Msg
displayUserWallByPseudo state _ = wallScreen state

displayFeed: AppState -> Element Msg
displayFeed = feedScreen

displayEvent: AppState -> Element Msg
displayEvent state = (text "Display Event")

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

displayWizardNewPollPost: AppState -> Element Msg
displayWizardNewPollPost = newWizardNewPollScreen

displayWizardNewEvent: AppState -> Element Msg
displayWizardNewEvent state = (text "Wizard new event")

displayBlocked: AppState -> Element Msg
displayBlocked state = (text "Your user has been blocked !")

displayChallenge: AppState -> Element Msg
displayChallenge = challengeScreen

displayChallengeDetails: AppState -> ChallengeId -> Element Msg
displayChallengeDetails = challengeDetailsScreen


-- We really should have a picture with a proper logo here ...
logo : Element Msg
logo =  text ">> GG"
    |> el [centerX, centerY]
    |> el [width <| px 80
           , height <| px 40
           , Border.width 2
           , Border.rounded 6
           , Background.color background
           , Font.color foreground]

appTitle: Element Msg
appTitle = row [width fill, Background.color background] [
    el [width <| px 20] (horizontalSeparator 1 foreground)
    , text "GreenGen >>"
        |> el [alignLeft]
        |> el [Border.width 1
            , Border.rounded 5
            , padding 5
            , Background.color background
            , Font.color foreground
            , Font.italic
            , Font.family [ Font.typeface "Open Sans", Font.sansSerif]]
    , horizontalSeparator 1 foreground
 ]


backButton : AppState -> Element Msg
backButton _ = tabIconButton (Icons.back Icons.normal) Back

wallTab : AppState -> Element Msg
wallTab state = screenTabIcon state WallPage (Icons.wall Icons.normal) (DisplayPage WallPage)

feedTab : AppState -> Element Msg
feedTab state =
    screenTabIconWithRefresh state FeedPage state.feed.newPostsAvailable (Icons.feed Icons.normal) (DisplayPage FeedPage)

challengeTab : AppState -> Element Msg
challengeTab state =
    screenTabIcon state ChallengePage (Icons.challenge Icons.normal) (DisplayPage ChallengePage)

eventTab : AppState -> Element Msg
eventTab state = screenTabIcon state EventPage (Icons.event Icons.normal) (DisplayPage EventPage)

pinnedTab : AppState -> Element Msg
pinnedTab state = screenTabIcon state PinnedPostPage (Icons.pinned Icons.normal) (DisplayPage PinnedPostPage)

searchTab : AppState -> Element Msg
searchTab state = screenTabIcon state SearchPage (Icons.search Icons.normal) (DisplayPage SearchPage)

notificationTab : AppState -> Element Msg
notificationTab state =
    screenTabIconWithRefresh state NotificationPage state.notifications.unread (Icons.notifications Icons.normal) (DisplayPage NotificationPage)

newTab : AppState -> Element Msg
newTab state = screenTabButton state NewPostPage "+" (DisplayPage NewPostPage)

searchField : AppState -> Element Msg
searchField = searchBar