module State.AppState exposing (..)

import Data.Challenge exposing (ChallengeId)
import Data.Event exposing (EventId)
import Data.Schedule exposing (UTCTimestamp(..))
import Data.User exposing (UserId)
import Element exposing (Device, DeviceClass(..), Orientation(..))
import Http
import State.Cache as Cache exposing (Cache)
import State.ChallengeState exposing (ChallengeState)
import State.EventDetailsState exposing (EventDetailsState)
import State.EventState exposing (EventState)
import State.FeedState exposing (FeedState)
import State.FormState exposing (FormState)
import State.NotificationState exposing (NotificationState)
import State.PinnedState exposing (PinnedState)
import State.SearchState exposing (SearchState)
import State.UserState exposing (UserState)
import State.WallState exposing (WallState)
import View.Theme as Theme exposing (Theme)
import View.UIStyle as UIStyle exposing (UIStyle)

type AuthError = HttpError Http.Error | AuthenticationFailed

type Display = LoginPage
    | LoginFailedPage AuthError
    | RegistrationPage
    | LoggedOffPage
    | BlockedPage
    | WallPage
    | FeedPage
    | EventPage
    | PinnedPostPage
    | SearchPage
    | UserPage UserId
    | PseudoPage String
    | NotificationPage
    | NewPostPage
    | ChallengePage
    | ChallengeDetailsPage ChallengeId
    | EventDetailsPage EventId
    | WizardNewEventPage
    | WizardNewTipPage
    | WizardRepostPage
    | WizardNewChallengePage
    | WizardNewFreePostPage
    | WizardNewPollPage

type alias WindowSize = {
    width: Int,
    height: Int
 }

type alias AppState = {
    timestamp: UTCTimestamp,
    device: Device,
    windowSize: WindowSize,
    uiStyle: UIStyle,
    display: Display,
    previous: List Display,
    user: UserState,
    wall: WallState,
    feed: FeedState,
    challenge: ChallengeState,
    event: EventState,
    eventDetails: EventDetailsState,
    pinned: PinnedState,
    notifications: NotificationState,
    search: SearchState,
    forms: FormState,
    cache: Cache
 }

empty: AppState
empty = {
    timestamp     = UTC(0),
    device        = { class = Desktop, orientation = Portrait },
    windowSize    = { width = 0, height = 0 },
    uiStyle       = UIStyle.desktop,
    display       = LoginPage,
    previous      = [],
    user          = State.UserState.NotLogged,
    wall          = State.WallState.empty,
    feed          = State.FeedState.empty,
    challenge     = State.ChallengeState.empty,
    event         = State.EventState.empty,
    eventDetails  = State.EventDetailsState.empty,
    pinned        = State.PinnedState.empty,
    notifications = State.NotificationState.empty,
    search        = State.SearchState.empty,
    forms         = State.FormState.empty,
    cache         = Cache.empty
 }

isUserLoggedIn: AppState -> Bool
isUserLoggedIn state = state.user
    |> State.UserState.isUserLoggedIn
