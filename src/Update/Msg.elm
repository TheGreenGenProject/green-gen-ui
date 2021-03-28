module Update.Msg exposing (..)

import Data.Challenge exposing (Challenge, ChallengeId, ChallengeOutcomeStatus, ChallengeStepStatus)
import Data.Feed exposing (Feed)
import Data.Hash exposing (Hash)
import Data.Hashtag exposing (Hashtag)
import Data.Notification exposing (Notification, NotificationId)
import Data.Page exposing (Page)
import Data.Post exposing (PinnedPost, Post, PostId)
import Data.Schedule exposing (UTCTimestamp)
import Data.User exposing (UserId)
import Data.Wall exposing (Wall)
import Http
import State.AppState exposing (AppState, AuthError, Display(..))
import State.Cache exposing (Cache)
import State.FormState exposing (NewFreeTextWizardState, NewTipWizardState)
import State.SearchState exposing (SearchResult)
import State.UserState exposing (UserInfo, UserState(..))


type Msg =
    Logon Hash Hash
    | EnteringCredentials String String
    | Logoff
    | Back
    | ClockTick
    | SetCurrentTime UTCTimestamp
    | RefreshHashtagTrend Int
    | DisplayPage State.AppState.Display
    | ChangeWallPage Page
    | ChangeFeedPage Page
    | ChangePinnedPage Page
    | ChangeNotificationPage Page
    | ChangeSearchPage Page
    | FollowUser UserId
    | UnfollowUser UserId
    | FollowHashtag Hashtag
    | UnfollowHashtag Hashtag
    | LikePost PostId
    | UnlikePost PostId
    | PinPost PostId
    | UnpinPost PostId
    | EnteringSearch String
    | PerformSearchFromField
    | PerformSearchFromHashtag Hashtag
    | PerformSearchFromUserId UserId
    | MarkNotificationRead NotificationId
    | AcceptChallenge ChallengeId
    | RejectChallenge ChallengeId
    | ReportChallengeStepStatus ChallengeId Int ChallengeStepStatus
    | CheckNotifications
    | CheckFeed
    -- Form
    | FillingNewTipWizard NewTipWizardState
    | PostNewTip
    | FillingNewFreeTextWizard NewFreeTextWizardState
    | PostNewFreeText
    -- Http
    | HttpAuthenticated (Result AuthError UserInfo)
    | HttpLoggedOff (Result Http.Error ())
    | HttpPostLiked (Result Http.Error ())
    | HttpPostUnliked (Result Http.Error ())
    | HttpPostPinned (Result Http.Error ())
    | HttpPostUnpinned (Result Http.Error ())
    | HttpPinnedPostsFetched (Result Http.Error (Cache, List PinnedPost))
    | HttpUserFollowed (Result Http.Error ())
    | HttpUserUnfollowed (Result Http.Error ())
    | HttpHashtagFollowed (Result Http.Error ())
    | HttpHashtagUnfollowed (Result Http.Error ())
    | HttpWallFetched (Result Http.Error (Cache, Wall))
    | HttpUnreadNotificationsFetched (Result Http.Error (Cache, List Notification))
    | HttpMarkNotificationAsRead (Result Http.Error ())
    | HttpNotificationsChecked (Result Http.Error Bool)
    | HttpFeedChecked (Result Http.Error Bool)
    | HttpSearchResultFetched (Result Http.Error (Cache, SearchResult))
    | HttpFeedFetched (Result Http.Error (Cache, Feed))
    | HttpNewTipPosted (Result Http.Error ())
    | HttpNewFreeTextPosted (Result Http.Error ())
    | HttpChallengeDetailsFetched (Result Http.Error (Cache, ChallengeId))
    | HttpChallengeAccepted (Result Http.Error ())
    | HttpChallengeRejected (Result Http.Error ())
    | HttpChallengeStepStatusReported (Result Http.Error ())
    | HttpHashtagTrendRefreshed (Result Http.Error (List (Int, Hashtag)))
    -- Special
    | Batch (List Msg)
    | NoOp
