module Update.Msg exposing (..)

import Data.Challenge exposing (Challenge, ChallengeId, ChallengeOutcomeStatus, ChallengeStepStatus)
import Data.Conversation exposing (Conversation, ConversationPage, Message, MessageId)
import Data.Event exposing (EventId)
import Data.Feed exposing (Feed)
import Data.Hash exposing (Hash)
import Data.Hashtag exposing (Hashtag)
import Data.Notification exposing (Notification, NotificationId)
import Data.Page exposing (Page)
import Data.Pinned exposing (Pinned)
import Data.Poll exposing (PollId, PollOption)
import Data.Post exposing (PinnedPost, Post, PostId)
import Data.Schedule exposing (UTCTimestamp)
import Data.User exposing (UserId)
import Data.Wall exposing (Wall)
import Element exposing (Device, DeviceClass)
import Http
import State.AppState exposing (AppState, AuthError, Display(..))
import State.Cache exposing (Cache)
import State.ChallengeState exposing (ChallengePagedTab, ChallengeTab)
import State.EventDetailsState exposing (EventDetailsPagedTab, EventDetailsTab)
import State.EventState exposing (EventPagedTab, EventTab)
import State.FormState exposing (NewChallengeWizardState, NewEventWizardState, NewFreeTextWizardState, NewPollWizardState, NewRepostWizardState, NewTipWizardState, RegistrationFormState)
import State.NotificationState exposing (NotificationPage, NotificationTab)
import State.SearchState exposing (PostSearchResult, PostType, SearchFilter, UserSearchResult)
import State.UserState exposing (UserInfo, UserState(..))

type Msg =
    Logon Hash Hash
    | EnteringCredentials String String
    | Logoff
    | Back
    | ClockTick
    | SetCurrentTime UTCTimestamp
    | SetWindowSize Int Int
    | IdentifyDevice Device
    | RefreshHashtagTrend Int
    | DisplayPage State.AppState.Display
    | ChangeWallPage Page
    | RefreshWall
    | ChangeFeedPage Page
    | GenerateWelcomeFeed
    | RefreshFeed
    | ChangePinnedPage Page
    | RefreshPinnedPosts
    | ChangeNotificationTab NotificationTab
    | ChangeNotificationPage Page
    | RefreshNotifications
    | ChangeSearchPostPage Page
    | ChangeSearchUserPage Page
    | ChangeSearchPostTypeFilter PostType
    | ChangeChallengeTab ChallengeTab
    | ChangeChallengePage Page
    | ChangeEventTab EventTab
    | ChangeEventPage Page
    | ChangeEventDetailsTab EventId EventDetailsTab
    | ChangeEventDetailsPage EventId Page
    | RefreshEventDetails EventId
    | FollowUser UserId
    | UnfollowUser UserId
    | FollowHashtag Hashtag
    | UnfollowHashtag Hashtag
    | Repost PostId
    | LikePost PostId
    | UnlikePost PostId
    | PinPost PostId
    | UnpinPost PostId
    | OpenPostConversation PostId
    | ClosePostConversation PostId
    | UpdateNewPostComment PostId String
    | PostNewComment PostId String
    | FlagComment MessageId
    | UnflagComment MessageId
    | LoadMore String Msg
    | LoadMoreOrLess String Msg Msg
    | LoadMoreComment PostId Page
    | EnteringSearch String
    | PerformSearchFromField
    | PerformSearchFromHashtag Hashtag
    | PerformSearchFromUserId UserId
    | MarkNotificationRead NotificationId
    | AcceptChallenge ChallengeId
    | RejectChallenge ChallengeId
    | ReportChallengeStepStatus ChallengeId Int ChallengeStepStatus
    | AnswerPoll PollId PollOption
    | RequestEventParticipation EventId
    | CancelEventParticipation EventId
    | AcceptUserEventParticipation EventId UserId
    | RejectUserEventParticipation EventId UserId
    | CancelEvent EventId
    | CheckNotifications
    | CheckFeed
    -- Form
    | FillingRegistrationForm RegistrationFormState
    | CheckPseudoAvailability (Maybe String)
    | RegisterNewAccount
    | VerifyAccount
    | FillingNewTipWizard NewTipWizardState
    | PostNewTip
    | PostNewRepost
    | FillingNewFreeTextWizard NewFreeTextWizardState
    | PostNewFreeText
    | FillingNewChallengeWizard NewChallengeWizardState
    | PostNewChallenge NewChallengeWizardState
    | FillingNewEventWizard NewEventWizardState
    | PostNewEvent NewEventWizardState
    | FillingNewPollWizard NewPollWizardState
    | PostNewPoll NewPollWizardState
    -- Http feedback
    | HttpAuthenticated (Result AuthError UserInfo)
    | HttpLoggedOff (Result Http.Error ())
    | HttpNewAccountRegistered (Result Http.Error ())
    | HttpNewAccountVerified (Result Http.Error ())
    | HttpPostLiked (Result Http.Error ())
    | HttpPostUnliked (Result Http.Error ())
    | HttpPostPinned (Result Http.Error ())
    | HttpPostUnpinned (Result Http.Error ())
    | HttpPinnedPostsFetched (Result Http.Error (Cache, Pinned))
    | HttpUserFollowed (Result Http.Error ())
    | HttpUserUnfollowed (Result Http.Error ())
    | HttpHashtagFollowed (Result Http.Error ())
    | HttpHashtagUnfollowed (Result Http.Error ())
    | HttpWallFetched (Result Http.Error (Cache, Wall))
    | HttpNotificationsFetched (Result Http.Error (Cache, NotificationPage))
    | HttpMarkNotificationAsRead (Result Http.Error ())
    | HttpNotificationsChecked (Result Http.Error Bool)
    | HttpFeedChecked (Result Http.Error Bool)
    | HttpPostSearchResultFetched (Result Http.Error (Cache, PostSearchResult))
    | HttpUserSearchResultFetched (Result Http.Error (Cache, UserSearchResult))
    | HttpFeedFetched (Result Http.Error (Cache, Feed))
    | HttpPseudoAvailabilityChecked (Result Http.Error (String, Bool))
    | HttpNewTipPosted (Result Http.Error ())
    | HttpNewRepostPosted (Result Http.Error ())
    | HttpNewFreeTextPosted (Result Http.Error ())
    | HttpNewChallengePosted (Result Http.Error ())
    | HttpChallengePostsFetched (Result Http.Error (Cache, ChallengePagedTab, List PostId))
    | HttpChallengeDetailsFetched (Result Http.Error (Cache, ChallengeId))
    | HttpChallengeAccepted (Result Http.Error ())
    | HttpChallengeRejected (Result Http.Error ())
    | HttpChallengeStepStatusReported (Result Http.Error ())
    | HttpHashtagTrendRefreshed (Result Http.Error (List (Int, Hashtag)))
    | HttpPollAnswered (Result Http.Error ())
    | HttpNewPollPosted (Result Http.Error ())
    | HttpEventParticipationRequested (Result Http.Error EventId)
    | HttpEventParticipationRequestCancelled (Result Http.Error EventId)
    | HttpEventParticipationAccepted (Result Http.Error EventId)
    | HttpEventParticipationRejected (Result Http.Error EventId)
    | HttpEventCancelled (Result Http.Error EventId)
    | HttpEventPostsFetched (Result Http.Error (Cache, EventPagedTab, List PostId))
    | HttpEventDetailsFetched (Result Http.Error (Cache, EventId))
    | HttpEventPendingRequestsFetched (Result Http.Error (Cache, EventDetailsPagedTab, List UserId))
    | HttpEventParticipantsFetched (Result Http.Error (Cache, EventDetailsPagedTab, List UserId))
    | HttpNewEventPosted (Result Http.Error ())
    | HttpConversationPageFetched (Result Http.Error (Cache, ConversationPage))
    | HttpNewCommentPosted (Result Http.Error PostId)
    | HttpCommentFlagged (Result Http.Error ())
    | HttpCommentUnflagged (Result Http.Error ())
    -- Special
    | Batch (List Msg)
    | NoOp
