module Update.Logic exposing (update)

import Data.Challenge exposing (ChallengeOutcomeStatus(..))
import Data.Page as Page
import Platform.Cmd exposing (Cmd(..), none)
import Query.Authentication exposing (logon)
import Query.Challenge exposing (acceptChallenge, fetchChallengeDetails, rejectChallenge, reportStepStatus)
import Query.Clock as Clock
import Query.Feed exposing (fetchFeed, hasNewPosts, scheduleFeedCheck)
import Query.Following exposing (follow, unfollow)
import Query.FreeText exposing (postFreeText)
import Query.Hashtag exposing (refreshHashtagTrend)
import Query.Like exposing (like, unlike)
import Query.Notification exposing (fetchNotifications, hasUnreadNotifications, markAsRead, scheduleNotificationCheck)
import Query.Pinned exposing (fetchPinnedPosts, pin, unpin)
import Query.QueryUtils exposing (errorToString)
import Query.Search exposing (performSearch)
import Query.Tip exposing (postTip)
import Query.Wall exposing (fetchUserWall, fetchWall)
import State.AppState as AppState exposing (AppState, Display(..), isUserLoggedIn)
import State.Cache as Cache
import State.FeedState as FeedState
import State.FormState as FormState exposing (clearNewFreeTextWizardState, clearNewTipWizardState)
import State.NotificationState as NotificationState
import State.PinnedState as PinnedState
import State.SearchState as SearchState
import State.UserState exposing (UserInfo, UserState(..))
import State.WallState as WallState
import Update.Msg exposing(..)


update: Msg -> AppState -> (AppState, Cmd Msg)
update msg state = case msg of
    ----------------------
    --- Logon / Logoff ---
    ----------------------
    Logoff -> {state | display = LoggedOffPage, user = NotLogged }
        |> nocmd
    Logon user pw -> state |> cmd (logon user pw)

    -----------------
    --- Commands ---
    -----------------
    Back ->
        case state.previous of
            [ ]          -> state |> nocmd
            [_]          -> state |> nocmd
            _ :: x :: xs -> update (DisplayPage x) {state| previous = xs }
    ClockTick -> state
        |> cmd Clock.currentTime
    SetCurrentTime now ->
        {state| timestamp = now }
        |> cmd Clock.scheduleClockTick
    ChangeWallPage page ->
        update (DisplayPage WallPage) {state| wall = WallState.moveToPage state.wall page }
    ChangeFeedPage page ->
        update (DisplayPage FeedPage) {state| feed = FeedState.moveToPage state.feed page }
    ChangePinnedPage page ->
        update (DisplayPage PinnedPostPage) {state| pinned = PinnedState.moveToPage state.pinned page }
    ChangeNotificationPage page ->
        update (DisplayPage NotificationPage) {state| notifications = NotificationState.moveToPage state.notifications page }
    ChangeSearchPage page ->
         update (DisplayPage SearchPage) {state| search = SearchState.moveToPage state.search page }
    DisplayPage page  -> {state |
        display = if (isUserLoggedIn state) then page else LoginPage,
        previous = (if (isUserLoggedIn state) then [page] else []) ++ state.previous }
        |> cmd (loadPageContent state page)
    FollowUser followee ->  {state| cache = Cache.addFollowing state.cache followee }
        |> ifLogged (\user -> follow user followee)
    UnfollowUser followee -> {state| cache = Cache.removeFollowing state.cache followee }
        |> ifLogged (\user -> unfollow user followee)
    LikePost postId -> {state| cache = Cache.addLike state.cache postId }
        |> ifLogged (\user -> like state.cache user postId)
    UnlikePost postId -> {state| cache = Cache.removeLike state.cache postId }
        |> ifLogged (\user -> unlike state.cache user postId)
    PinPost postId -> {state| cache = Cache.addPinned state.cache postId }
        |> ifLogged (\user -> pin user postId)
    UnpinPost postId -> {state| cache = Cache.removePinned state.cache postId }
        |> ifLogged (\user -> unpin user postId)
    PerformSearchFromField ->
        update (DisplayPage SearchPage) {state| search = SearchState.applyInput state.search }
    PerformSearchFromHashtag hashtag ->
        update (DisplayPage SearchPage) {state| search = SearchState.fromHashtags state.search [hashtag] }
    CheckNotifications -> state
        |> ifLogged (\user -> hasUnreadNotifications user)
    CheckFeed -> state
        |> ifLogged (\user -> hasNewPosts user state.feed)
    RefreshHashtagTrend n -> state
        |> ifLogged (\user -> refreshHashtagTrend user)
    MarkNotificationRead notifId -> {state |
        display = if (isUserLoggedIn state) then NotificationPage else LoginPage,
        notifications = NotificationState.markAsRead state.notifications notifId }
        |> ifLogged (\user -> markAsRead user notifId)
    AcceptChallenge challengeId -> {state| cache = Cache.addChallengeOutcomeStatus state.cache challengeId Accepted}
        |> ifLogged (\user -> acceptChallenge state.cache user challengeId)
    RejectChallenge challengeId -> {state| cache = Cache.addChallengeOutcomeStatus state.cache challengeId Rejected}
        |> ifLogged (\user -> rejectChallenge state.cache user challengeId)
    ReportChallengeStepStatus challengeId step status ->  {state|
        cache = Cache.updateChallengeStepReports state.cache challengeId {step = step, status = status}}
        |> ifLogged (\user -> reportStepStatus user challengeId step status)
    -----------------------
    --- Form processing ---
    -----------------------
    EnteringCredentials username password -> {state |
        user = LoggingIn { email = username, password = password } }
        |> nocmd
    EnteringSearch content -> {state |
        search = SearchState.input state.search content }
        |> nocmd
    FillingNewTipWizard tipState -> { state |
        forms = FormState.updateNewTipWizardState state.forms tipState }
        |> nocmd
    PostNewTip -> { state |
        forms = FormState.postingNewTip state.forms }
        |> ifLogged (\user -> postTip user state.forms.newTipWizard)
    FillingNewFreeTextWizard freeTextState -> { state |
        forms = FormState.updateNewFreeTextWizardState state.forms freeTextState }
        |> nocmd
    PostNewFreeText -> { state |
        forms = FormState.postingNewFreeText state.forms }
        |> ifLogged (\user -> postFreeText user state.forms.newFreeTextWizard)
    ---------------------
    --- Http commands ---
    ---------------------
    HttpAuthenticated (Ok userInfo)    ->
        -- FIXME we need to execute several commands on a successful authentication
        -- following code is probably subject to race conditions
        let (state1, com1) = update (DisplayPage FeedPage) {state | user = LoggedIn userInfo }
            (state2, com2) = state1 |> cmd Clock.scheduleClockTick
            (state3, com3) = update CheckNotifications state2
            (state4, com4) = update CheckFeed state3
            (state5, com5) = update (RefreshHashtagTrend 25) state4
        in state5 |> allOf [com1, com2, com3, com4, com5]
    HttpAuthenticated (Err err)        -> {state | display = (LoginFailedPage err), user = NotLogged }
        |> nocmd
    HttpHashtagTrendRefreshed (Ok trend) -> {state |
        cache = Cache.updateHashtagTrend state.cache trend }
        |> nocmd
    HttpHashtagTrendRefreshed (Err err) -> Debug.log ("Error while retrieving hashtag trend " ++ (errorToString err) )
        state |> nocmd

    HttpPostLiked _                    -> state |> nocmd
    HttpPostUnliked _                  -> state |> nocmd
    HttpUserFollowed _                 -> state |> nocmd
    HttpUserUnfollowed _               -> state |> nocmd
    HttpPostPinned _                   -> state |> nocmd
    HttpPostUnpinned _                 -> state |> nocmd
    HttpMarkNotificationAsRead _       -> state |> nocmd

    HttpWallFetched (Ok (cache, wall)) -> {state |
        wall = (WallState.from wall),
        cache = Cache.merge cache state.cache }
        |> nocmd
    HttpWallFetched (Err err)          -> Debug.log ("Error while receiving wall " ++ (errorToString err) )
        {state | display = WallPage, wall = WallState.empty }
        |> nocmd
    HttpFeedFetched (Ok (cache, feed)) -> {state |
        display = FeedPage,
        feed = (FeedState.from feed),
        cache = Cache.merge cache state.cache }
        |> nocmd
    HttpFeedFetched (Err err)          -> Debug.log ("Error while receiving feed " ++ (errorToString err))
        {state | display = FeedPage, feed = FeedState.empty }
        |> nocmd
    HttpFeedChecked (Ok newPosts) -> {state| feed = FeedState.updateNewPostsAvailable state.feed newPosts }
        |> ifLogged (\_ -> scheduleFeedCheck feedDelay)
    HttpFeedChecked (Err err) -> Debug.log ("Error while checking new feed: " ++ (errorToString err)) state
        |> ifLogged (\_ -> scheduleFeedCheck feedDelay)
    -- Post pinning
    HttpPinnedPostsFetched (Ok (cache, pinned)) -> let merged = Cache.merge cache state.cache in
        Debug.log ("Processing " ++ (pinned |> List.length |> String.fromInt) ++ " pinned posts") {state |
            display = PinnedPostPage,
            pinned = PinnedState.from pinned,
            cache = merged }
            |> nocmd
    HttpPinnedPostsFetched (Err err) -> Debug.log ("Error while receiving pinned posts " ++ (errorToString err))
        {state | display = PinnedPostPage, pinned = PinnedState.empty }
        |> nocmd
    HttpSearchResultFetched (Ok (cache, result)) -> {state |
        display = SearchPage,
        search = SearchState.withResults state.search result,
        cache = Cache.merge cache state.cache }
        |> nocmd
    HttpSearchResultFetched (Err err)  -> Debug.log ("Error while receiving search result " ++ (errorToString err))
        {state | display = SearchPage, search = SearchState.empty }
        |> nocmd
    HttpNotificationsChecked (Ok hasUnread) -> {state |
        notifications = NotificationState.updateUnreadStatus state.notifications hasUnread }
        |> ifLogged (\_ -> scheduleNotificationCheck notificationDelay)
    HttpNotificationsChecked (Err err) -> Debug.log ("Error while checking notifications: " ++ (errorToString err)) state
        |> ifLogged (\_ -> scheduleNotificationCheck notificationDelay)
    HttpUnreadNotificationsFetched (Ok (cache, notifs)) -> {state |
        notifications = NotificationState.from state.notifications.currentPage notifs,
        cache = Cache.merge cache state.cache }
        |> nocmd
    HttpUnreadNotificationsFetched (Err err) -> Debug.log ("Error while receiving unread notifications: " ++ (errorToString err))
        state |> nocmd
    HttpNewTipPosted (Ok _) -> {state |
        display = NewPostPage,
        forms = clearNewTipWizardState state.forms}
        |> nocmd
    HttpNewTipPosted (Err err) -> Debug.log ("Error while posting new tip: " ++ (errorToString err))
        {state| forms = FormState.newTipPosted state.forms }
        |> nocmd
    HttpNewFreeTextPosted (Ok _) -> {state |
        display = NewPostPage,
        forms = clearNewFreeTextWizardState state.forms}
        |> nocmd
    HttpNewFreeTextPosted (Err err) -> Debug.log ("Error while posting new free text: " ++ (errorToString err))
        {state| forms = FormState.newFreeTextPosted state.forms }
        |> nocmd
    HttpChallengeDetailsFetched (Ok (cache, _)) -> {state |
        cache = Cache.merge cache state.cache }
        |> nocmd
    HttpChallengeDetailsFetched (Err err) -> Debug.log ("Error while fetching challenge details: " ++ (errorToString err))
        state |> nocmd
    HttpChallengeAccepted _             -> state |> nocmd
    HttpChallengeRejected _             -> state |> nocmd
    HttpChallengeStepStatusReported _   -> state |> nocmd
    HttpLoggedOff _                     -> {state | display = LoggedOffPage, user = NotLogged }
        |> nocmd

    other -> Debug.log ("Unprocessed message " ++ (Debug.toString other)) state
        |> nocmd


-- Helpers

notificationDelay = 30000.0
feedDelay = 30000.0

nocmd: AppState -> (AppState, Cmd Msg)
nocmd state = (state, Cmd.none)

cmd: Cmd Msg -> AppState -> (AppState, Cmd Msg)
cmd command state = (state, command)

allOf: List (Cmd Msg) -> AppState -> (AppState, Cmd Msg)
allOf commands state = (state, Cmd.batch commands)

-- Applies the command only if user is logged in
-- Else cleans up state and present LoginPage again
ifLogged: (UserInfo -> Cmd Msg) -> AppState -> (AppState, Cmd Msg)
ifLogged f state = case state.user of
    LoggedIn user -> state |> cmd (f user)
    _             -> Debug.log "Couldn't process command: user is not logged in !" AppState.empty |> nocmd

-- Loads/Reload a page content if needed and if the user is logged in
loadPageContent: AppState -> Display -> Cmd Msg
loadPageContent state page = case (state.user, page) of
    (LoggedIn user, WallPage)                         -> (fetchWall state.cache user state.wall.currentPage)
    (LoggedIn user, FeedPage)                         -> (fetchFeed state.cache user state.feed.currentPage)
    (LoggedIn user, PinnedPostPage)                   -> (fetchPinnedPosts state.cache user state.pinned.currentPage)
    (LoggedIn user, SearchPage)                       -> (performSearch state.cache user state.search.filter state.search.currentPage)
    (LoggedIn user, NotificationPage)                 -> (fetchNotifications state.cache user state.notifications.currentPage)
    (LoggedIn user, UserPage userId)                  -> (fetchUserWall state.cache user userId state.wall.currentPage)
    (LoggedIn user, ChallengeDetailsPage challengeId) -> (fetchChallengeDetails state.cache user challengeId)
    _                                                 -> none

