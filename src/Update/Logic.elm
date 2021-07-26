module Update.Logic exposing (update)

import Data.Challenge exposing (ChallengeOutcomeStatus(..))
import Data.Page as Page exposing (Page(..))
import Platform.Cmd exposing (Cmd(..), none)
import Query.Authentication exposing (logon)
import Query.Challenge exposing (acceptChallenge, fetchChallengeDetails, fetchUserChallengePosts, postChallenge, rejectChallenge, reportStepStatus)
import Query.Clock as Clock
import Query.Conversation exposing (fetchConversation, flagComment, postComment, unflagComment)
import Query.Feed exposing (fetchFeed, hasNewPosts, scheduleFeedCheck)
import Query.Following exposing (followHashtag, followUser, unfollowHashtag, unfollowUser)
import Query.FreeText exposing (postFreeText)
import Query.Hashtag exposing (refreshHashtagTrend)
import Query.Like exposing (like, unlike)
import Query.Notification exposing (fetchNotifications, hasUnreadNotifications, markAsRead, scheduleNotificationCheck)
import Query.Pinned exposing (fetchPinnedPosts, pin, unpin)
import Query.Poll exposing (answerPollOption, postPoll)
import Query.QueryUtils exposing (errorToString)
import Query.Registration exposing (checkPseudoAvailability, register, verifyAccount)
import Query.Search exposing (performSearch)
import Query.Tip exposing (postTip)
import Query.Wall exposing (fetchUserWall, fetchWall, fetchWallByPseudo)
import State.AppState as AppState exposing (AppState, Display(..), isUserLoggedIn)
import State.Cache as Cache exposing (simulatePollAnswer)
import State.ChallengeState as ChallengeState
import State.FeedState as FeedState
import State.FormState as FormState exposing (clearNewChallengeWizardState, clearNewFreeTextWizardState, clearNewPollWizardState, clearNewTipWizardState)
import State.NotificationState as NotificationState
import State.PinnedState as PinnedState
import State.SearchState as SearchState
import State.UserState exposing (UserInfo, UserState(..))
import State.WallState as WallState
import Update.Msg exposing(..)
import View.InfiniteScroll exposing (loadMoreIfNeeded, loadMoreOrLessIfNeeded)


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
        if WallState.isLoadingMore state.wall then state |> nocmd
        else if Page.isAfter page state.wall.currentPage && WallState.noMoreDataToLoad state.wall then state |> nocmd
        else update (DisplayPage WallPage) {state| wall = WallState.moveToPage state.wall page }
    RefreshWall ->
        update (DisplayPage WallPage) {state| wall = WallState.refresh }
    ChangeFeedPage page ->
        if FeedState.isLoadingMore state.feed then state |> nocmd
        else if Page.isAfter page state.feed.currentPage && FeedState.noMoreDataToLoad state.feed then state |> nocmd
        else update (DisplayPage FeedPage) {state| feed = FeedState.moveToPage state.feed page }
    RefreshFeed ->
        update (DisplayPage FeedPage) {state| feed = FeedState.refresh }
    ChangePinnedPage page ->
        if PinnedState.isLoadingMore state.pinned then state |> nocmd
        else if Page.isAfter page state.pinned.currentPage && PinnedState.noMoreDataToLoad state.pinned then state |> nocmd
        else update (DisplayPage PinnedPostPage) {state| pinned = PinnedState.moveToPage state.pinned page }
    RefreshPinnedPosts ->
        update (DisplayPage PinnedPostPage) {state| pinned = PinnedState.refresh }
    ChangeNotificationPage page ->
        update (DisplayPage NotificationPage) {state| notifications = NotificationState.moveToPage state.notifications page }
    ChangeSearchPage page ->
        if SearchState.isLoadingMore state.search then state |> nocmd
        else if Page.isAfter page state.search.currentPage && SearchState.noMoreDataToLoad state.search then state |> nocmd
        else update (DisplayPage SearchPage) {state| search = SearchState.moveToPage state.search page }
    DisplayPage RegistrationPage -> {state | display = RegistrationPage }
        |> nocmd
    DisplayPage page  -> {state |
        display = if (isUserLoggedIn state) then page else LoginPage,
        previous = (if (isUserLoggedIn state) then [page] else []) ++ state.previous }
        |> cmd (loadPageContent state page)
    FollowUser followee ->  {state| cache = Cache.addFollowingUser state.cache followee }
        |> ifLogged (\user -> followUser user followee)
    UnfollowUser followee -> {state| cache = Cache.removeFollowingUser state.cache followee }
        |> ifLogged (\user -> unfollowUser user followee)
    FollowHashtag hashtag ->  {state| cache = Cache.addFollowingHashtag state.cache hashtag }
        |> ifLogged (\user -> followHashtag user hashtag)
    UnfollowHashtag hashtag -> {state| cache = Cache.removeFollowingHashtag state.cache hashtag }
        |> ifLogged (\user -> unfollowHashtag user hashtag)
    LikePost postId -> {state| cache = Cache.addLike state.cache postId }
        |> ifLogged (\user -> like state.cache user postId)
    UnlikePost postId -> {state| cache = Cache.removeLike state.cache postId }
        |> ifLogged (\user -> unlike state.cache user postId)
    PinPost postId -> {state| cache = Cache.addPinned state.cache postId }
        |> ifLogged (\user -> pin user postId)
    UnpinPost postId -> {state| cache = Cache.removePinned state.cache postId }
        |> ifLogged (\user -> unpin user postId)
    OpenPostConversation postId -> {state| cache = Cache.setConversationOpened state.cache postId True }
        |> ifLogged (\user -> fetchConversation state.cache user postId (Page 1))
    ClosePostConversation postId -> {state| cache = Cache.setConversationOpened state.cache postId False }
        |> nocmd
    UpdateNewPostComment postId comment -> {state| cache = Cache.addComment state.cache postId comment }
        |> nocmd
    PostNewComment postId comment -> {state| cache = Cache.removeComment state.cache postId }
        |> ifLogged (\user -> postComment user postId comment)
    FlagComment messageId -> {state| cache = Cache.setFlaggedByUser state.cache messageId True }
        |> ifLogged (\user -> flagComment user messageId)
    UnflagComment messageId -> {state| cache = Cache.setFlaggedByUser state.cache messageId False }
        |> ifLogged (\user -> unflagComment user messageId)
    LoadMore id loadMoreMsg -> let _ = Debug.log "LoadMore msg received: " loadMoreMsg in
        state |> ifLogged (\_ -> loadMoreIfNeeded id loadMoreMsg)
    LoadMoreOrLess id loadLess loadMore -> let _ = Debug.log "LoadMoreOrLess msg received: " loadLess in
        state |> ifLogged (\_ -> loadMoreOrLessIfNeeded id loadLess loadMore)
    LoadMoreComment postId page -> state
        |> ifLogged (\user -> fetchConversation state.cache user postId page)
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
    ReportChallengeStepStatus challengeId step status -> {state|
        cache = Cache.simulateChallengeStepReports state.cache challengeId {step = step, status = status}}
        |> ifLogged (\user -> reportStepStatus user challengeId step status)
    ChangeChallengePage page ->
        if ChallengeState.isLoadingMore state.challenge then state |> nocmd
        else if Page.isAfter page state.challenge.currentPage && ChallengeState.noMoreDataToLoad state.challenge then state |> nocmd
        else state |> ifLogged (\user -> fetchUserChallengePosts state.cache user {tab = state.challenge.currentTab, page=page})
    ChangeChallengeTab tab -> {state|
        challenge = state.challenge |> ChallengeState.changeTab tab }
        |> ifLogged (\user -> fetchUserChallengePosts state.cache user {tab = tab, page = Page.first})
    AnswerPoll pollId option -> {state| cache = simulatePollAnswer state.cache pollId option }
        |> ifLogged (\user -> answerPollOption state.cache user pollId option)
    -----------------------
    --- Form processing ---
    -----------------------
    EnteringCredentials username password -> {state |
        user = LoggingIn { email = username, password = password } }
        |> nocmd
    EnteringSearch content -> {state |
        search = SearchState.input state.search content }
        |> nocmd
    FillingRegistrationForm content ->
        let updated = {state | forms = FormState.updateRegistrationFormState state.forms content }
        in update (CheckPseudoAvailability content.pseudo) updated
    CheckPseudoAvailability maybePseudo -> case maybePseudo of
        Nothing     -> state |> nocmd
        Just pseudo ->
            if String.length pseudo < 3
            then state |> nocmd
            else {state | forms = FormState.checkingPseudoAvailability state.forms }
                |> cmd (checkPseudoAvailability pseudo)
    RegisterNewAccount -> state
        |> cmd (register state.forms.registrationForm)
    VerifyAccount -> {state | forms = FormState.validatingAccount state.forms }
        |> cmd (verifyAccount state.forms.registrationForm)
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
    FillingNewChallengeWizard challengeState -> { state |
        forms = FormState.updateNewChallengeWizardState state.forms challengeState }
        |> nocmd
    PostNewChallenge newChallengeWizard -> let forms = state.forms in { state |
        forms = FormState.postingNewChallenge {forms| newChallengeWizard = newChallengeWizard} }
        |> ifLogged (\user -> postChallenge user newChallengeWizard)
    FillingNewPollWizard pollState -> { state |
        forms = FormState.updateNewPollWizardState state.forms pollState }
        |> nocmd
    PostNewPoll newPollWizard -> let forms = state.forms in { state |
        forms = FormState.postingNewPoll {forms| newPollWizard = newPollWizard} }
        |> ifLogged (\user -> postPoll user newPollWizard)
    ---------------------
    --- Http commands ---
    ---------------------
    HttpAuthenticated (Ok userInfo)    ->
        -- FIXME we need to execute several commands on a successful authentication
        -- following code is probably subject to race conditions
        let (state1, com1) = update (DisplayPage FeedPage) {state | user = LoggedIn userInfo, cache = Cache.addUser state.cache userInfo.id userInfo }
            (state2, com2) = state1 |> cmd Clock.scheduleClockTick
            (state3, com3) = update CheckNotifications state2
            (state4, com4) = update CheckFeed state3
            (state5, com5) = update (RefreshHashtagTrend 25) state4
        in state5 |> allOf [com1, com2, com3, com4, com5]
    HttpAuthenticated (Err err)        -> {state | display = (LoginFailedPage err), user = NotLogged }
        |> nocmd
    HttpPseudoAvailabilityChecked (Ok (pseudo,checked)) ->
        (if Just pseudo == state.forms.registrationForm.pseudo
        then {state | forms = FormState.pseudoAvailabilityChecked state.forms checked}
        else state) |> nocmd
    HttpPseudoAvailabilityChecked (Err err)             -> Debug.log ("Error while checking pseudo availability " ++ (errorToString err) )
        state |> nocmd
    HttpNewAccountRegistered (Ok ())   -> {state|
        forms = FormState.registrationSubmitted state.forms }
        |> nocmd
    HttpNewAccountRegistered (Err err) -> Debug.log ("Error while registering the account: " ++ (errorToString err))
        {state| forms = FormState.registrationSubmissionFailed state.forms }
        |> nocmd
    HttpNewAccountVerified (Ok ())   -> {state|
        forms = FormState.accountVerified state.forms }
        |> nocmd
    HttpNewAccountVerified (Err err) -> Debug.log ("Error while registering the account: " ++ (errorToString err))
        {state| forms = FormState.accountVerificationFailed state.forms }
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
    HttpHashtagFollowed _              -> state |> nocmd
    HttpHashtagUnfollowed _            -> state |> nocmd
    HttpPostPinned _                   -> state |> nocmd
    HttpPostUnpinned _                 -> state |> nocmd
    HttpMarkNotificationAsRead _       -> state |> nocmd

    HttpWallFetched (Ok (cache, wall)) -> {state |
        wall = (WallState.from state.wall wall),
        cache = Cache.merge cache state.cache }
        |> nocmd
    HttpWallFetched (Err err)          -> Debug.log ("Error while receiving wall " ++ (errorToString err) )
        state |> nocmd
    HttpFeedFetched (Ok (cache, feed)) -> {state |
        display = FeedPage,
        feed    = FeedState.from state.feed feed,
        cache   = Cache.merge cache state.cache }
        |> nocmd
    HttpFeedFetched (Err err)          -> Debug.log ("Error while receiving feed " ++ (errorToString err))
        state |> nocmd
    HttpFeedChecked (Ok newPosts) -> {state| feed = FeedState.updateNewPostsAvailable state.feed newPosts }
        |> ifLogged (\_ -> scheduleFeedCheck feedDelay)
    HttpFeedChecked (Err err) -> Debug.log ("Error while checking new feed: " ++ (errorToString err)) state
        |> ifLogged (\_ -> scheduleFeedCheck feedDelay)
    -- Post pinning
    HttpPinnedPostsFetched (Ok (cache, pinned)) -> { state |
        display = PinnedPostPage,
        pinned  = PinnedState.from state.pinned pinned,
        cache   = Cache.merge cache state.cache }
        |> nocmd
    HttpPinnedPostsFetched (Err err) -> Debug.log ("Error while receiving pinned posts " ++ (errorToString err))
        state |> nocmd
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
    HttpNewChallengePosted (Ok _) -> {state |
        display = NewPostPage,
        forms = clearNewChallengeWizardState state.forms}
        |> nocmd
    HttpNewChallengePosted (Err err) -> Debug.log ("Error while posting new challenge text: " ++ (errorToString err))
        {state| forms = FormState.newChallengePosted state.forms }
        |> nocmd
    HttpChallengePostsFetched (Ok (cache, {tab, page}, challenges)) -> {state |
        challenge = ChallengeState.from challenges {tab=tab, page=page} state.challenge
        , cache = Cache.merge cache state.cache }
        |> nocmd
    HttpChallengePostsFetched (Err err) -> Debug.log ("Error while getting challenge posts: " ++ (errorToString err))
        state |> nocmd
    HttpChallengeDetailsFetched (Ok (cache, _)) -> {state |
        cache = Cache.merge cache state.cache }
        |> nocmd
    HttpChallengeDetailsFetched (Err err) -> Debug.log ("Error while fetching challenge details: " ++ (errorToString err))
        state |> nocmd
    HttpChallengeAccepted _             -> state |> nocmd
    HttpChallengeRejected _             -> state |> nocmd
    HttpChallengeStepStatusReported _   -> state |> nocmd
    HttpPollAnswered _                  -> state |> nocmd
    HttpNewPollPosted (Ok _) -> {state |
        display = NewPostPage,
        forms = clearNewPollWizardState state.forms}
        |> nocmd
    HttpNewPollPosted (Err err) -> Debug.log ("Error while posting new Poll: " ++ (errorToString err))
        {state| forms = FormState.newPollPosted state.forms }
        |> nocmd
    HttpConversationPageFetched (Ok (cache, conversationPage)) ->
        let added = Cache.getConversationMessages state.cache conversationPage.postId ++ conversationPage.messages
            updated = Cache.addConversationMessages cache conversationPage.postId added
        in {state | cache = Cache.merge updated state.cache }
        |> nocmd
    HttpConversationPageFetched (Err err) -> Debug.log ("Error while getting challenge posts: " ++ (errorToString err))
        state |> nocmd
    HttpNewCommentPosted (Ok postId)         -> state
        |> ifLogged (\user -> fetchConversation state.cache user postId Page.first)
    HttpNewCommentPosted (Err err) -> Debug.log ("Error while getting posted comment: " ++ (errorToString err))
        state |> nocmd
    HttpCommentFlagged _                 -> state |> nocmd
    HttpCommentUnflagged _               -> state |> nocmd
    HttpLoggedOff _                      -> {state | display = LoggedOffPage, user = NotLogged }
        |> nocmd

    NoOp  -> state |> nocmd
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

-- Loads/Reloads a page content if needed and if the user is logged in
loadPageContent: AppState -> Display -> Cmd Msg
loadPageContent state page = case (state.user, page) of
    (LoggedIn user, WallPage)                         -> (fetchWall state.cache user state.wall.currentPage)
    (LoggedIn user, FeedPage)                         -> (fetchFeed state.cache user state.feed.currentPage)
    (LoggedIn user, PinnedPostPage)                   -> (fetchPinnedPosts state.cache user state.pinned.currentPage)
    (LoggedIn user, SearchPage)                       -> (performSearch state.cache user state.search.filter state.search.currentPage)
    (LoggedIn user, NotificationPage)                 -> (fetchNotifications state.cache user state.notifications.currentPage)
    (LoggedIn user, UserPage userId)                  -> (fetchUserWall state.cache user userId state.wall.currentPage)
    (LoggedIn user, PseudoPage pseudo)                -> (fetchWallByPseudo state.cache user pseudo)
    (LoggedIn user, ChallengePage)                    -> (fetchUserChallengePosts state.cache user {tab=state.challenge.currentTab, page=Page.first})
    (LoggedIn user, ChallengeDetailsPage challengeId) -> (fetchChallengeDetails state.cache user challengeId)
    _                                                 -> none

