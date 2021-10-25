module View.PostRenderer exposing (..)


import Basics as Int
import Data.Challenge as Challenge exposing (ChallengeOutcomeStatus(..), ChallengeStatistics, SuccessMeasure)
import Data.Conversation as Conversation exposing (Message)
import Data.Event as Event
import Data.Location exposing (Latitude(..), Location(..), Longitude(..), formatAddress, toMapUrl)
import Data.Page exposing (Page(..))
import Data.Poll as Poll exposing (Poll, PollOption(..), PollStats(..), emptyPollStats, normalizePollStats, optionAsString, respondents)
import Data.Post as Post exposing (Post, PostContent(..), PostId(..), Source(..))
import Data.Schedule as Schedule exposing (Schedule, UTCTimestamp(..))
import Data.Tip as Tip exposing (Tip)
import Data.Url exposing (Url(..))
import Data.User as User exposing (UserId(..))
import Element exposing (Color, Element, alignBottom, alignLeft, alignRight, alignTop, centerX, centerY, column, el, fill, height, padding, paddingEach, px, row, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (labelHidden)
import Query.QueryUtils exposing (baseUrl)
import State.AppState exposing (Display(..))
import State.Cache as Cache exposing (Cache)
import Update.Msg exposing (Msg(..))
import Utils.DateUtils as DateUtils exposing (formatDate)
import Utils.TextUtils as TextUtils exposing (QuotedString(..))
import Uuid
import View.Chart.ChartUtils exposing (legend)
import View.Chart.ColorScheme as ColorScheme
import View.Chart.Donut as Donut exposing (smallPieChart)
import View.Icons as Icons
import View.PartnershipStyle as PartnershipStyle
import View.ScreenUtils exposing (neverElement)
import View.Style exposing (..)
import View.Theme exposing (blue, charcoal, darkRed, grey, lightGreen, lightPurple, orange)
import View.UIStyle exposing (UIStyle)


renderPostId: UIStyle -> UTCTimestamp -> Cache -> PostId -> Element Msg
renderPostId ui tmstp cache postId = postId
    |> Cache.getPost cache
    |> Maybe.map (renderPost ui tmstp cache)
    |> Maybe.withDefault ("<Unable to render post " ++ (Post.toString postId) ++ ">" |> text)

renderPost: UIStyle -> UTCTimestamp -> Cache -> Post -> Element Msg
renderPost ui tmstp cache post =
    let reposted = isRepost post in
    column [width fill, alignLeft, spacing 5, padding 10 ]
        [renderHeader ui tmstp cache post
         , renderPostContent ui tmstp cache post
         , if not reposted then renderFooter ui cache post else Element.none
         , renderConversation ui tmstp cache post.id]
    |> PartnershipStyle.postDecoration ui cache post.id

renderPostContent: UIStyle -> UTCTimestamp -> Cache -> Post -> Element Msg
renderPostContent ui tmstp cache post = case post.content of
    RePost _         -> renderRepostPost ui tmstp cache post
    EventPost _      -> renderEventPost ui cache post
    ChallengePost _  -> renderChallengePost ui cache post
    TipPost _        -> renderTipPost ui cache post
    PollPost _       -> renderPollPost ui cache post
    FreeTextPost _ _ -> renderFreeTextPost ui cache post

renderPostContentById: UIStyle -> UTCTimestamp -> Cache -> PostId -> Element Msg
renderPostContentById ui tmstp cache postId = case Cache.getPost cache postId of
    Just post -> renderPostContent ui tmstp cache post
    Nothing   -> neverElement

renderEventPost: UIStyle -> Cache -> Post -> Element Msg
renderEventPost ui cache post = case post.content of
    EventPost id -> case Cache.getEvent cache id of
        Just event -> let openSlots =  Cache.getEventParticipantCount cache id |> Maybe.map (\ n -> event.maxParticipants - n) in
            row [spacing 10, width fill, height fill] [
                renderNextDate ui event.schedule,
                verticalSeparator 1 ui.theme.background,
                column [alignLeft, width fill, height fill, spacing 3] [
                    event.description |> multiLineQuotedText ui |> postBodyStyle ui |> el [width fill, alignTop]
                    , row [alignLeft, spacing 5] ["When:" |> bold |> relSize ui 0, formatDate (Schedule.start event.schedule) |> text |> relSize ui 0]
                    , row [alignLeft, spacing 5] ["Where:" |> bold |> relSize ui 0, renderLocation ui event.location]
                    , row [alignLeft, spacing 5] ["Max participants:" |> bold |> relSize ui 0, event.maxParticipants |> String.fromInt |> text |> relSize ui 0]
                    , row [alignLeft, spacing 5] ["Remaining slots:" |> bold |> relSize ui 0,
                       openSlots |> Maybe.map (String.fromInt) |> Maybe.withDefault "-" |> text |> relSize ui 0]
                ]]
        Nothing    -> id |> Event.toString |> text |> postBodyStyle ui
    _            -> neverElement

renderTipPost: UIStyle -> Cache -> Post -> Element Msg
renderTipPost ui cache post = case post.content of
    TipPost id -> case Cache.getTip cache id of
                    Just tip -> tip.content |> multiLineQuotedText ui |> postBodyStyle ui
                    Nothing  -> id |> Tip.toString |> text |> postBodyStyle ui
    _          -> neverElement

renderRepostPost: UIStyle -> UTCTimestamp -> Cache -> Post -> Element Msg
renderRepostPost ui tmstp cache post = case post.content of
    RePost id -> el [width fill, height fill, paddingEach {left=20,top=5, bottom=5,right=0}] (renderPostId ui tmstp cache id)
    _         -> neverElement

isRepost: Post -> Bool
isRepost post = case post.content of
    RePost _ -> True
    _        -> False

renderChallengePost: UIStyle -> Cache -> Post -> Element Msg
renderChallengePost ui cache post = case post.content of
    ChallengePost id   -> case Cache.getChallenge cache id of
        Just challenge -> row [spacing 10, width fill, height fill] [
            Cache.getChallengeStatistics cache id
                |> Maybe.map (renderChallengeStatistics ui)
                |> Maybe.withDefault Element.none
            , verticalSeparator 1 ui.theme.background
            , column [spacing 10, alignTop, width fill] [
                challenge.title |> quotedText ui |> el [Font.semiBold, relFontSize ui 0]
                , challenge.content |> multiLineQuotedText ui |> postBodyStyle ui]
            ]
        Nothing        -> id |> Challenge.toString |> text |> postBodyStyle ui
    _                  -> neverElement

renderPollPost: UIStyle -> Cache -> Post -> Element Msg
renderPollPost ui cache post = case post.content of
    PollPost id -> case (Cache.getPoll cache id, Cache.getPollAnswered cache id) of
        (Just poll, Just True)  -> renderAnsweredPoll ui cache poll
        (Just poll, _)          -> renderUnansweredPoll ui cache poll
        (Nothing, _)            -> id |> Poll.toString |> text |> postBodyStyle ui
    _                           -> neverElement

renderFreeTextPost: UIStyle -> Cache -> Post -> Element Msg
renderFreeTextPost ui cache post = case post.content of
    FreeTextPost content sources -> content |> multiLineQuotedText ui |> postBodyStyle ui
    _                            -> neverElement

-- Helpers
renderHeader: UIStyle -> UTCTimestamp -> Cache -> Post -> Element Msg
renderHeader ui tmstp cache post =
    let isFollowing = Cache.containsFollowingUser cache post.author
    in
        row [width fill, spacing 5, paddingEach {left=0,top=0,bottom=2,right=0}, Border.widthEach {left=0,top=0,bottom=2,right=0}, Border.color ui.theme.background] [
            el [alignLeft] (postLogo ui post |> standard ui)
            , el [alignLeft] (renderUser cache post.author |> relSize ui 0)
            , el [alignLeft] ((if isFollowing then unfollowButtonStyle ui else followButtonStyle ui) post.author)
            , el [alignLeft] (renderDate ui tmstp post.created)
            , el [alignRight, paddingEach {left=10,top=0,bottom=0,right=0}] (specialPostActions ui cache post)
        ]

specialPostActions: UIStyle -> Cache -> Post -> Element Msg
specialPostActions ui cache post =
    let partnerUserId = Cache.partnerForPost cache post.id
            |> Maybe.andThen (Cache.getPartner cache)
            |> Maybe.map (.userId)
        partnerAction = partnerUserId
            |> Maybe.map(\userId -> buttonBarStyle [relFontSize ui -1, Font.semiBold] [("View Partner", DisplayPage (UserPage userId))])
            |> Maybe.withDefault Element.none
    in
    case post.content of
        ChallengePost id -> let challengeText = case Cache.getChallengeOutcomeStatus cache id of
                                                Just OnTracks    -> "Report"
                                                Just NotYetTaken -> "Take challenge"
                                                Just Failed      -> "Failed"
                                                _                -> "View challenge"
                                actionButtons = [(challengeText, DisplayPage (ChallengeDetailsPage id))] |> List.map (postButtonBarStyle ui)
            in row [alignRight, spacing 3] (actionButtons ++ [partnerAction] |> withVerticalSeparator 1 ui.theme.background)
        EventPost id ->
            let viewAction = ("View event", (DisplayPage (EventDetailsPage id)))
                actions = case (Cache.getEventCancelledStatus cache id,
                                Cache.getEventParticipationRequestStatus cache id,
                                Cache.getEventParticipationStatus cache id) of
                          (Just True, _, _)          -> [("Event has been cancelled", NoOp)]
                          (_, _, Just True)          -> [("Cancel participation", CancelEventParticipation id), viewAction]
                          (_, Just True, Just False) -> [("Cancel participation request", CancelEventParticipation id), viewAction]
                          (_, Just False, _)         -> [("Request participation", RequestEventParticipation id), viewAction]
                          (_, Nothing,    _)         -> [("Request participation", RequestEventParticipation id), viewAction]
                          _                          -> [viewAction]
                actionButtons = actions |> List.map (postButtonBarStyle ui)
            in wrappedRow [alignRight, spacing 3] (actionButtons ++ [partnerAction] |> withVerticalSeparator 1 ui.theme.background)
        _                -> partnerAction

withVerticalSeparator: Int -> Color -> List (Element Msg) -> List (Element Msg)
withVerticalSeparator w col xs = xs
    |> List.filter (\el -> el /= Element.none)
    |> List.intersperse (verticalSeparator w col)


renderFooter: UIStyle -> Cache -> Post -> Element Msg
renderFooter ui cache post =
    let likes = Cache.getLikeCount cache post.id
        comments = Cache.getConversationSize cache post.id
        hasLiked = Cache.containsLike cache post.id
        isPinned = Cache.containsPinned cache post.id
        openedConversation = Cache.isConversationOpened cache post.id
    in
    (el [width fill]
        (row [width fill, spacing 5, alignLeft]
            ([el [] ((if isPinned then unpinButtonStyle ui else pinButtonStyle ui) post.id |> invert ui)
             , el [Font.italic, relFontSize ui -2] (repostButtonStyle ui post.id |> invert ui)
             , el [] ((if hasLiked then unlikeButtonStyle ui else likeButtonStyle ui) post.id |> invert ui)
             , el [Font.italic, relFontSize ui -2] ("x" ++ (String.fromInt likes) |> text)
             , el [] ((if openedConversation then closeConversationButtonStyle ui else openConversationButtonStyle ui) post.id |> invert ui)
             , el [Font.italic, relFontSize ui -2] ("x " ++ (String.fromInt comments) |> text)
             ]))
        |> postFooterStyle ui)

renderSource: UIStyle -> Cache -> Source -> Element Msg
renderSource ui cache src = case src of
    Web (Url url) -> linkStyle ui (Url url) url
    MySelf -> (text "Myself")
    PostReference (PostId uuid) -> let id = uuid |> Uuid.toString in
        linkStyle ui (Url (baseUrl ++ "/post/" ++ id)) "post"
    AcademicReference ref -> italicTextStyle ui ref

renderUser: Cache -> UserId -> Element Msg
renderUser cache userId = case Cache.getUser cache userId of
    Just user -> userPseudoStyle user.pseudo (Just user.id)
    Nothing   -> userId |> User.toString |> text

renderDate: UIStyle -> UTCTimestamp -> UTCTimestamp -> Element Msg
renderDate ui reftime timestamp = DateUtils.formatRelativeTo reftime timestamp
    |> text
    |> headerDateStyle ui

renderCommentDate: UTCTimestamp -> UTCTimestamp -> Element Msg
renderCommentDate reftime timestamp =
    DateUtils.formatRelativeTo reftime timestamp |> text

postLogo: UIStyle -> Post -> Element Msg
postLogo ui post =
    let render = el [width <| px 20, height <| px 20, Border.width 2, centerX, centerY]
        center = el [centerX, centerY] >> render
    in
        case post.content of
            RePost _         -> ui.tiny |> Icons.repost    |> center
            EventPost _      -> ui.tiny |> Icons.event     |> center
            ChallengePost _  -> ui.tiny |> Icons.challenge |> center
            TipPost _        -> ui.tiny |> Icons.tip       |> center
            PollPost _       -> ui.tiny |> Icons.poll      |> center
            FreeTextPost _ _ -> ui.tiny |> Icons.post      |> center

renderChallengeStatistics: UIStyle -> ChallengeStatistics -> Element Msg
renderChallengeStatistics ui stats = let contestants = stats.acceptedCount
                                         total   = (stats.elapsedPeriodCount * contestants) |> Int.toFloat
                                         success = (stats.successCount |> Int.toFloat) / total
                                         skipped = (stats.skippedCount |> Int.toFloat) / total
                                         partial = (stats.partialSuccessCount |> Int.toFloat) / total
                                         failure = (stats.failureCount |> Int.toFloat) / total
    in column [spacing 5, relFontSize ui -2] [
        Donut.doubleTinyDonut ui
            [(skipped, charcoal),(failure, darkRed), (partial, orange),(success, lightGreen)]
            [(stats.elapsedPeriodCount |> Int.toFloat, lightPurple), (stats.totalPeriodCount |> Int.toFloat, grey)]
        |>  el [centerX, centerY]
        , ((contestants |> String.fromInt) ++ " contestant(s)") |> bold |> el [alignRight]
     ]

renderChallengeSuccessMeasure: UIStyle -> SuccessMeasure -> Element Msg
renderChallengeSuccessMeasure ui measure = checkListStyle ui [relFontSize ui -1] [
    (measure.maxSkip > 0, if measure.maxSkip > 0 then "Skipping some reports allowed" else "Skipping report not allowed")
    , (measure.maxPartial > 0, if measure.maxPartial > 0 then "Reporting partial success allowed" else "Partial success report not allowed")
    , (measure.maxFailure > 0, if measure.maxFailure > 0 then "Some failure allowed" else "No failed report allowed")
 ]

renderAnsweredPoll: UIStyle -> Cache -> Poll -> Element Msg
renderAnsweredPoll ui cache poll =
    let (PollStats stats) = Cache.getPollStats cache poll.id
            |> Maybe.withDefault (emptyPollStats poll)
            |> normalizePollStats poll
        respondentCount = respondents (PollStats stats)
        sum = respondentCount |> Int.toFloat
        pollScheme = ColorScheme.pollPieChartScheme
        donutData = stats
            |> List.indexedMap (\index entry -> (100.0 * (Int.toFloat entry.count) / sum, pollScheme |> ColorScheme.cycledColorAt index))
        legendData = stats
            |> List.indexedMap (\index entry -> (entry.option |> optionAsString, pollScheme |> ColorScheme.cycledColorAt index))
    in column [spacing 10] [
        poll.title |> multiLineQuotedText ui |> el [Font.bold] |> postBodyStyle ui
        , row [width fill, height fill, spacing 5] [
            column [height fill, paddingEach {left=0, top=0, bottom=0, right=5}, spacing 5, relFontSize ui -2] [
                el [centerX, centerY] (smallPieChart ui donutData)
                , ((respondentCount |> String.fromInt) ++ " respondent(s)") |> bold |> el [alignRight]
            ],
            verticalSeparator 1 ui.theme.background,
            legend ui [relFontSize ui 1] legendData
        ]
     ]

renderUnansweredPoll: UIStyle -> Cache -> Poll -> Element Msg
renderUnansweredPoll ui cache poll = Input.radio
    [padding 5 , spacing 5]
    { onChange = AnswerPoll poll.id
      , selected = Nothing
      , label = Input.labelAbove [] (poll.title |> multiLineQuotedText ui |> el [Font.bold] |> postBodyStyle ui)
      , options = poll.options |> List.map (\ (PollOption opt) -> Input.option (PollOption opt) (opt |> multiLineQuotedText ui |> postBodyStyle ui))
    }

renderConversation: UIStyle -> UTCTimestamp -> Cache -> PostId -> Element Msg
renderConversation ui tmstp cache postId =
    let closed = Cache.isConversationClosed cache postId in
    if closed then Element.none
    else renderOpenedConversation ui tmstp cache postId

renderOpenedConversation: UIStyle -> UTCTimestamp -> Cache -> PostId -> Element Msg
renderOpenedConversation ui tmstp cache postId =
    let messages = Cache.getConversationMessages cache postId in
    column [spacing 7, width fill, paddingEach { left = 10, right = 0, top = 0, bottom = 0}]
    ([renderMessageInput ui cache postId] ++
     (messages |> List.map (renderConversationMessage ui tmstp cache postId)) ++
     [renderMoreMessageSeparator ui cache postId]
    )

renderMessageInput: UIStyle -> Cache -> PostId -> Element Msg
renderMessageInput ui cache postId =
    let currentComment = Cache.getComment cache postId |> Maybe.withDefault ""
        valid = currentComment |> TextUtils.nonEmpty
    in
    row [width fill, spacing 5] [
        Input.multiline [width fill
            , relFontSize ui 1
            , Font.color ui.theme.textFieldForeground
            , Background.color ui.theme.textFieldBackground] {
        onChange = (\txt -> UpdateNewPostComment postId txt)
        , text = Cache.getComment cache postId |> Maybe.withDefault ""
        , placeholder = placeholderStyle ui "Enter a comment ..."
        , label = labelHidden "hidden comment"
        , spellcheck = True }
        , Input.button [alignRight, alignBottom
            , Border.width 2, Border.rounded 5
            , padding 5
            , Font.color ui.theme.foreground, Background.color ui.theme.background] {
          onPress = if valid
                    then PostNewComment postId (Cache.getComment cache postId |> Maybe.withDefault "") |> Just
                    else Nothing
          , label = "Post" |> text
        }
 ]

renderConversationMessage: UIStyle -> UTCTimestamp -> Cache -> PostId -> Message -> Element Msg
renderConversationMessage ui tmstp cache _ message =
    let pseudo = Cache.getUserPseudo cache message.author |> Maybe.withDefault "<unknown>"
        flagged = Cache.isFlagged cache message.id
        contentStyle = if flagged then postBodyStyle ui >> el [Font.color ui.theme.flaggedForeground, Font.italic] else postBodyStyle ui
    in  column [width fill] [
        row [alignLeft, spacing 1] [
            userStyle ui pseudo (Just message.author) |> postBodyStyle ui
            , renderCommentDate tmstp message.timestamp |> postFooterStyle ui |> rightGap 5
            , renderFlagAction ui cache message |> rightGap 20
            , renderFlagWarning ui flagged
        ]
        , el [paddingEach {left = 15, right = 0, top = 0, bottom = 0}]
            (message.content |> multiLineQuotedText ui |> contentStyle)
    ]

renderFlagAction: UIStyle -> Cache -> Message -> Element Msg
renderFlagAction ui cache message =
    let flaggedByUser = Cache.isFlaggedByUser cache message.id
        action = (if flaggedByUser then UnflagComment else FlagComment) message.id
        col = if flaggedByUser then ui.theme.alertColor else ui.theme.background
        icon = if flaggedByUser then Icons.flagged else Icons.flag
    in Input.button [Border.width 0] {
        onPress = Just action
        , label = icon ui.tiny |> el [Font.color col]
    }

renderFlagWarning: UIStyle -> Bool -> Element Msg
renderFlagWarning ui flagged =
    if flagged then "*** This message has been reported ***"
        |> text
        |> postFooterStyle ui
        |> el [Font.color darkRed]
    else Element.none

renderMoreMessageSeparator: UIStyle -> Cache -> PostId -> Element Msg
renderMoreMessageSeparator ui cache postId =
    let messagesCacheSize = Cache.getConversationMessages cache postId |> List.length
        nextPage = (messagesCacheSize // Conversation.pageSize) + 1
    in
    row [width fill, spacing 10]
        [Input.button [
            alignLeft
            ,Border.width 0
            , relFontSize ui 0
            , padding 5] {
            onPress = LoadMoreComment postId (Page nextPage) |> Just
            , label = "More ..." |> text |> el [Font.color ui.theme.foreground]
        }
       , horizontalSeparator 1 ui.theme.background
 ]

-- Render a loading post
renderLoadingSinglePost: UIStyle -> Element Msg
renderLoadingSinglePost ui =
    column [width fill, alignLeft, spacing 5, padding 10 ]
        [row [alignLeft] [loadingFixedTextLine ui 12 16, loadingFixedTextLine ui 12 100, loadingFixedTextLine ui 12 35]
         |> el [width fill, spacing 5, paddingEach {left=0,top=0,bottom=2,right=0}, Border.widthEach {left=0,top=0,bottom=2,right=0}, Border.color ui.theme.background]
         , loadingTextBlock ui 12 3
         , row [alignLeft, width fill] (loadingFixedTextLine ui 8 12 |> List.repeat 5)]

renderLoadingPostPage: UIStyle -> Int -> Element Msg
renderLoadingPostPage ui count = List.range 1 count
    |> List.map (\_ -> renderLoadingSinglePost ui)
    |> column [width fill , height fill, centerX, spacing 5, padding 10]

renderNextDate: UIStyle -> Schedule -> Element Msg
renderNextDate ui schedule =
    let timestamp = Schedule.start schedule
        weekday = Schedule.weekDay timestamp |> text
        day = Schedule.day timestamp |> TextUtils.format2Digits |> text
        month = Schedule.month timestamp |> text
        year = Schedule.year timestamp |> TextUtils.format4Digits |> text |> el [centerX]
    in column [alignLeft, spacing 5, Border.width 1, Border.color ui.theme.background, Border.rounded 3] [
        year |> el [centerY, relFontSize ui 2, Font.semiBold, Font.color ui.theme.foreground, Background.color ui.theme.background, padding 2, width fill]
        , weekday |> el [centerX, relFontSize ui 0, paddingEach {left=3, right=3, top=0, bottom=3}]
        , day |> el [centerX, Font.color ui.theme.background, relFontSize ui 4, Font.bold, paddingEach {left=3, right=3, top=0, bottom=3}]
        , month |> el [centerX, relFontSize ui 0, paddingEach {left=3, right=3, top=0, bottom=3}]
    ]

renderLocation: UIStyle -> Location -> Element Msg
renderLocation ui loc = case loc of
    Online (Url url) ->
        Element.newTabLink [relFontSize ui 0] { url = url, label = "online" |> text |> el [Font.color ui.theme.linkForeground] }
    MapUrl (Url url) ->
        Element.newTabLink [relFontSize ui 0] { url = url, label = "See on maps" |> text |> el [Font.color ui.theme.linkForeground] }
    (GeoLocation _ _) as geo -> let (Url url) = toMapUrl 17 geo in
        Element.newTabLink [relFontSize ui 0] { url = url, label = "See on maps" |> text |> el [Font.color ui.theme.linkForeground] }
    (Address _ _ _) as address -> formatAddress address |> text |> el [Font.color blue, relFontSize ui 0]