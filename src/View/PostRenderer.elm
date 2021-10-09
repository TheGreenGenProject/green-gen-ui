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
import Element exposing (Color, Element, alignBottom, alignLeft, alignRight, alignTop, centerX, centerY, column, el, fill, height, padding, paddingEach, px, row, spacing, text, width)
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
import View.Theme exposing (background, blue, charcoal, darkGrey, darkRed, foreground, grey, lightGreen, lightPurple, linkForeground, orange, textFieldBackground, textFieldForeground)


renderPostId: UTCTimestamp -> Cache -> PostId -> Element Msg
renderPostId tmstp cache postId = postId
    |> Cache.getPost cache
    |> Maybe.map (renderPost tmstp cache)
    |> Maybe.withDefault ("<Unable to render post " ++ (Post.toString postId) ++ ">" |> text)

renderPost: UTCTimestamp -> Cache -> Post -> Element Msg
renderPost tmstp cache post =
    let reposted = isRepost post in
    column [width fill, alignLeft, spacing 5, padding 10 ]
        [renderHeader tmstp cache post
         , renderPostContent tmstp cache post
         , if not reposted then renderFooter cache post else Element.none
         , renderConversation tmstp cache post.id]
    |> PartnershipStyle.postDecoration cache post.id

renderPostContent: UTCTimestamp -> Cache -> Post -> Element Msg
renderPostContent tmstp cache post = case post.content of
    RePost _         -> renderRepostPost tmstp cache post
    EventPost _      -> renderEventPost cache post
    ChallengePost _  -> renderChallengePost cache post
    TipPost _        -> renderTipPost cache post
    PollPost _       -> renderPollPost cache post
    FreeTextPost _ _ -> renderFreeTextPost cache post

renderPostContentById: UTCTimestamp -> Cache -> PostId -> Element Msg
renderPostContentById tmstp cache postId = case Cache.getPost cache postId of
    Just post -> renderPostContent tmstp cache post
    Nothing   -> neverElement

renderEventPost: Cache -> Post -> Element Msg
renderEventPost cache post = case post.content of
    EventPost id -> case Cache.getEvent cache id of
        Just event -> let openSlots =  Cache.getEventParticipantCount cache id |> Maybe.map (\ n -> event.maxParticipants - n) in
            row [spacing 10, width fill, height fill] [
                renderNextDate event.schedule,
                verticalSeparator 1 background,
                column [alignLeft, width fill, height fill, spacing 3] [
                    event.description |> multiLineQuotedText |> postBodyStyle |> el [width fill, alignTop]
                    , row [alignLeft, spacing 5] ["When:" |> bold |> size 10, formatDate (Schedule.start event.schedule) |> text |> size 10]
                    , row [alignLeft, spacing 5] ["Where:" |> bold |> size 10, renderLocation event.location]
                    , row [alignLeft, spacing 5] ["Max participants:" |> bold |> size 10, event.maxParticipants |> String.fromInt |> text |> size 10]
                    , row [alignLeft, spacing 5] ["Remaining slots:" |> bold |> size 10,
                       openSlots |> Maybe.map (String.fromInt) |> Maybe.withDefault "-" |> text |> size 10]
                ]]
        Nothing    -> id |> Event.toString |> text |> postBodyStyle
    _            -> neverElement

renderTipPost: Cache -> Post -> Element Msg
renderTipPost cache post = case post.content of
    TipPost id -> case Cache.getTip cache id of
                    Just tip -> tip.content |> multiLineQuotedText |> postBodyStyle
                    Nothing  -> id |> Tip.toString |> text |> postBodyStyle
    _          -> neverElement

renderRepostPost: UTCTimestamp -> Cache -> Post -> Element Msg
renderRepostPost tmstp cache post = case post.content of
    RePost id -> el [width fill, height fill, paddingEach {left=20,top=5, bottom=5,right=0}] (renderPostId tmstp cache id)
    _         -> neverElement

isRepost: Post -> Bool
isRepost post = case post.content of
    RePost _ -> True
    _        -> False

renderChallengePost: Cache -> Post -> Element Msg
renderChallengePost cache post = case post.content of
    ChallengePost id   -> case Cache.getChallenge cache id of
        Just challenge -> row [spacing 10, width fill, height fill] [
            Cache.getChallengeStatistics cache id
                |> Maybe.map renderChallengeStatistics
                |> Maybe.withDefault Element.none
            , verticalSeparator 1 background
            , column [spacing 10, alignTop, width fill] [
                challenge.title |> quotedText |> el [Font.semiBold, Font.size 10]
                , challenge.content |> multiLineQuotedText |> postBodyStyle]
            ]
        Nothing        -> id |> Challenge.toString |> text |> postBodyStyle
    _                  -> neverElement

renderPollPost: Cache -> Post -> Element Msg
renderPollPost cache post = case post.content of
    PollPost id -> case (Cache.getPoll cache id, Cache.getPollAnswered cache id) of
        (Just poll, Just True)  -> renderAnsweredPoll cache poll
        (Just poll, _)          -> renderUnansweredPoll cache poll
        (Nothing, _)            -> id |> Poll.toString |> text |> postBodyStyle
    _                           -> neverElement

renderFreeTextPost: Cache -> Post -> Element Msg
renderFreeTextPost cache post = case post.content of
    FreeTextPost content sources -> content |> multiLineQuotedText |> postBodyStyle
    _                            -> neverElement

-- Helpers
renderHeader: UTCTimestamp -> Cache -> Post -> Element Msg
renderHeader tmstp cache post =
    let isFollowing = Cache.containsFollowingUser cache post.author
    in
        row [width fill, spacing 5, paddingEach {left=0,top=0,bottom=2,right=0}, Border.widthEach {left=0,top=0,bottom=2,right=0}, Border.color background] [
            el [alignLeft] (postLogo post |> standard)
            , el [alignLeft] (renderUser cache post.author |> size 10)
            , el [alignLeft] ((if isFollowing then unfollowButtonStyle else followButtonStyle) post.author)
            , el [alignLeft] (renderDate tmstp post.created)
            , el [alignRight, paddingEach {left=10,top=0,bottom=0,right=0}] (specialPostActions cache post)
        ]

specialPostActions: Cache -> Post -> Element Msg
specialPostActions cache post =
    let partnerUserId = Cache.partnerForPost cache post.id
            |> Maybe.andThen (Cache.getPartner cache)
            |> Maybe.map (.userId)
        partnerAction = partnerUserId
            |> Maybe.map(\userId -> buttonBarStyle [Font.size 9, Font.semiBold] [("View Partner", DisplayPage (UserPage userId))])
            |> Maybe.withDefault Element.none
    in
    case post.content of
        ChallengePost id -> let challengeText = case Cache.getChallengeOutcomeStatus cache id of
                                                Just OnTracks    -> "Report"
                                                Just NotYetTaken -> "Take challenge"
                                                Just Failed      -> "Failed"
                                                _                -> "View challenge"
                                actionButtons = [(challengeText, DisplayPage (ChallengeDetailsPage id))] |> List.map (postButtonBarStyle)
            in row [alignRight, spacing 3] (actionButtons ++ [partnerAction] |> withVerticalSeparator 1 background)
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
                actionButtons = actions |> List.map (postButtonBarStyle)
            in row [alignRight, spacing 3] (actionButtons ++ [partnerAction] |> withVerticalSeparator 1 background)
        _                -> partnerAction

withVerticalSeparator: Int -> Color -> List (Element Msg) -> List (Element Msg)
withVerticalSeparator w col xs = xs
    |> List.filter (\el -> el /= Element.none)
    |> List.intersperse (verticalSeparator w col)


renderFooter: Cache -> Post -> Element Msg
renderFooter cache post =
    let likes = Cache.getLikeCount cache post.id
        comments = Cache.getConversationSize cache post.id
        hasLiked = Cache.containsLike cache post.id
        isPinned = Cache.containsPinned cache post.id
        openedConversation = Cache.isConversationOpened cache post.id
    in
    (el [width fill]
        (row [width fill, spacing 5, alignLeft]
            ([el [] ((if isPinned then unpinButtonStyle else pinButtonStyle) post.id |> invert)
             , el [Font.italic, Font.size 8] (repostButtonStyle post.id |> invert)
             , el [] ((if hasLiked then unlikeButtonStyle else likeButtonStyle) post.id |> invert)
             , el [Font.italic, Font.size 8] ("x" ++ (String.fromInt likes) |> text)
             , el [] ((if openedConversation then closeConversationButtonStyle else openConversationButtonStyle) post.id |> invert)
             , el [Font.italic, Font.size 8] ("x " ++ (String.fromInt comments) |> text)
             ]))
        |> postFooterStyle)

renderSource: Cache -> Source -> Element Msg
renderSource cache src = case src of
    Web (Url url) -> linkStyle (Url url) url
    MySelf -> (text "Myself")
    PostReference (PostId uuid) -> let id = uuid |> Uuid.toString in
        linkStyle (Url (baseUrl ++ "/post/" ++ id)) "post"
    AcademicReference ref -> italicTextStyle ref

renderUser: Cache -> UserId -> Element Msg
renderUser cache userId = case Cache.getUser cache userId of
    Just user -> userPseudoStyle user.pseudo (Just user.id)
    Nothing   -> userId |> User.toString |> text

renderDate: UTCTimestamp -> UTCTimestamp -> Element Msg
renderDate reftime timestamp = DateUtils.formatRelativeTo reftime timestamp
    |> text
    |> headerDateStyle

renderCommentDate: UTCTimestamp -> UTCTimestamp -> Element Msg
renderCommentDate reftime timestamp =
    DateUtils.formatRelativeTo reftime timestamp |> text

postLogo: Post -> Element Msg
postLogo post =
    let render = el [width <| px 20, height <| px 20, Border.width 2, centerX, centerY]
        center = el [centerX, centerY] >> render
    in
        case post.content of
            RePost _         -> Icons.tiny |> Icons.repost    |> center
            EventPost _      -> Icons.tiny |> Icons.event     |> center
            ChallengePost _  -> Icons.tiny |> Icons.challenge |> center
            TipPost _        -> Icons.tiny |> Icons.tip       |> center
            PollPost _       -> Icons.tiny |> Icons.poll      |> center
            FreeTextPost _ _ -> Icons.tiny |> Icons.post      |> center

renderChallengeStatistics: ChallengeStatistics -> Element Msg
renderChallengeStatistics stats = let contestants = stats.acceptedCount
                                      total   = (stats.elapsedPeriodCount * contestants) |> Int.toFloat
                                      success = (stats.successCount |> Int.toFloat) / total
                                      skipped = (stats.skippedCount |> Int.toFloat) / total
                                      partial = (stats.partialSuccessCount |> Int.toFloat) / total
                                      failure = (stats.failureCount |> Int.toFloat) / total
    in column [spacing 5, Font.size 8] [
        Donut.doubleTinyDonut
            [(skipped, charcoal),(failure, darkRed), (partial, orange),(success, lightGreen)]
            [(stats.elapsedPeriodCount |> Int.toFloat, lightPurple), (stats.totalPeriodCount |> Int.toFloat, grey)]
        |>  el [centerX, centerY]
        , ((contestants |> String.fromInt) ++ " contestant(s)") |> bold |> el [alignRight]
     ]

renderChallengeSuccessMeasure: SuccessMeasure -> Element Msg
renderChallengeSuccessMeasure measure = checkListStyle [Font.size 9] [
    (measure.maxSkip > 0, if measure.maxSkip > 0 then "Skipping some reports allowed" else "Skipping report not allowed")
    , (measure.maxPartial > 0, if measure.maxPartial > 0 then "Reporting partial success allowed" else "Partial success report not allowed")
    , (measure.maxFailure > 0, if measure.maxFailure > 0 then "Some failure allowed" else "No failed report allowed")
 ]

renderAnsweredPoll: Cache -> Poll -> Element Msg
renderAnsweredPoll cache poll =
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
        poll.title |> multiLineQuotedText |> el [Font.bold] |> postBodyStyle
        , row [width fill, height fill, spacing 5] [
            column [height fill, paddingEach {left=0, top=0, bottom=0, right=5}, spacing 5, Font.size 8] [
                el [centerX, centerY] (smallPieChart donutData)
                , ((respondentCount |> String.fromInt) ++ " respondent(s)") |> bold |> el [alignRight]
            ],
            verticalSeparator 1 background,
            legend [Font.size 11] legendData
        ]
     ]

renderUnansweredPoll: Cache -> Poll -> Element Msg
renderUnansweredPoll cache poll = Input.radio
    [padding 5 , spacing 5]
    { onChange = AnswerPoll poll.id
      , selected = Nothing
      , label = Input.labelAbove [] (poll.title |> multiLineQuotedText |> el [Font.bold] |> postBodyStyle)
      , options = poll.options |> List.map (\ (PollOption opt) -> Input.option (PollOption opt) (opt |> multiLineQuotedText |> postBodyStyle))
    }

renderConversation: UTCTimestamp -> Cache -> PostId -> Element Msg
renderConversation tmstp cache postId =
    let closed = Cache.isConversationClosed cache postId in
    if closed then Element.none
    else renderOpenedConversation tmstp cache postId

renderOpenedConversation: UTCTimestamp -> Cache -> PostId -> Element Msg
renderOpenedConversation tmstp cache postId =
    let messages = Cache.getConversationMessages cache postId in
    column [spacing 7, width fill, paddingEach { left = 10, right = 0, top = 0, bottom = 0}]
    ([renderMessageInput cache postId] ++
     (messages |> List.map (renderConversationMessage tmstp cache postId)) ++
     [renderMoreMessageSeparator cache postId]
    )

renderMessageInput: Cache -> PostId -> Element Msg
renderMessageInput cache postId =
    let currentComment = Cache.getComment cache postId |> Maybe.withDefault ""
        valid = currentComment |> TextUtils.nonEmpty
    in
    row [width fill, spacing 5] [
        Input.multiline [width fill, Font.size 11, Font.color textFieldForeground, Background.color textFieldBackground] {
        onChange = (\txt -> UpdateNewPostComment postId txt)
        , text = Cache.getComment cache postId |> Maybe.withDefault ""
        , placeholder = placeholderStyle "Enter a comment ..."
        , label = labelHidden "hidden comment"
        , spellcheck = True }
        , Input.button [alignRight, alignBottom
            , Border.width 2, Border.rounded 5
            , padding 5
            , Font.color foreground, Background.color background] {
          onPress = if valid
                    then PostNewComment postId (Cache.getComment cache postId |> Maybe.withDefault "") |> Just
                    else Nothing
          , label = "Post" |> text
        }
 ]

renderConversationMessage: UTCTimestamp -> Cache -> PostId -> Message -> Element Msg
renderConversationMessage tmstp cache _ message =
    let pseudo = Cache.getUserPseudo cache message.author |> Maybe.withDefault "<unknown>"
        flagged = Cache.isFlagged cache message.id
        contentStyle = if flagged then postBodyStyle >> el [Font.color darkGrey, Font.italic] else postBodyStyle
    in  column [width fill] [
        row [alignLeft, spacing 1] [
            userStyle pseudo (Just message.author) |> postBodyStyle
            , renderCommentDate tmstp message.timestamp |> postFooterStyle |> rightGap 5
            , renderFlagAction cache message |> rightGap 20
            , renderFlagWarning flagged
        ]
        , el [paddingEach {left = 15, right = 0, top = 0, bottom = 0}]
            (message.content |> multiLineQuotedText |> contentStyle)
    ]

renderFlagAction: Cache -> Message -> Element Msg
renderFlagAction cache message =
    let flaggedByUser = Cache.isFlaggedByUser cache message.id
        action = (if flaggedByUser then UnflagComment else FlagComment) message.id
        col = if flaggedByUser then darkRed else background
        icon = if flaggedByUser then Icons.flagged else Icons.flag
    in Input.button [Border.width 0] {
        onPress = Just action
        , label = icon Icons.tiny |> el [Font.color col]
    }

renderFlagWarning: Bool -> Element Msg
renderFlagWarning flagged =
    if flagged then "*** This message has been reported ***"
        |> text
        |> postFooterStyle
        |> el [Font.color darkRed]
    else Element.none

renderMoreMessageSeparator: Cache -> PostId -> Element Msg
renderMoreMessageSeparator cache postId =
    let messagesCacheSize = Cache.getConversationMessages cache postId |> List.length
        nextPage = (messagesCacheSize // Conversation.pageSize) + 1
    in
    row [width fill, spacing 10]
        [Input.button [
            alignLeft
            ,Border.width 0
            , Font.size 10
            , padding 5] {
            onPress = LoadMoreComment postId (Page nextPage) |> Just
            , label = "More ..." |> text |> el [Font.color darkGrey]
        }
       , horizontalSeparator 1 background
 ]

-- Render a loading post
renderLoadingSinglePost:  Element Msg
renderLoadingSinglePost =
    column [width fill, alignLeft, spacing 5, padding 10 ]
        [row [alignLeft] [loadingFixedTextLine 12 16, loadingFixedTextLine 12 100, loadingFixedTextLine 12 35]
         |> el [width fill, spacing 5, paddingEach {left=0,top=0,bottom=2,right=0}, Border.widthEach {left=0,top=0,bottom=2,right=0}, Border.color background]
         , loadingTextBlock 12 3
         , row [alignLeft, width fill] (loadingFixedTextLine 8 12 |> List.repeat 5)]

renderLoadingPostPage: Int -> Element Msg
renderLoadingPostPage count = List.range 1 count
    |> List.map (\_ -> renderLoadingSinglePost)
    |> column [width fill , height fill, centerX, spacing 5, padding 10]

renderNextDate: Schedule -> Element Msg
renderNextDate schedule =
    let timestamp = Schedule.start schedule
        weekday = Schedule.weekDay timestamp |> text
        day = Schedule.day timestamp |> TextUtils.format2Digits |> text
        month = Schedule.month timestamp |> text
        year = Schedule.year timestamp |> TextUtils.format4Digits |> text |> el [centerX]
    in column [alignLeft, spacing 5, Border.width 1, Border.color background, Border.rounded 3] [
        year |> el [centerY, Font.size 12, Font.semiBold, Font.color foreground, Background.color background, padding 2, width fill]
        , weekday |> el [centerX, Font.size 10, paddingEach {left=3, right=3, top=0, bottom=3}]
        , day |> el [centerX, Font.color background, Font.size 14, Font.bold, paddingEach {left=3, right=3, top=0, bottom=3}]
        , month |> el [centerX, Font.size 10, paddingEach {left=3, right=3, top=0, bottom=3}]
    ]

renderLocation: Location -> Element Msg
renderLocation loc = case loc of
    Online (Url url) ->
        Element.newTabLink [Font.size 10] { url = url, label = "online" |> text |> el [Font.color linkForeground] }
    MapUrl (Url url) ->
        Element.newTabLink [Font.size 10] { url = url, label = "See on maps" |> text |> el [Font.color linkForeground] }
    (GeoLocation _ _) as geo -> let (Url url) = toMapUrl 17 geo in
        Element.newTabLink [Font.size 10] { url = url, label = "See on maps" |> text |> el [Font.color linkForeground] }
    (Address _ _ _) as address -> formatAddress address |> text |> el [Font.color blue, Font.size 10]