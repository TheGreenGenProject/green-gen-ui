module View.PostRenderer exposing (..)


import Basics as Int
import Data.Challenge as Challenge exposing (ChallengeOutcomeStatus(..), ChallengeStatistics, SuccessMeasure)
import Data.Conversation as Conversation exposing (Message)
import Data.Page exposing (Page(..))
import Data.Poll as Poll exposing (Poll, PollOption(..), PollStats(..), emptyPollStats, normalizePollStats, respondents)
import Data.Post as Post exposing (Post, PostContent(..), PostId(..), Source(..))
import Data.Schedule exposing (UTCTimestamp(..))
import Data.Tip as Tip exposing (Tip)
import Data.Url exposing (Url(..))
import Data.User as User exposing (UserId(..))
import Element exposing (Element, alignBottom, alignLeft, alignRight, alignTop, centerX, centerY, column, el, fill, height, padding, paddingEach, paragraph, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (labelHidden)
import Query.QueryUtils exposing (baseUrl)
import State.AppState exposing (Display(..))
import State.Cache as Cache exposing (Cache)
import Update.Msg exposing (Msg(..))
import Utils.DateUtils as DateUtils
import Utils.TextUtils as TextUtils exposing (QuotedString(..))
import Uuid
import View.Chart.Donut as Donut
import View.Icons as Icons
import View.ScreenUtils exposing (neverElement)
import View.Style exposing (..)
import View.Theme exposing (background, darkGrey, darkRed, foreground, lightGrey, lightPurple, orange)


renderPostId: UTCTimestamp -> Cache -> PostId -> Element Msg
renderPostId tmstp cache postId = postId
    |> Cache.getPost cache
    |> Maybe.map (renderPost tmstp cache)
    |> Maybe.withDefault ("<Unable to render post " ++ (Post.toString postId) ++ ">" |> text)

renderPost: UTCTimestamp -> Cache -> Post -> Element Msg
renderPost tmstp cache post = column [width fill, alignLeft, spacing 5, padding 10 ]
    [renderHeader tmstp cache post
     , renderPostContent cache post
     , renderFooter cache post
     , renderConversation tmstp cache post.id]

renderPostContent: Cache -> Post -> Element Msg
renderPostContent cache post = case post.content of
    RePost _         -> renderRepostPost cache post
    EventPost _      -> renderEventPost cache post
    ChallengePost _  -> renderChallengePost cache post
    TipPost _        -> renderTipPost cache post
    PollPost _       -> renderPollPost cache post
    FreeTextPost _ _ -> renderFreeTextPost cache post

renderPostContentById: Cache -> PostId -> Element Msg
renderPostContentById cache postId = case Cache.getPost cache postId of
    Just post -> renderPostContent cache post
    Nothing   -> neverElement

-- Render each type of post
renderEventPost: Cache -> Post -> Element Msg
renderEventPost cache post = (text "Event")

renderTipPost: Cache -> Post -> Element Msg
renderTipPost cache post = case post.content of
    TipPost id -> case Cache.getTip cache id of
                    Just tip -> tip.content |> multiLineQuotedText |> postBodyStyle
                    Nothing  -> id |> Tip.toString |> text |> postBodyStyle
    _          -> neverElement

renderRepostPost: Cache -> Post -> Element Msg
renderRepostPost cache post = case post.content of
    RePost id -> renderPostContentById cache id
    _         -> neverElement

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
specialPostActions cache post = case post.content of
    ChallengePost id -> let challengeText = case Cache.getChallengeOutcomeStatus cache id of
                                            Just OnTracks    -> "Report"
                                            Just NotYetTaken -> "Take challenge"
                                            Just Failed      -> "Failed"
                                            _                -> "View challenge"
        in buttonBarStyle [Font.size 9, Font.semiBold] [(challengeText, DisplayPage (ChallengeDetailsPage id))]
    _                -> Element.none

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
            [(skipped, lightGrey),(failure, darkRed), (partial, orange),(success, background)]
            [(stats.elapsedPeriodCount |> Int.toFloat, lightPurple), (stats.totalPeriodCount |> Int.toFloat, lightGrey)]
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
        sum = respondents (PollStats stats) |> Int.toFloat
        percentages = stats
            |> List.map (\entry -> (entry.option, 100.0 * (Int.toFloat entry.count) / sum))
    in column [spacing 10] [
        poll.title |> multiLineQuotedText |> el [Font.bold] |> postBodyStyle
        , column [spacing 5]
          (percentages
            |> List.map (\(PollOption opt, percentage) -> (opt ++ ": " ++ (String.fromFloat percentage) ++ "%")
                |> multiLineQuotedText
                |> postBodyStyle))
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
        Input.multiline [width fill, Font.size 11] {
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
            , renderCommentDate tmstp message.timestamp |> postFooterStyle |> space 5
            , renderFlagAction cache message |> space 20
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