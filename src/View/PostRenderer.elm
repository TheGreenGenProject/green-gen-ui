module View.PostRenderer exposing (..)


import Basics as Int
import Data.Challenge as Challenge exposing (ChallengeStatistics, SuccessMeasure)
import Data.Post as Post exposing (Post, PostContent(..), PostId(..), Source(..))
import Data.Schedule exposing (UTCTimestamp(..))
import Data.Tip as Tip exposing (Tip)
import Data.Url exposing (Url(..))
import Data.User as User exposing (UserId(..))
import Element exposing (Element, alignLeft, alignRight, alignTop, centerX, centerY, column, el, fill, height, padding, paddingEach, paragraph, px, row, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Query.QueryUtils exposing (baseUrl)
import State.AppState exposing (Display(..))
import State.Cache as Cache exposing (Cache)
import Update.Msg exposing (Msg(..))
import Utils.DateUtils as DateUtils
import Uuid
import View.Chart.Donut as Donut
import View.Icons as Icons
import View.ScreenUtils exposing (neverElement)
import View.Style exposing (..)
import View.Theme exposing (background, darkGreen, darkRed, green, lightGrey, lightOrange, lightPurple, lightRed, orange)

renderPostId: UTCTimestamp -> Cache -> PostId -> Element Msg
renderPostId tmstp cache postId = postId
    |> Cache.getPost cache
    |> Maybe.map (renderPost tmstp cache)
    |> Maybe.withDefault ("<Unable to render post " ++ (Post.toString postId) ++ ">" |> text)

renderPost: UTCTimestamp -> Cache -> Post -> Element Msg
renderPost tmstp cache post = column [width fill, alignLeft, spacing 5, padding 10 ]
    [renderHeader tmstp cache post, renderPostContent cache post, renderFooter cache post]

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
                    Just tip -> tip.content |> text |> postBodyStyle
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
            , verticalSeparator 5 background
            , column [spacing 10, alignTop, width fill] [
                challenge.title |> text |> el [Font.semiBold, Font.size 10]
                , challenge.content |> text |> postBodyStyle]
            , verticalSeparator 5 background
            , el [alignTop] (renderChallengeSuccessMeasure challenge.measure)
            ]
        Nothing        -> id |> Challenge.toString |> text |> postBodyStyle
    _                  -> neverElement

renderPollPost: Cache -> Post -> Element Msg
renderPollPost cache post = (text "Poll")

renderFreeTextPost: Cache -> Post -> Element Msg
renderFreeTextPost cache post = case post.content of
    FreeTextPost content sources -> content |> text |> postBodyStyle
    _                            -> neverElement

-- Helpers
renderHeader: UTCTimestamp -> Cache -> Post -> Element Msg
renderHeader tmstp cache post =
    let isFollowing = Cache.containsFollowingUser cache post.author
        hasLiked = Cache.containsLike cache post.id
        isPinned = Cache.containsPinned cache post.id
    in
        row [width fill, spacing 5] [
            el [alignLeft] (postLogo post)
            , el [alignLeft] (renderUser cache post.author)
            , el [alignLeft] ((if isFollowing then unfollowButtonStyle else followButtonStyle) post.author)
            , el [alignLeft, paddingEach {left=10,top=0,bottom=0,right=10}] (specialPostActions cache post)
            , el [alignRight] ((if hasLiked then unlikeButtonStyle else likeButtonStyle) post.id)
             , el [alignRight] ((if isPinned then unpinButtonStyle else pinButtonStyle) post.id)
            , el [alignRight] (renderDate tmstp post.created)]
            |> postHeaderStyle

specialPostActions: Cache -> Post -> Element Msg
specialPostActions _ post = case post.content of
    ChallengePost id -> buttonBarStyle [Font.size 9, Font.semiBold] [("View", DisplayPage (ChallengeDetailsPage id))]
    _                -> Element.none


renderFooter: Cache -> Post -> Element Msg
renderFooter cache post = let likes = Cache.getLikeCount cache post.id in
    (el [width fill]
        (row [width fill, spacing 5]
            ([el [alignLeft, alignTop, Font.italic, Font.size 9] ("Liked " ++ (String.fromInt likes) ++ " time(s)" |> text)] ++
            [paragraph [] (post.hashtags |> List.map (el [alignRight] << hashtagStyle))]))
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

postLogo: Post -> Element Msg
postLogo post =
    let render = el [width <| px 20, height <| px 20, Border.width 2, Border.rounded 2, centerX, centerY]
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