module View.PostRenderer exposing (..)


import Data.Challenge as Challenge
import Data.Post as Post exposing (Post, PostContent(..), PostId(..), Source(..))
import Data.Schedule exposing (UTCTimestamp(..))
import Data.Tip as Tip exposing (Tip)
import Data.Url exposing (Url(..))
import Data.User as User exposing (UserId(..))
import Element exposing (Element, alignLeft, alignRight, alignTop, centerX, centerY, column, el, fill, height, padding, paragraph, px, row, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Query.QueryUtils exposing (baseUrl)
import State.Cache as Cache exposing (Cache)
import Update.Msg exposing (Msg)
import Utils.DateUtils as DateUtils
import Uuid
import View.Icons as Icons
import View.ScreenUtils exposing (neverElement)
import View.Style exposing (..)

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
        Just challenge -> challenge.content |> text |> postBodyStyle
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
            , el [alignRight] ((if hasLiked then unlikeButtonStyle else likeButtonStyle) post.id)
             , el [alignRight] ((if isPinned then unpinButtonStyle else pinButtonStyle) post.id)
            , el [alignRight] (renderDate tmstp post.created)]
            |> postHeaderStyle

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
