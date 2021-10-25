module View.WelcomeWallScreen exposing (welcomeWallScreen)


import Element exposing (Element, alignLeft, column, el, fill, padding, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import State.AppState exposing (AppState, Display(..))
import Update.Msg exposing (Msg(..))
import Utils.TextUtils exposing (oneline)
import View.Icons as Icons
import View.Style exposing (horizontalSeparator, internalPageLinkStyle, leftGap, relFontSize, titledElementStyle, titledParagraphStyle, titledTextStyle, topGap)
import View.Theme
import View.UIStyle exposing (UIStyle)


welcomeWallScreen: AppState -> Element Msg
welcomeWallScreen state =
    column [width fill, spacing 15] [
        horizontalSeparator 1 state.uiStyle.theme.background |> topGap 15
        , titledText state.uiStyle "There is no post on your wall ..."
            """Not a problem! When you are ready, you can easily create a post, a poll, a challenge or an event using one of below wizard."""
        , titledElementStyle state.uiStyle "Create your first post" (column [width fill, spacing 5][
            bullet state.uiStyle (Icons.tip state.uiStyle.small) (internalPageLinkStyle WizardNewTipPage "Create a new tip for your followers") |> leftGap 10
            , bullet state.uiStyle (Icons.post state.uiStyle.small) (internalPageLinkStyle WizardNewFreePostPage "Create a blog post") |> leftGap 10
            , bullet state.uiStyle (Icons.challenge state.uiStyle.small) (internalPageLinkStyle WizardNewChallengePage "Challenge yourself or your followers") |> leftGap 10
            , bullet state.uiStyle (Icons.poll state.uiStyle.small) (internalPageLinkStyle WizardNewPollPage "Create a new poll") |> leftGap 10
            , bullet state.uiStyle (Icons.event state.uiStyle.small) (internalPageLinkStyle WizardNewEventPage "Organize a new event") |> leftGap 10
            , bullet state.uiStyle (Icons.repost state.uiStyle.small) ("... or you simply share with your followers something interesting you found" |> text)  |> leftGap 10
        ])
        , titledMultiText state.uiStyle "User rank" [
            """When a new user is joining GreenGen, it starts at the first rank - GreenWood"""
            , """With time, posts written, challenges taken, followers acquired, events participation or organized, the number of points increases.
                 Your rank will increase, and your actions on GreenGen will have more weight,
                 your posts will become more visible to others and your action cap will go up.
              """
            , """Maybe you start being a 'GreenWood', but with a steady usage of the website
                 you can soon become an 'Influencer', an 'Evangelist', or even a 'Sensei' or a 'Guru' -
                 and proudly show off the world your dedication to a better future :)"""
        ]
    ]

-- Helpers

titledText: UIStyle -> String -> String -> Element Msg
titledText ui title content = titledTextStyle ui title (content |> oneline)

titledMultiText: UIStyle -> String -> List String -> Element Msg
titledMultiText ui title content = titledParagraphStyle ui title (content |> List.map oneline)

bullet: UIStyle -> Element Msg -> Element Msg -> Element Msg
bullet ui icon elt = row [width fill, spacing 5] [
    icon |> el [padding 2, Font.color ui.theme.foreground, Background.color ui.theme.background, Border.rounded 2, alignLeft],
    el [relFontSize ui 2, Font.regular] elt
 ]