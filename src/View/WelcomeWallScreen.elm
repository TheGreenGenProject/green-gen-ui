module View.WelcomeWallScreen exposing (welcomeWallScreen)


import Element exposing (Element, alignLeft, column, el, fill, padding, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import State.AppState exposing (AppState, Display(..))
import Update.Msg exposing (Msg(..))
import Utils.TextUtils exposing (oneline)
import View.Icons as Icons
import View.Style exposing (horizontalSeparator, internalPageLinkStyle, leftGap, titledElementStyle, titledParagraphStyle, titledTextStyle, topGap)
import View.Theme exposing (background, foreground)


welcomeWallScreen: AppState -> Element Msg
welcomeWallScreen state =
    column [width fill, spacing 15] [
        horizontalSeparator 1 background |> topGap 15
        , titledText "There is no post on your wall ..."
            """Not a problem! When you are ready, you can easily create a post, a poll, a challenge or an event using one of below wizard."""
        , titledElementStyle "Create your first post" (column [width fill, spacing 5][
            bullet (Icons.tip Icons.small) (internalPageLinkStyle WizardNewTipPage "Create a new tip for your followers") |> leftGap 10
            , bullet (Icons.post Icons.small) (internalPageLinkStyle WizardNewFreePostPage "Create a blog post") |> leftGap 10
            , bullet (Icons.challenge Icons.small) (internalPageLinkStyle WizardNewChallengePage "Challenge yourself or your followers") |> leftGap 10
            , bullet (Icons.poll Icons.small) (internalPageLinkStyle WizardNewPollPage "Create a new poll") |> leftGap 10
            , bullet (Icons.event Icons.small) (internalPageLinkStyle WizardNewEventPage "Organize a new event") |> leftGap 10
            , bullet (Icons.repost Icons.small) ("... or you simply share with your followers something interesting you found" |> text)  |> leftGap 10
        ]) 12
        , titledMultiText "User rank" [
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

titledText: String -> String -> Element Msg
titledText title content = titledTextStyle title (content |> oneline) 12

titledMultiText: String -> List String -> Element Msg
titledMultiText title content = titledParagraphStyle title (content |> List.map oneline) 12

bullet: Element Msg -> Element Msg -> Element Msg
bullet icon elt = row [width fill, spacing 5] [
    icon |> el [padding 2, Font.color foreground, Background.color background, Border.rounded 2, alignLeft],
    el [Font.size 12, Font.regular] elt
 ]