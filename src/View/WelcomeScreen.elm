module View.WelcomeScreen exposing (welcomeScreen)


import Element exposing (Element, alignLeft, centerX, centerY, column, el, fill, height, padding, paragraph, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Update.Msg exposing (Msg(..))
import Utils.TextUtils exposing (oneline)
import View.Icons as Icons
import View.Style exposing (horizontalSeparator, relFontSize, titledElementStyle, titledParagraphStyle, titledTextStyle)
import View.Theme
import View.UIStyle exposing (UIStyle)


welcomeScreen: UIStyle -> Element Msg
welcomeScreen ui = column [
    width fill
    , height fill
    , centerX
    , spacing 20
    , padding 5 ]
    [ welcome ui, renderWelcomePage ui]

renderWelcomePage: UIStyle -> Element Msg
renderWelcomePage ui = column [width fill, spacing 15] [
    whatIsGreenGen ui
    , howDoesItWork ui
    , userGuide ui
    , dataPrivacy ui
    , startNow ui
    , generateFeedButton ui |> el [centerX]
 ]

welcome: UIStyle -> Element Msg
welcome ui = column [width fill, padding 10, spacing 10] [
    el [relFontSize ui 22, Font.semiBold, centerX, Font.color ui.theme.background] ("Welcome !" |> text)
    , horizontalSeparator 1 ui.theme.background
 ]

whatIsGreenGen: UIStyle -> Element Msg
whatIsGreenGen ui = titledMultiText ui "What is GreenGen ?" [
    "GreenGen is a new social network which aims at helping people to do small efforts to improve environment."
    , "This version of GreenGen is a still a proof of concept. Expect bugs and issues."
 ]

isThatTheSolution: UIStyle -> Element Msg
isThatTheSolution ui = titledMultiText ui "Is GreenGen THE solution to the environment crisis ?" [
    """Certainly not :) The solution is obviously very complex, requires mainly drastic political changes,
       but starting to change bad habits and mentalities of each one of us is definitely a part of this solution."""
    , """We hope that helping you to change will become something contagious, and that people around you will start following your example."""
 ]

willYouBeAStar: UIStyle -> Element Msg
willYouBeAStar ui = titledText ui "Will you become a shiny and rich social media star with GreenGen ?"
    "No, go away !"

howDoesItWork: UIStyle -> Element Msg
howDoesItWork ui = titledMultiText ui "How does it work ?" [
    """GreenGen provides you with a number of tools to help you changing your behaviour,
       and with a community to support you with advices and keep you motivated.
       We believe small, sustained, regular steps will work better than introducing big changes in one go."""
    , """Post tips, challenge yourself and your followers, organize events, poll opinions ... we have the tools for that."""
 ]

userGuide: UIStyle -> Element Msg
userGuide ui = titledElementStyle ui "Quick user guide"
    (column [spacing 2] [
        bullet ui (Icons.wall ui.small) "Your home page. You can see all your posts here."
        , bullet ui (Icons.feed ui.small) "Your feed. All the new content of people or hashtags you are following should be there"
        , bullet ui (Icons.challenge ui.small) "Challenge page. You can see all your current challenges, and statuses."
        , bullet ui (Icons.calendar ui.small) "Your event page. This is where you can see your incoming events, or manage events you are organizing."
        , bullet ui (Icons.notifications ui.small) "Notifications from GreenGen will be there"
        , bullet ui (text "+" |> el [width <| px 16, height <| px 16, centerX, centerY])
            "And when you are ready ... go to our wizard page, and post something or even organize an event !"
  ])

dataPrivacy: UIStyle -> Element Msg
dataPrivacy ui = titledMultiText ui "Data privacy and protection" [
    """GreenGen's main goal is to help people to become more environment friendly.
       We are not a social media to profile you activity - we actually don't care who you are."""
    , """We store on our servers only the data we need for security purpose and to get the website running. We try to make your identification more difficult -
         so we are not an interesting purchase for any big social media companies or target for hackers."""
    , """You need an email to register, but we just use it to generate a deterministic id from it -
         we don't even keep it after your registration is completed. This means we cannot even contact you by email !"""
    , """We don't use tracking cookies. Actually, we don't use cookies at all."""
 ]

startNow: UIStyle -> Element Msg
startNow ui = titledText ui "Let's start now !"
    """Your feed is now empty. We will generate one randomly from recent posts from our users.
       You can also go to the search screen and look for topics you are interested in, follow hashtags and users.
       With time, your feed will improve.
    """

generateFeedButton: UIStyle -> Element Msg
generateFeedButton ui = el [centerX, width fill]
    (Input.button [
        padding 5
        , relFontSize ui 14
        , Font.color ui.theme.foreground
        , Background.color ui.theme.background
        , height fill
        , Border.color ui.theme.background
        , Border.width 1
        , Border.rounded 5]
        { onPress = Just GenerateWelcomeFeed
         , label = "Let's get started !" |> Element.text |> el [centerX] })


-- Helpers

titledText: UIStyle -> String -> String -> Element Msg
titledText ui title content = titledTextStyle ui title (content |> oneline)

titledMultiText: UIStyle -> String -> List String -> Element Msg
titledMultiText ui title content = titledParagraphStyle ui title (content |> List.map oneline)

bullet: UIStyle -> Element Msg -> String -> Element Msg
bullet ui icon txt = row [width fill, spacing 5] [
    icon |> el [padding 2, Font.color ui.theme.foreground, Background.color ui.theme.background, Border.rounded 2, alignLeft],
    paragraph [relFontSize ui 0] [txt |> text]
 ]