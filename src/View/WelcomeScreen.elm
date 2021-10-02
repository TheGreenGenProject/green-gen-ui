module View.WelcomeScreen exposing (welcomeScreen)


import Element exposing (Element, alignLeft, centerX, centerY, column, el, fill, height, padding, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Update.Msg exposing (Msg(..))
import Utils.TextUtils exposing (oneline)
import View.Icons as Icons
import View.Style exposing (titledElementStyle, titledParagraphStyle, titledTextStyle, horizontalSeparator)
import View.Theme exposing (background, foreground)


welcomeScreen: Element Msg
welcomeScreen = column [
    width fill
    , height fill
    , centerX
    , spacing 20
    , padding 5 ]
    [ welcome, renderWelcomePage ]

renderWelcomePage: Element Msg
renderWelcomePage = column [width fill, spacing 15] [
    whatIsGreenGen
    , howDoesItWork
    , userGuide
    , dataPrivacy
    , startNow
    , generateFeedButton |> el [centerX]
 ]

welcome: Element Msg
welcome = column [width fill, padding 10, spacing 10] [
    el [Font.size 32, Font.semiBold, centerX] ("Welcome to the GreenGen family !" |> text)
    , horizontalSeparator 1 background
 ]

whatIsGreenGen: Element Msg
whatIsGreenGen = titledMultiText "What is GreenGen ?" [
    "GreenGen is a new social network which aims at helping people to do small efforts to improve environment."
    , "This version of GreenGen is a still a proof of concept. Expect bugs and issues."
 ]

isThatTheSolution: Element Msg
isThatTheSolution = titledMultiText "Is GreenGen THE solution to the environment crisis ?" [
    """Certainly not :) The solution is obviously very complex, requires mainly drastic political changes,
       but starting to change bad habits and mentalities of each one of us is definitely a part of this solution."""
    , """We hope that helping you to change will become something contagious, and that people around you will start following your example."""
 ]

willYouBeAStar: Element Msg
willYouBeAStar = titledText "Will you become a shiny and rich social media star with GreenGen ?"
    "No, go away !"

howDoesItWork: Element Msg
howDoesItWork = titledMultiText "How does it work ?" [
    """GreenGen provides you with a number of tools to help you changing your behaviour,
       and with a community to support you with advices and keep you motivated.
       We believe small, sustained, regular steps will work better than introducing big changes in one go."""
    , """Post tips, challenge yourself and your followers, organize events, poll opinions ... we have the tools for that."""
 ]

userGuide: Element Msg
userGuide = titledElementStyle "Quick user guide"
    (column [spacing 2] [
        bullet (Icons.wall Icons.small) "Your home page. You can see all your posts here."
        , bullet (Icons.feed Icons.small) "Your feed. All the new content of people or hashtags you are following should be there"
        , bullet (Icons.challenge Icons.small) "Challenge page. You can see all your current challenges, and statuses."
        , bullet (Icons.calendar Icons.small) "Your event page. This is where you can see your incoming events, or manage events you are organizing."
        , bullet (Icons.notifications Icons.small) "Notifications from GreenGen will be there"
        , bullet (text "+" |> el [Font.semiBold, width <| px 16, height <| px 16, centerX, centerY])
            "And when you are ready ... go to our wizard page, and post something or even organize an event !"
  ]) 12

dataPrivacy: Element Msg
dataPrivacy = titledMultiText "Data privacy and protection" [
    """GreenGen's main goal is to help people to become more environment friendly.
       We are not a social media to profile you activity - we actually don't care who you are."""
    , """We store on our servers only the data we need for security purpose and to get the website running. We try to make your identification more difficult -
         so we are not an interesting purchase for any big social media companies or target for hackers."""
    , """You need an email to register, but we just use it to generate a deterministic id from it -
         we don't even keep it after your registration is completed. This means we cannot even contact you by email !"""
    , """We don't use tracking cookies. Actually, we don't use cookies at all."""
 ]

startNow: Element Msg
startNow = titledText "Let's start now !"
    """Your feed is now empty. We will generate one randomly from recent posts from our users.
       You can also go to the search screen and look for topics you are interested in, follow hashtags and users.
       With time, your feed will improve.
    """

generateFeedButton: Element Msg
generateFeedButton = el [centerX, width fill]
    (Input.button [
        padding 5
        , Font.size 24
        , Font.color foreground
        , Background.color background
        , height fill
        , Border.color background
        , Border.width 1
        , Border.rounded 5]
        { onPress = Just GenerateWelcomeFeed
         , label = "Let's get started !" |> Element.text |> el [centerX] })


-- Helpers

titledText: String -> String -> Element Msg
titledText title content = titledTextStyle title (content |> oneline) 12

titledMultiText: String -> List String -> Element Msg
titledMultiText title content = titledParagraphStyle title (content |> List.map oneline) 12

bullet: Element Msg -> String -> Element Msg
bullet icon txt = row [width fill, spacing 5] [
    icon |> el [padding 2, Font.color foreground, Background.color background, Border.rounded 2, alignLeft],
    el [Font.size 12] (txt |> text)
 ]