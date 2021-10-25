module View.LoginScreen exposing (loginScreen, loginFailedScreen)

import Data.Hash as Hash exposing (Hash(..))
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (labelLeft)
import Query.QueryUtils exposing (errorToString)
import State.AppState exposing (AppState, AuthError(..), Display(..))
import State.UserState exposing (UserState(..))
import Update.Msg exposing (Msg(..))
import View.Icons as Icons
import View.Style exposing (errorTextStyle, internalPageLinkStyle, placeholderStyle, relFontSize)
import View.Theme

loginScreen: AppState -> Element Msg
loginScreen state = column [centerX, centerY, spacing 30, padding 20]
        [ column [
            centerX, centerY,
            spacing 10,
            padding 10,
            width <| maximum 500 fill,
            Border.rounded 20,
            Background.color state.uiStyle.theme.background,
            Font.color state.uiStyle.theme.foreground] (loginComponents state)
         , signUpLink state.uiStyle]

loginFailedScreen: AuthError -> AppState -> Element Msg
loginFailedScreen err state = column [
        centerX,
        centerY,
        spacing 10,
        padding 10,
        width <| maximum 500 fill,
        Border.rounded 20,
        Background.color state.uiStyle.theme.background,
        Font.color state.uiStyle.theme.foreground ]
    ((errorMessage err |> errorTextStyle state.uiStyle) :: (loginComponents state))


-- Widgets for the login screen
loginComponents : AppState -> List (Element Msg)
loginComponents state = [
        (Input.username [Background.color state.uiStyle.theme.textFieldBackground, Font.color state.uiStyle.theme.textFieldForeground] {
            onChange = updateUsername state
            ,text = readUsername state
            ,placeholder = placeholderStyle state.uiStyle "Username"
            ,label = (labelLeft [centerX, centerY] (Icons.user state.uiStyle.normal))
        })
        ,(Input.currentPassword [Background.color state.uiStyle.theme.textFieldBackground, Font.color state.uiStyle.theme.textFieldForeground] {
            onChange = updatePassword state
            ,text = readPassword state
            ,placeholder = placeholderStyle state.uiStyle "Password"
            ,label = (labelLeft [centerX, centerY] (Icons.password state.uiStyle.normal))
            ,show = False
        })
        ,(Input.button [alignRight
            , Border.width 2
            , Border.rounded 5
            , padding 5
            , Background.color state.uiStyle.theme.appBackground
            , Font.color state.uiStyle.theme.enabledButton] {
            onPress = attemptLogin state
            ,label = (text "Login")
        })
    ]

signUpLink ui = paragraph [width fill, centerX, spacing 5, relFontSize ui 3,  Font.italic, Font.color ui.theme.background] [
    "If you don't have yet an account, please use our " |> text
    , internalPageLinkStyle RegistrationPage "Sign-Up"
    , " page instead." |> text
 ]

-- Helpers

attemptLogin: AppState -> Maybe Msg
attemptLogin state = case state.user of
    LoggingIn { email, password } -> Just (Logon (Hash.md5 email) (Hash.md5 password))
    _ -> Nothing

readUsername: AppState -> String
readUsername state = case state.user of
    LoggingIn { email } -> email
    _ -> "@green-gen.org"

updateUsername: AppState -> String -> Msg
updateUsername state user = case state.user of
    LoggingIn { password } -> EnteringCredentials user password
    _ -> EnteringCredentials user ""

readPassword: AppState -> String
readPassword state = case state.user of
    LoggingIn {password} -> password
    _ -> ""

updatePassword: AppState -> String -> Msg
updatePassword state pw = case state.user of
    LoggingIn { email } -> EnteringCredentials email pw
    _ -> EnteringCredentials "" pw

errorMessage: AuthError -> String
errorMessage err = case err of
    HttpError error      -> "Error Http" ++ (errorToString error)
    AuthenticationFailed -> "Authentication failed"



