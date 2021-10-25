module View.RegistrationScreen exposing (registrationScreen)

import Data.VerificationCode exposing (VerificationCode(..), updateSection)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (labelHidden, labelLeft)
import Query.QueryUtils exposing (errorToString)
import State.AppState exposing (AppState, AuthError(..), Display(..))
import State.FormState exposing (RegistrationSubmissionStage(..))
import Update.Msg exposing (Msg(..))
import Utils.MaybeUtils as MaybeUtils
import Utils.TextUtils as TextUtils
import View.Icons as Icons
import View.Style as Style exposing (internalPageLinkStyle, placeholderStyle, relFontSize)
import View.Theme exposing (darkRed)
import View.UIStyle exposing (UIStyle)


registrationScreen: AppState -> Element Msg
registrationScreen state = column [centerX, centerY, spacing 30, padding 20] [
    "Create a new account" |> text |> el [centerX, relFontSize state.uiStyle 15, Font.color state.uiStyle.theme.background, Font.semiBold]
    , column [
        centerX, centerY,
        spacing 10, padding 10,
        Border.rounded 20,
        width <| maximum 500 fill,
        Background.color state.uiStyle.theme.background,
        Font.color state.uiStyle.theme.foreground ] (
            if isSubmissionPhase state then registrationComponents state
            else if isVerificationPhase state then verificationComponents state
            else if isVerificationSuccessful state then createdAccountComponents state
            else registrationComponents state)
    , signIngLink state.uiStyle
 ]

registrationComponents : AppState -> List (Element Msg)
registrationComponents state = [
        row [width fill, spacing 5] [
            (Input.username [Font.color state.uiStyle.theme.textFieldForeground
                             , Background.color state.uiStyle.theme.foreground
                             , Border.width 1
                             , Border.color (if checkEmail state then state.uiStyle.theme.background else darkRed)] {
                onChange = updateEmail state
                ,text = readEmail state
                ,placeholder = placeholderStyle state.uiStyle "Email"
                ,label = (labelLeft [centerX, centerY, Font.color state.uiStyle.theme.foreground] (Icons.email state.uiStyle.normal))
            })
        , state.forms.registrationForm.email |> Maybe.map(\_ -> checkEmail state) |> validityIcon state.uiStyle]
        , row [width fill, spacing 5] [
            Input.text [Font.color state.uiStyle.theme.textFieldForeground
                        , Background.color state.uiStyle.theme.foreground
                        , Border.width 1
                        , Border.color (if checkPseudo state then state.uiStyle.theme.background else darkRed)] {
                onChange = updatePseudo state
                ,text = readPseudo state
                ,placeholder = placeholderStyle state.uiStyle "Pseudo"
                ,label = (labelLeft [centerX, centerY, Font.color state.uiStyle.theme.foreground] (Icons.user state.uiStyle.normal))
            }
            , serverSidePseudoCheck state |> validityIcon state.uiStyle]
        , row [width fill, spacing 5] [
            Input.currentPassword [Font.color state.uiStyle.theme.textFieldForeground
                                   , Background.color state.uiStyle.theme.foreground
                                   , Border.width 1
                                   , Border.color (if checkPassword state then state.uiStyle.theme.background else darkRed)] {
                onChange = updatePassword state
                ,text = readPassword state
                ,placeholder = placeholderStyle state.uiStyle "Password"
                ,label = (labelLeft [centerX, centerY, Font.color state.uiStyle.theme.foreground] (Icons.password state.uiStyle.normal))
                ,show = False
            }
            , state.forms.registrationForm.password |> Maybe.map(\_ -> checkPassword state) |> validityIcon state.uiStyle]
        , row [width fill, spacing 5] [
            Input.currentPassword [Font.color state.uiStyle.theme.textFieldForeground
                                   , Background.color state.uiStyle.theme.foreground
                                   , Border.width 1
                                   , Border.color (if checkPasswordVerification state then state.uiStyle.theme.background else darkRed)] {
                onChange = updatePasswordVerification state
                ,text = readPasswordVerification state
                ,placeholder = placeholderStyle state.uiStyle "Retype your password"
                ,label = (labelLeft [centerX, centerY, Font.color state.uiStyle.theme.foreground] (Icons.password state.uiStyle.normal))
                ,show = False
            }
            , state.forms.registrationForm.passwordVerification |> Maybe.map(\_ -> checkPasswordVerification state) |> validityIcon state.uiStyle]
        , row [width fill] [
            "Introduce yourself" |> text |> el [width fill, alignLeft, Font.color state.uiStyle.theme.foreground]
            , state.forms.registrationForm.introduction |> Maybe.map(\_ -> checkIntroduction state) |> validityIcon state.uiStyle |> el [alignRight]
         ]
        ,(Input.multiline [Font.color state.uiStyle.theme.textFieldForeground
                           , height <| px 150
                           , Background.color state.uiStyle.theme.foreground
                           , Border.width 1
                           , Border.color (if checkIntroduction state then state.uiStyle.theme.background else darkRed)] {
            onChange = updateIntroduction state
            ,text = readIntroduction state
            ,placeholder = placeholderStyle state.uiStyle "Tell others who you are ..."
            ,label = labelHidden "Introduction"
            ,spellcheck = True
        })
        ,row [width fill] [
            if isFailedRegistrationSubmission state
            then "Registration failed" |> text |> el [Font.color darkRed, alignLeft]
            else Style.empty
            ,Input.button [alignRight
                , Border.width 2
                , Border.rounded 5
                , padding 5
                , fontColor state.uiStyle (checkRegistrationForm state)
                , Background.color state.uiStyle.theme.foreground] {
            onPress = if checkRegistrationForm state then Just RegisterNewAccount else Nothing
            , label = (text "Sign-up now !")
        }]
 ]

verificationComponents: AppState -> List (Element Msg)
verificationComponents state = [
    paragraph [Font.color state.uiStyle.theme.foreground] [
        "Please enter the verification code we just sent you" |> text]
    , wrappedRow [width fill, centerX, spacing 20] [
        Input.text [Font.color state.uiStyle.theme.textFieldForeground
                    , Background.color state.uiStyle.theme.foreground
                    , width <| px 100] {
            onChange = updateVerificationCode state 1
            , text = readVerificationCodeSection state 1
            , placeholder = placeholderStyle state.uiStyle "0000"
            , label = labelHidden "Section1"
        }
        , Input.text [Font.color state.uiStyle.theme.textFieldForeground
                      , Background.color state.uiStyle.theme.foreground
                      , width <| px 100] {
            onChange = updateVerificationCode state 2
            , text = readVerificationCodeSection state 2
            , placeholder = placeholderStyle state.uiStyle "0000"
            , label = labelHidden "Section2"
        }
        , Input.text [Font.color state.uiStyle.theme.textFieldForeground
                      , Background.color state.uiStyle.theme.foreground
                      , width <| px 100] {
            onChange = updateVerificationCode state 3
            , text = readVerificationCodeSection state 3
            , placeholder = placeholderStyle state.uiStyle "0000"
            , label = labelHidden "Section3"
        }]
    , row [width fill] [
        if isFailedVerification state
        then "Verification failed. Please try again" |> text |> el [Font.color darkRed, alignLeft]
        else Style.empty
       , Input.button [alignRight
            , Border.width 2
            , Border.rounded 5
            , padding 5
            , fontColor state.uiStyle (checkVerificationCodeForm state)
            , Background.color state.uiStyle.theme.foreground] {
        onPress = if checkVerificationCodeForm state then Just VerifyAccount else Nothing
        ,label = (text "Verify" |> el [fontColor state.uiStyle True])
    }]
 ]

createdAccountComponents: AppState -> List (Element Msg)
createdAccountComponents state = [
    "Congratulations ! You are all set-up !" |> text
    ,(Input.button [alignRight, Border.width 2, Border.rounded 5, padding 5, Background.color state.uiStyle.theme.foreground, fontColor state.uiStyle True] {
        onPress = DisplayPage LoginPage |> Just
        ,label = (text "Go to Sign-in page")
    })
 ]


-- Helpers

fontColor: UIStyle -> Bool -> Attribute msg
fontColor ui b = Font.color (if b then ui.theme.enabledButton else ui.theme.disabledButton)

readEmail: AppState -> String
readEmail state =
    state.forms.registrationForm.email |> Maybe.withDefault ""

updateEmail: AppState -> String -> Msg
updateEmail state email =
    let registrationState = state.forms.registrationForm
    in {registrationState| email = MaybeUtils.maybeString email }
        |> FillingRegistrationForm

checkEmail: AppState -> Bool
checkEmail state = case state.forms.registrationForm.email of
    Just email -> (String.length email > 5) && (String.contains "@" email) && (not (String.contains " " email))
    Nothing    -> True

readPseudo: AppState -> String
readPseudo state =
    state.forms.registrationForm.pseudo |> Maybe.withDefault ""

updatePseudo: AppState -> String -> Msg
updatePseudo state pseudo =
    let registrationState = state.forms.registrationForm
    in {registrationState| pseudo = MaybeUtils.maybeString pseudo, checkingPseudo = Just True }
        |> FillingRegistrationForm

checkPseudo: AppState -> Bool
checkPseudo state = case state.forms.registrationForm.pseudo of
    Just pseudo -> String.length pseudo >= 3 &&
        String.length pseudo <= 15 &&
        serverSidePseudoCheck state == Just True
    Nothing     -> True

serverSidePseudoCheck: AppState -> Maybe Bool
serverSidePseudoCheck state = case state.forms.registrationForm.pseudo of
    Just _   -> state.forms.registrationForm.serverValidatedPseudo
    Nothing  -> Nothing

readPassword: AppState -> String
readPassword state =
    state.forms.registrationForm.password |> Maybe.withDefault ""

updatePassword: AppState -> String -> Msg
updatePassword state pw =
    let registrationState = state.forms.registrationForm
    in {registrationState| password = MaybeUtils.maybeString pw }
        |> FillingRegistrationForm

checkPassword: AppState -> Bool
checkPassword state = case state.forms.registrationForm.password of
    Just password -> String.length password > 5
    Nothing       -> True

readPasswordVerification: AppState -> String
readPasswordVerification state =
    state.forms.registrationForm.passwordVerification |> Maybe.withDefault ""

updatePasswordVerification: AppState -> String -> Msg
updatePasswordVerification state pw =
    let registrationState = state.forms.registrationForm
    in {registrationState| passwordVerification = MaybeUtils.maybeString pw }
        |> FillingRegistrationForm

checkPasswordVerification: AppState -> Bool
checkPasswordVerification state = case state.forms.registrationForm.passwordVerification of
    Just _   -> state.forms.registrationForm.password == state.forms.registrationForm.passwordVerification
    Nothing  -> True

readIntroduction: AppState -> String
readIntroduction state =
    state.forms.registrationForm.introduction |> Maybe.withDefault ""

updateIntroduction: AppState -> String -> Msg
updateIntroduction state intro =
    let registrationState = state.forms.registrationForm
    in {registrationState| introduction = MaybeUtils.maybeString intro }
        |> FillingRegistrationForm

checkIntroduction: AppState -> Bool
checkIntroduction state = case state.forms.registrationForm.introduction of
    Just intro -> String.length intro > 0 && String.length intro <= 5000
    Nothing    -> True

checkRegistrationForm: AppState -> Bool
checkRegistrationForm state =
    (checkEmail state && MaybeUtils.nonEmpty state.forms.registrationForm.email) &&
    (checkPseudo state && MaybeUtils.nonEmpty state.forms.registrationForm.pseudo &&
      state.forms.registrationForm.serverValidatedPseudo == Just True ) &&
    (checkPassword state && MaybeUtils.nonEmpty state.forms.registrationForm.password) &&
    (checkPasswordVerification state && MaybeUtils.nonEmpty state.forms.registrationForm.passwordVerification) &&
    (checkIntroduction state && MaybeUtils.nonEmpty state.forms.registrationForm.introduction)

checkVerificationCodeForm: AppState -> Bool
checkVerificationCodeForm state = case state.forms.registrationForm.verification of
    Just (VerificationCode s1 s2 s3) -> s1 >=0 && s1 <= 9999 && s2 >=0 && s2 <= 9999 && s3 >=0 && s3 <= 9999
    Nothing                          -> False

errorMessage: AuthError -> String
errorMessage err = case err of
    HttpError error      -> "Error Http" ++ (errorToString error)
    AuthenticationFailed -> "Authentication failed"

readVerificationCodeSection: AppState -> Int -> String
readVerificationCodeSection state section =
    case state.forms.registrationForm.verification of
        Just (VerificationCode s1 s2 s3) ->
            if section == 1 then TextUtils.format4Digits s1
            else if section == 2 then TextUtils.format4Digits s2
            else TextUtils.format4Digits s3
        Nothing -> ""

updateVerificationCode: AppState -> Int -> String -> Msg
updateVerificationCode state section codeStr =
    let registrationState = state.forms.registrationForm
        code = codeStr |> String.toInt |> Maybe.withDefault 0
        verifCode = registrationState.verification |> Maybe.withDefault (VerificationCode 0 0 0)
    in {registrationState| verification = updateSection verifCode section code |> Just }
        |> FillingRegistrationForm

signIngLink ui = paragraph [width fill, centerX, spacing 5, relFontSize ui 3,  Font.italic, Font.color ui.theme.background] [
    "If you already have an account with us, please use our " |> text
    , internalPageLinkStyle LoginPage "Sign-in"
    , " page instead." |> text
 ]

validityIcon: UIStyle -> Maybe Bool -> Element Msg
validityIcon ui flag = case flag of
    Just True  -> Icons.valid ui.normal |> el [Font.color ui.theme.foreground]
    Just False -> Icons.invalid ui.normal |> el [Font.color ui.theme.foreground]
    Nothing    -> Element.none

isFailedRegistrationSubmission: AppState -> Bool
isFailedRegistrationSubmission state = let submissionState = state.forms.registrationForm.submissionState in
    submissionState == RegistrationSubmissionFailed

isSubmissionPhase: AppState -> Bool
isSubmissionPhase state = let submissionState = state.forms.registrationForm.submissionState in
    submissionState == FillingNewRegistration ||
    submissionState == SubmittingRegistration ||
    submissionState == RegistrationSubmissionFailed

isVerificationPhase: AppState -> Bool
isVerificationPhase state = let submissionState = state.forms.registrationForm.submissionState in
    submissionState == RegistrationSubmitted ||
    submissionState == SubmittingValidationCode ||
    submissionState == ValidationCodeFailed

isFailedVerification: AppState -> Bool
isFailedVerification state = let submissionState = state.forms.registrationForm.submissionState in
    submissionState == ValidationCodeFailed

isVerificationSuccessful: AppState -> Bool
isVerificationSuccessful state = let submissionState = state.forms.registrationForm.submissionState in
    submissionState == RegistrationSuccessful