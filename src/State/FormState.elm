module State.FormState exposing (..)


import Data.Challenge exposing (SuccessMeasure)
import Data.Hashtag exposing (Hashtag)
import Data.Poll exposing (PollOption)
import Data.Post exposing (PostId, Source)
import Data.Schedule exposing (UTCTimestamp)
import Data.User exposing (UserId)
import Data.VerificationCode exposing (VerificationCode)


type alias FormState = {
    registrationForm: RegistrationFormState,
    newTipWizard: NewTipWizardState,
    newRepostWizard: NewRepostWizardState,
    newFreeTextWizard: NewFreeTextWizardState,
    newChallengeWizard: NewChallengeWizardState,
    newPollWizard: NewPollWizardState
 }

empty: FormState
empty = {
    registrationForm   = emptyRegistrationForm,
    newTipWizard       = emptyTipWizard,
    newRepostWizard    = emptyRepostWizard,
    newFreeTextWizard  = emptyFreeTextWizard,
    newChallengeWizard = emptyChallengeWizard,
    newPollWizard      = emptyPollWizard
 }

-- Registration

type RegistrationSubmissionStage =
    FillingNewRegistration
    | SubmittingRegistration
    | RegistrationSubmitted
    | RegistrationSubmissionFailed
    | SubmittingValidationCode
    | ValidationCodeFailed
    | RegistrationSuccessful

type alias RegistrationFormState = {
    submissionState: RegistrationSubmissionStage,
    email: Maybe String,
    pseudo: Maybe String,
    password: Maybe String,
    passwordVerification: Maybe String,
    introduction: Maybe String,
    verification: Maybe VerificationCode,
    error: Maybe String,
    checkingPseudo: Maybe Bool,
    serverValidatedPseudo: Maybe Bool
 }

emptyRegistrationForm = {
    submissionState       = FillingNewRegistration,
    email                 = Nothing,
    pseudo                = Nothing,
    password              = Nothing,
    passwordVerification  = Nothing,
    introduction          = Nothing,
    verification          = Nothing,
    error                 = Nothing,
    checkingPseudo        = Nothing,
    serverValidatedPseudo = Nothing
 }

updateRegistrationFormState: FormState -> RegistrationFormState -> FormState
updateRegistrationFormState formState registrationForm = {formState |
    registrationForm = registrationForm }

checkingPseudoAvailability: FormState -> FormState
checkingPseudoAvailability formState =
    let registrationForm = formState.registrationForm in
    {formState | registrationForm = {registrationForm| checkingPseudo = Just True, serverValidatedPseudo = Nothing} }

pseudoAvailabilityChecked: FormState -> Bool -> FormState
pseudoAvailabilityChecked formState valid =
    let registrationForm = formState.registrationForm in
    {formState | registrationForm = {registrationForm| checkingPseudo = Just False, serverValidatedPseudo = Just valid} }

changeRegistrationState: RegistrationSubmissionStage -> FormState -> FormState
changeRegistrationState submissionState formState =
    let registrationForm = formState.registrationForm in
    {formState | registrationForm = {registrationForm| submissionState = submissionState} }

submittingRegistration: FormState -> FormState
submittingRegistration = changeRegistrationState SubmittingRegistration

registrationSubmitted: FormState -> FormState
registrationSubmitted = changeRegistrationState RegistrationSubmitted

registrationSubmissionFailed: FormState -> FormState
registrationSubmissionFailed = changeRegistrationState RegistrationSubmissionFailed

validatingAccount: FormState -> FormState
validatingAccount = changeRegistrationState SubmittingValidationCode

accountVerified: FormState -> FormState
accountVerified = changeRegistrationState RegistrationSuccessful

accountVerificationFailed: FormState -> FormState
accountVerificationFailed = changeRegistrationState ValidationCodeFailed

-- Tips

type alias NewTipWizardState = {
    posting: Bool,
    content: Maybe String,
    sources: List Source
 }

emptyTipWizard = {
    posting = False,
    content = Nothing,
    sources = []
 }

updateNewTipWizardState: FormState -> NewTipWizardState -> FormState
updateNewTipWizardState formState newTipState = {formState |
    newTipWizard = newTipState }

clearNewTipWizardState: FormState -> FormState
clearNewTipWizardState formState = {formState |
    newTipWizard = emptyTipWizard }

postingNewTip: FormState -> FormState
postingNewTip formState = let wizard = formState.newTipWizard in
    {formState | newTipWizard = {wizard| posting = True} }

newTipPosted: FormState -> FormState
newTipPosted formState = let wizard = formState.newTipWizard in
    {formState | newTipWizard = {wizard| posting = False} }

-- Repost

type alias NewRepostWizardState = {
    posting: Bool,
    repost: Maybe PostId
 }

emptyRepostWizard = {
    posting  = False,
    repost   = Nothing
 }

repost: FormState -> PostId -> FormState
repost formState postId =
    let repostState = formState.newRepostWizard
        newRepostState = {repostState| repost = Just postId }
    in {formState | newRepostWizard = newRepostState }

clearNewRepostWizardState: FormState -> FormState
clearNewRepostWizardState formState = {formState |
    newRepostWizard = emptyRepostWizard }

reposting: FormState -> FormState
reposting formState = let wizard = formState.newRepostWizard in
    {formState | newRepostWizard = {wizard| posting = True} }

reposted: FormState -> FormState
reposted formState = let wizard = formState.newRepostWizard in
    {formState | newRepostWizard = {wizard| posting = False} }

-- Free Text

type alias NewFreeTextWizardState = {
    posting: Bool,
    title: Maybe String,
    content: Maybe String,
    sources: List Source
 }

emptyFreeTextWizard = {
    posting = False,
    title = Nothing,
    content = Nothing,
    sources = []
 }

updateNewFreeTextWizardState: FormState -> NewFreeTextWizardState -> FormState
updateNewFreeTextWizardState formState newFreeTextState = {formState |
    newFreeTextWizard = newFreeTextState }

clearNewFreeTextWizardState: FormState -> FormState
clearNewFreeTextWizardState formState = {formState |
    newFreeTextWizard = emptyFreeTextWizard }

postingNewFreeText: FormState -> FormState
postingNewFreeText formState = let wizard = formState.newFreeTextWizard in
    {formState | newFreeTextWizard = {wizard| posting = True} }

newFreeTextPosted: FormState -> FormState
newFreeTextPosted formState = let wizard = formState.newFreeTextWizard in
    {formState | newFreeTextWizard = {wizard| posting = False} }


-- Challenges

type Audience = Followers | Specific (List UserId)
type ReportPeriod = Daily | Weekly

type alias NewChallengeWizardState = {
    posting: Bool,
    title: Maybe String,
    content: Maybe String,
    start: Maybe UTCTimestamp,
    end: Maybe UTCTimestamp,
    reportPeriod: ReportPeriod,
    audience: Audience,
    successMeasure: SuccessMeasure
 }

emptyChallengeWizard = {
    posting = False,
    title = Nothing,
    start = Nothing,
    end = Nothing,
    reportPeriod = Daily,
    content = Nothing,
    audience = Followers,
    successMeasure = {
        maxFailure = 0,
        maxPartial = 0,
        maxSkip    = 0
    }
 }

updateNewChallengeWizardState: FormState -> NewChallengeWizardState -> FormState
updateNewChallengeWizardState formState newChallengeState = {formState |
    newChallengeWizard = newChallengeState }

clearNewChallengeWizardState: FormState -> FormState
clearNewChallengeWizardState formState = {formState |
    newChallengeWizard = emptyChallengeWizard }

postingNewChallenge: FormState -> FormState
postingNewChallenge formState = let wizard = formState.newChallengeWizard in
    {formState | newChallengeWizard = {wizard| posting = True} }

newChallengePosted: FormState -> FormState
newChallengePosted formState = let wizard = formState.newChallengeWizard in
    {formState | newChallengeWizard = {wizard| posting = False} }


-- Polls

type alias NewPollWizardState = {
    posting: Bool,
    question: Maybe String,
    options: Maybe (List PollOption)
 }

emptyPollWizard: NewPollWizardState
emptyPollWizard = {
    posting = False,
    question = Nothing,
    options = Nothing
 }

clearNewPollWizardState: FormState -> FormState
clearNewPollWizardState formState = {formState| newPollWizard = emptyPollWizard }

updateNewPollWizardState: FormState -> NewPollWizardState -> FormState
updateNewPollWizardState formState newPollState = {formState |
    newPollWizard = newPollState }

postingNewPoll: FormState -> FormState
postingNewPoll formState = let wizard = formState.newPollWizard in
    {formState | newPollWizard = {wizard| posting = True} }

newPollPosted: FormState -> FormState
newPollPosted formState = let wizard = formState.newPollWizard in
    {formState | newPollWizard = {wizard| posting = False} }

