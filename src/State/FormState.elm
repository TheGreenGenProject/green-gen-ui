module State.FormState exposing (..)


import Data.Challenge exposing (SuccessMeasure)
import Data.Poll exposing (PollOption)
import Data.Post exposing (Source)
import Data.Schedule exposing (UTCTimestamp)
import Data.User exposing (UserId)


type alias FormState = {
    newTipWizard: NewTipWizardState,
    newFreeTextWizard: NewFreeTextWizardState,
    newChallengeWizard: NewChallengeWizardState,
    newPollWizard: NewPollWizardState
 }

empty: FormState
empty = {
    newTipWizard       = emptyTipWizard,
    newFreeTextWizard  = emptyFreeTextWizard,
    newChallengeWizard = emptyChallengeWizard,
    newPollWizard = emptyPollWizard
 }


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

