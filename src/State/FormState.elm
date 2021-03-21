module State.FormState exposing (..)


import Data.Post exposing (Source)
type alias FormState = {
    newTipWizard: NewTipWizardState,
    newFreeTextWizard: NewFreeTextWizardState
 }

empty: FormState
empty = {
    newTipWizard      = emptyTipWizard,
    newFreeTextWizard = emptyFreeTextWizard
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
