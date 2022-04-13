module Tagger where

import Data.Maybe
import Data.Symbol

import Quickstrom

-- STARTING POINT

readyWhen :: Selector
readyWhen = "#title"

-- ACTIONS

register :: String -> String -> ProbabilisticAction
register username password =
               focus "#anonymous #register input.username"
  `followedBy` enterText username
  `followedBy` focus "#anonymous #register input.password"
  `followedBy` enterText password
  `followedBy` click "#anonymous #register div.button"

login :: String -> String -> ProbabilisticAction
login username password =
               focus "#anonymous #login input.username"
  `followedBy` enterText username
  `followedBy` focus "#anonymous #login input.password"
  `followedBy` enterText password
  `followedBy` click "#anonymous #login div.button"

filterByTag :: String -> ProbabilisticAction
filterByTag tag =
               focus "#logged #filter-by-tag input"
  `followedBy` enterText tag
  `followedBy` click "#logged #filter-by-tag .button"

removeTag :: ProbabilisticAction
removeTag = click "#logged .tag .remove"

addNewTag :: String -> ProbabilisticAction
addNewTag tag =
               focus "#logged #new-tag input"
  `followedBy` enterText tag
  `followedBy` click "#logged #new-tag .button"

addNewContent :: String -> ProbabilisticAction
addNewContent content =
               focus "#logged input#new-content"
  `followedBy` enterText content

submitContent :: ProbabilisticAction
submitContent = click "#logged #add-content > .button"

actions :: Actions
actions =
  [ register "username" "password"
  , register "otheruser" "otherpassword"
  , login "username" "password"
  , login "username" "wrongpassword"
  , login "nonexistinguser" "password"
  , filterByTag "tag1"
  , filterByTag "tag2"
  , filterByTag "tag3"
  , removeTag
  , addNewTag "tag1"
  , addNewTag "tag2"
  , addNewTag "tag3"
  , addNewContent "content1"
  , addNewContent "content2"
  , addNewContent "content3"
  , submitContent
  ]

-- MODEL

type Tag = String

type Content = {content :: String, tags :: Array Tag}

contentRow :: Attribute "content-row"
contentRow = attribute (SProxy :: SProxy "content-row")

extractTags :: String -> Array Tag
extractTags i = map _.textContent (queryAll ("#logged #contents-table [tag-row=\" <> i <> \"]") {textContent})

extractContents :: Array Content
extractContents = map
  (\r -> {content : r.textContent, tags : extractTags (fromMaybe "" r.contentRow)})
  (queryAll "#logged #contents-table [content-row]" {textContent, contentRow})

-- INVARIANTS

proposition :: Boolean
proposition = titleIsTagger && isAnonymous && always (remainAmonymous || logIn || remainLogged)
  where
    remainAmonymous :: Boolean
    remainAmonymous = isAnonymous && next isAnonymous

    logIn :: Boolean
    logIn = isAnonymous && next isLogged

    remainLogged :: Boolean
    remainLogged = isLogged && next isLogged

titleIsTagger :: Boolean
titleIsTagger = always (title == Just "Tagger")
  where
    title = map _.textContent (queryOne "#title" {textContent})

isAnonymous :: Boolean
isAnonymous = isJust (queryOne "#anonymous" {})

isLogged :: Boolean
isLogged = isJust (queryOne "#logged" {})
