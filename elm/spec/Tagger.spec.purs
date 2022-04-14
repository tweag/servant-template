module Tagger where

import Data.Array as Array
import Data.Maybe
import Data.String.CodeUnits as String
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
removeTag = click "#logged .removable .tag .remove"

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

-- QUERIES

contentRow :: Attribute "content-row"
contentRow = attribute (SProxy :: SProxy "content-row")

extractTags :: String -> Array Tag
extractTags i = map _.textContent (queryAll ("#logged #contents-table [tag-row=\" <> i <> \"]") {textContent})

extractContents :: Array Content
extractContents = map
  (\r -> {content : r.textContent, tags : extractTags (fromMaybe "" r.contentRow)})
  (queryAll "#logged #contents-table [content-row]" {textContent, contentRow})

extractFilters :: Array Tag
extractFilters = map _.textContent (queryAll "#logged #contents #filter-by-tag .tag" {textContent})

extractNewContent :: Maybe String
extractNewContent = map _.value (queryOne "#logged #add-content #new-content" {value})

extractNewTags :: Array Tag
extractNewTags = map _.textContent (queryAll "#logged #add-content #new-tag .tag" {textContent})

-- STATES

anonymous :: Maybe Unit
anonymous = map (const unit) (queryOne "#anonymous" {})

logged :: Maybe {filters :: Array Tag, contents :: Array Content, newContent :: String, newTags :: Array Tag}
logged = queryOne "#logged" {} *> (
      (\filters contents newContent newTags -> {filters : filters, contents : contents, newContent : newContent, newTags : newTags})
  <$> Just extractFilters
  <*> Just extractContents
  <*> extractNewContent
  <*> Just extractNewTags)

-- ASSERTIONS

titleIsTagger :: Boolean
titleIsTagger = always (title == Just "Tagger")
  where
    title = map _.textContent (queryOne "#title" {textContent})

isAnonymous :: Boolean
isAnonymous = isJust anonymous

isLogged :: Boolean
isLogged = isJust logged

-- TRANSITIONS

remainAmonymous :: Boolean
remainAmonymous = isAnonymous && next isAnonymous

logIn :: Boolean
logIn = isAnonymous && next isLogged

addFilter :: Boolean
addFilter
  =  Array.length extractFilters < next (Array.length extractFilters)
  && Array.length extractContents >= next (Array.length extractContents)
  && unchanged extractNewContent
  && unchanged extractNewTags

fillNewContent :: Boolean
fillNewContent
  =  map String.length extractNewContent < next (map String.length extractNewContent)
  && unchanged extractFilters
  && unchanged extractContents
  && unchanged extractNewTags

addNewContentTag :: Boolean
addNewContentTag
  =  Array.length extractNewTags < next (Array.length extractNewTags)
  && unchanged extractFilters
  && unchanged extractContents
  && unchanged extractNewContent

submitNewContent :: Boolean
submitNewContent
  =  next extractNewContent == Just ""
  && next extractNewTags == []
  && next (Array.length extractContents) == Array.length extractContents + 1
  && unchanged extractFilters

-- INVARIANTS

proposition :: Boolean
proposition
  =  titleIsTagger
  && isAnonymous
  && always
    (  remainAmonymous
    || logIn
    || addFilter
    || fillNewContent
    || addNewContentTag
    || submitNewContent
    )
