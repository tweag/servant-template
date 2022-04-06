module Tagger where

import Quickstrom

readyWhen :: Selector
readyWhen = "#title"

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
  ]

proposition :: Boolean
proposition = true
