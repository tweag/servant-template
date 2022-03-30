module Tagger where

import Quickstrom

readyWhen :: Selector
readyWhen = "#title"

register :: String -> String -> ProbabilisticAction
register username password =
               focus "#register input[autocomplete=\"username\"]"
  `followedBy` enterText username
  `followedBy` focus "#register input[autocomplete=\"new-password\"]"
  `followedBy` enterText password
  `followedBy` click "#register div[role=\"button\"]"

login :: String -> String -> ProbabilisticAction
login username password =
               focus "#login input[autocomplete=\"username\"]"
  `followedBy` enterText username
  `followedBy` focus "#login input[autocomplete=\"new-password\"]"
  `followedBy` enterText password
  `followedBy` click "#login div[role=\"button\"]"

actions :: Actions
actions =
  [ register "username" "password"
  , register "otheruser" "otherpassword"
  , login "username" "password"
  , login "username" "wrongpassword"
  , login "nonexistinguser" "password"
  ]

proposition :: Boolean
proposition = true
