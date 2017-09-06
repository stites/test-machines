{-# LANGUAGE OverloadedStrings #-}
module Test.Machines.Utils.Dotfile where

import Prelude
import Data.Text
import Data.Monoid ((<>))

import Test.Machines

header :: Text -> Text
header name = intercalate "\n"
  [ "digraph " <> name <> " {"
  , "size = \"6,8.5\";"
  , "ratio = \"fill\";"
  , "node [shape = circle];"
  , "node [fontsize = 24];"
  , "edge [fontsize = 24];"
  ]

states :: [(s, s, Symbol s, Float)] -> Text
states = undefined
-- go ""
--   where
--     go :: Text -> [(s, s, Symbol s, Float)] -> Text
--     go acc [] = acc
--     go acc ((from, to, obs, p):ss) =
--       acc <> "\n  "
--       <> st2T from <> " -> " <> st2T to
--       <> " [label = \"" <> ts2T obs <> ": " <> (pack $ show p) <> "\"];"
--       <> (go acc ss)
--
-- dotFile name ss = header name <> "\n" <> states ss <> "\n}"
--
