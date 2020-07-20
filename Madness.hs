-- https://en.wikipedia.org/wiki/Mad_Libs

module Madness where

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin :: Exclamation
          -> Adverb
          -> Noun
          -> Adjective
          -> String
madlibbin e adv noun adj =
  mconcat [e
          ,"! he said "
          ,adv
          ," as he jumped into his convertible "
          ,noun
          ," and drove off with his "
          ,adj
          ," wife."]
