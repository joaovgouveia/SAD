{-# LANGUAGE DeriveGeneric #-}

module Diseases.Disease where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Disease = Disease {
    doenca                    :: String,
    especialidade_relacionada  :: String,
    sintomas_associados        :: [String],
    possivel_causa             :: String,
    medicamentos              :: [String]
} deriving (Show, Eq, Generic)

instance FromJSON Disease where
instance ToJSON Disease where