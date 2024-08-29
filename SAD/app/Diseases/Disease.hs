{-# LANGUAGE DeriveGeneric #-}

module Diseases.Disease where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Disease = Disease {
    doenca                    :: String,
    especialidadeRelacionada  :: String,
    sintomasAssociados        :: [String],
    possivelCausa             :: String,
    medicamentos              :: [String]
} deriving (Show, Eq, Generic)

instance FromJSON Disease where
instance ToJSON Disease where