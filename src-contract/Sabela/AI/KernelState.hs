{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.KernelState (
    KernelState (..),
    Activity (..),
    kernelStateOf,
    kernelStateJSON,
    isOccupied,
) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)

import Sabela.AI.KernelVocab (tagBuilding, tagCold, tagExecuting, tagIdle)

data KernelState
    = Cold
    | Alive
        { ksGen :: !Int
        , ksActivity :: !Activity
        , ksBuilding :: !Bool
        }
    deriving (Eq, Show)

data Activity
    = Idle
    | Executing
    deriving (Eq, Show)

kernelStateOf :: Bool -> Int -> Bool -> Bool -> KernelState
kernelStateOf alive gen busy building
    | not alive = Cold
    | otherwise =
        Alive
            { ksGen = gen
            , ksActivity = if busy then Executing else Idle
            , ksBuilding = building
            }

isOccupied :: KernelState -> Bool
isOccupied Cold = False
isOccupied (Alive _ activity building) = activity == Executing || building

kernelStateJSON :: KernelState -> Value
kernelStateJSON Cold = object ["state" .= tagCold]
kernelStateJSON (Alive gen activity building) =
    object
        [ "state" .= activityTag activity building
        , "ksGen" .= gen
        , "building" .= building
        ]

activityTag :: Activity -> Bool -> Text
activityTag Executing _ = tagExecuting
activityTag Idle True = tagBuilding
activityTag Idle False = tagIdle
