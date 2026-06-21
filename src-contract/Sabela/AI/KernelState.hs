{-# LANGUAGE OverloadedStrings #-}

{- | The typed @kernel_status@ state (design 1.1). The sole @kernel_status@
shape: a tagged 'KernelState' the agent matches on, replacing the retired
@kernel\/running\/compiling\/sessionGen@ loose-boolean blob.
-}
module Sabela.AI.KernelState (
    KernelState (..),
    Activity (..),
    kernelStateOf,
    kernelStateJSON,
) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)

{- | The kernel as a tag. @Cold@ is pre-spawn (never inferred from @gen == 0@);
every @Alive@ carries @ksGen@ (the restart counter @sbSessionGen@) and its
'Activity'. \"Wedged\" is deliberately absent: it is a two-observation harness
verdict, not a lock-free fact.
-}
data KernelState
    = Cold
    | Alive
        { ksGen :: !Int
        , ksActivity :: !Activity
        , ksBuilding :: !Bool
        }
    deriving (Show, Eq)

{- | What the run-lock axis is doing. @Building@ (the off-lock @appBuilding@
rebuild) is carried separately on 'Alive' as 'ksBuilding', not as a peer
constructor here, because it may co-occur with @Executing@.
-}
data Activity
    = Idle
    | Executing
    deriving (Show, Eq)

{- | Build the typed state from the same lock-free reads the legacy blob uses:
session presence, @sbBusy@, @sbSessionGen@, and @appBuilding@. Absent session
→ @Cold@; otherwise @Alive@ with @Executing@ iff busy and @ksBuilding@ iff the
build flag is up — the two are independent axes, so both can hold at once.
-}
kernelStateOf :: Bool -> Int -> Bool -> Bool -> KernelState
kernelStateOf alive gen busy building
    | not alive = Cold
    | otherwise =
        Alive
            { ksGen = gen
            , ksActivity = if busy then Executing else Idle
            , ksBuilding = building
            }

{- | The wire object for the typed state: one @state@ tag plus @ksGen@. The
@ebGeneration@ fence is emitted by the producer alongside this, distinct from
@ksGen@ — it is never folded in here under the word \"gen\".
-}
kernelStateJSON :: KernelState -> Value
kernelStateJSON Cold = object ["state" .= ("cold" :: Text)]
kernelStateJSON (Alive gen activity building) =
    object
        [ "state" .= activityTag activity building
        , "ksGen" .= gen
        , "building" .= building
        ]

{- | The single @state@ tag. @building@ stands alone only when idle; while a
cell runs the tag stays @executing@ and 'ksBuilding' carries the rebuild, so
neither axis swallows the other.
-}
activityTag :: Activity -> Bool -> Text
activityTag Executing _ = "executing"
activityTag Idle True = "building"
activityTag Idle False = "idle"
