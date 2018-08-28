-- |
-- Module:      State
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- REPL state.
--
module State
    ( State
    , emptyState
    , Value(..)
    , setUnits
    , lengthUnit
    , speedUnit
    , showLength
    , showSpeed
    , insert
    , delete
    , lookup
    ) where

import Control.Applicative
import Data.Char (isLetter)
import Data.Geo.Jord
import Data.List hiding (delete, insert, lookup)
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

-- | REPL state.
data State =
    State Units
          Vault

-- | A value accepted and returned by 'eval'.
data Value
    = Ang Angle -- ^ angle
    | Bool Bool -- ^ boolean
    | Cpa (Cpa (AngularPosition NVector)) -- ^ CPA
    | Dlt Delta -- ^ delta
    | Dur Duration -- ^ duration
    | Double Double -- ^ double
    | Ep EcefPosition -- ^ ECEF position
    | Em Earth -- ^ earth model
    | FrmB Angle
           Angle
           Angle -- ^ yaw, pitch and roll of Body frame
    | FrmL Angle -- ^ wander azimuth of Local frame
    | FrmN -- ^ North, east, down frame
    | Gc GreatCircle -- ^ great circle
    | Gp (AngularPosition LatLong) -- ^ latitude, longitude and height
    | Intp (Intercept (AngularPosition NVector)) -- ^ Intercept
    | Len Length -- ^ length
    | Ned Ned -- ^ north east down
    | Np (AngularPosition NVector) -- ^ n-vector and height
    | Spd Speed -- ^ speed
    | Trk (Track (AngularPosition NVector)) -- ^ track
    | Vals [Value] -- array of values

-- | A location for 'Value's to be shared by successive evalations.
newtype Vault =
    Vault [(String, Value)]

-- | functions to show values with a pre-defined unit.
data Units =
    Units (Length -> String)
          (Speed -> String)

-- | empty state: length in kilometres, speed in kilometres/hour and empty vault.
emptyState :: State
emptyState = State (Units len spd) (Vault [])
  where
    len l = show (toKilometres l) ++ "km"
    spd s = show (toKilometresPerHour s) ++ "km/h"

-- | sets length and or speed units, ignore all invalid units.
setUnits :: [String] -> State -> State
setUnits us (State (Units l s) v) = State (Units (fromMaybe l lu) (fromMaybe s su)) v
  where
    lu = foldl (<|>) Nothing (fmap toLenUnit us)
    su = foldl (<|>) Nothing (fmap toSpdUnit us)

toLenUnit :: String -> Maybe (Length -> String)
toLenUnit "m" = Just (\l -> show (toMetres l) ++ "m")
toLenUnit "km" = Just (\l -> show (toKilometres l) ++ "km")
toLenUnit "nm" = Just (\l -> show (toNauticalMiles l) ++ "nm")
toLenUnit "ft" = Just (\l -> show (toFeet l) ++ "ft")
toLenUnit _ = Nothing

toSpdUnit :: String -> Maybe (Speed -> String)
toSpdUnit "m/s" = Just (\l -> show (toMetresPerSecond l) ++ "m/s")
toSpdUnit "km/h" = Just (\l -> show (toKilometresPerHour l) ++ "km/h")
toSpdUnit "mph" = Just (\l -> show (toMilesPerHour l) ++ "mph")
toSpdUnit "kt" = Just (\l -> show (toKnots l) ++ "kt")
toSpdUnit "ft/s" = Just (\l -> show (toFeetPerSecond l) ++ "ft/s")
toSpdUnit _ = Nothing

-- | length unit.
lengthUnit :: State -> String
lengthUnit s = filter isLetter (showLength zero s)

-- | speed unit.
speedUnit :: State -> String
speedUnit s = filter (\c -> isLetter c || c == '/') (showSpeed zero s)

-- | show length in selected unit.
showLength :: Length -> State -> String
showLength l (State (Units len _) _) = len l

-- | show speed in selected unit.
showSpeed :: Speed -> State -> String
showSpeed s (State (Units _ spd) _) = spd s

-- | @insert k v state@ inserts value @v@ for key @k@. Overwrites any previous value.
insert :: String -> Value -> State -> State
insert k v (State u vault) = State u (Vault (e ++ [(k, v)]))
  where
    Vault e = delete' k vault

-- | @lookup k state@ looks up the value of key @k@ in the vault.
lookup :: String -> State -> Maybe Value
lookup k (State _ (Vault es)) = fmap snd (find (\e -> fst e == k) es)

-- | @delete k state@ deletes key @k@ from the vault.
delete :: String -> State -> State
delete k (State u v) = State u (delete' k v)

-- | @delete k vault@ deletes key @k@ from the vault.
delete' :: String -> Vault -> Vault
delete' k (Vault es) = Vault (filter (\e -> fst e /= k) es)
