module MoteRunner (moteCli, moteTCli) where

import Prelude

import Control.MonadPlus (guard)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Mote.Monad (MoteT, Mote)
import Mote.Monad as MoteM
import Mote.Plan (Plan(..), PlanItem, foldPlan)
import Mote.Plan as Plan
import MoteRunner.Options (Config, parseConfig)
import Node.Process as Process

-- | Accepts a `MoteT` suite of tests, an interpreter and returns a command-line
-- | application that supports running individual tests, and inspection of the
-- | test plan.
moteCli
  ∷ ∀ void void' test bracket m
  . MonadEffect m
  ⇒ (Plan bracket test → m void)
  → Mote bracket test void'
  → m Unit
moteCli interpret suite = do
  config ← getConfig
  let plan = filterPlanM config.pattern (MoteM.plan suite)
  if config.list
    then liftEffect (for_ (listPlan plan) log)
    else void (interpret plan)

-- | Like moteCli, but supports effectful construction of the test plan in MonadEff.
moteTCli
  ∷ ∀ bracket test void void' m
  . MonadEffect m
  ⇒ (Plan.Plan bracket test → m void')
  → MoteT bracket test m void
  → m Unit
moteTCli interpret suite = do
  config ← getConfig
  plan ← filterPlanM config.pattern <$> MoteM.planT suite
  if config.list
    then liftEffect (for_ (listPlan plan) log)
    else void (interpret plan)

getConfig
  ∷ ∀ m
  . MonadEffect m
  ⇒ m Config
getConfig = do
  liftEffect parseConfig >>= case _ of
    Left err → liftEffect do
      log err
      Process.exit 1
    Right config →
      pure config

listPlan ∷ ∀ bracket test. Plan bracket test -> Array String
listPlan = foldPlan
  (\ { label } -> [ label ])
  (\label -> [ label ])
  (\ { label, value } -> map ((label <> "/") <> _) (listPlan value))
  join

filterPlanM
  ∷ ∀ bracket test
  . Maybe String
  → Plan bracket test
  → Plan bracket test
filterPlanM = maybe identity filterPlan

filterPlan
  ∷ ∀ bracket test
  . String
  → Plan bracket test
  → Plan bracket test
filterPlan pattern (Plan items) =
  Plan (Array.mapMaybe (shouldKeep pattern) items)

unPlan ∷ ∀ bracket test. Plan bracket test → Array (PlanItem bracket test)
unPlan (Plan p) = p

shouldKeep
  ∷ ∀ bracket test
  . String
  → PlanItem bracket test
  → Maybe (PlanItem bracket test)
shouldKeep pattern = case _ of
  t@(Plan.Test { label }) → guard (label == pattern) $> t
  Plan.Skip label → guard (label == pattern) $> Plan.Skip label
  g@(Plan.Group { label, value, bracket }) →
    if label == pattern
      then Just g
      else
        let
          filtered = Array.mapMaybe (shouldKeep pattern) (unPlan value)
        in
          guard (Array.null filtered) $>
            (Plan.Group { label, bracket, value: Plan filtered })
