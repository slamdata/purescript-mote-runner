module MoteRunner where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.MonadPlus (guard)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for_)
import Mote.Monad (MoteT)
import Mote.Monad as MoteM
import Mote.Plan (Plan(..), PlanItem, foldPlan)
import Mote.Plan as Plan
import MoteRunner.Options (parseConfig)
import Node.Process (PROCESS)
import Node.Process as Process

type E e = ( process ∷ PROCESS, console ∷ CONSOLE | e )

-- | Accepts a `MoteT` suite of tests, an interpreter and returns a command-line
-- | application that supports running individual tests, and inspection of the
-- | test plan.
moteCli
  ∷ ∀ bracket test a e
  . (Plan.Plan bracket test → Aff (E e) Unit)
  → MoteT bracket test (Aff (E e)) a
  → Aff (E e) Unit
moteCli interpret suite = do
  liftEff parseConfig >>= case _ of
    Left err → liftEff do
      log err
      Process.exit 1
    Right config → do
      plan ← filterPlanM config.pattern <$> MoteM.planT suite
      if config.list
        then liftEff (for_ (listPlan plan) log)
        else interpret plan

listPlan ∷ ∀ bracket test. Plan bracket test -> Array String
listPlan = foldPlan
  (\ { label } -> [ label ])
  (\label -> [ label ])
  (\ { label, value } -> map ((label <> "/") <> _) (listPlan value))
  join

filterPlanM ∷ ∀ bracket test. Maybe String → Plan bracket test → Plan bracket test
filterPlanM = maybe id filterPlan

filterPlan ∷ ∀ bracket test. String → Plan bracket test → Plan bracket test
filterPlan pattern (Plan items) = Plan (Array.mapMaybe (shouldKeep pattern) items)

unPlan ∷ ∀ bracket test. Plan bracket test → Array (PlanItem bracket test)
unPlan (Plan p) = p

shouldKeep ∷ ∀ bracket test. String → PlanItem bracket test → Maybe (PlanItem bracket test)
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
          if Array.null filtered
            then Nothing
            else Just (Plan.Group { label, bracket, value: Plan filtered })
