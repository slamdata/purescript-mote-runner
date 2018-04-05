module MoteRunner where

import Prelude

import Control.Monad.Aff (Aff, error, throwError)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.MonadPlus (guard)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Mote.Monad (MoteT)
import Mote.Monad as MoteM
import Mote.Plan (Plan(..), PlanItem, foldPlan)
import Mote.Plan as Plan
import MoteRunner.Options (parseConfig)
import Node.Process (PROCESS)

type E e = (process ∷ PROCESS, console ∷ CONSOLE | e)

moteCli ∷ ∀ bracket test a e. MoteT bracket test (Aff (E e)) a → Aff (E e) Unit
moteCli suite = do
  liftEff parseConfig >>= case _ of
    Left err → throwError (error err)
    Right config
      | config.list → do
          tasks ← listTasks config.pattern suite
          liftEff (for_ tasks log)
      | otherwise → do
          pure unit

listTasks
  ∷ ∀ bracket test a e
  . Maybe String
  → MoteT bracket test (Aff e) a
  → Aff e (Array String)
listTasks pattern suite = do
  plan ← MoteM.planT suite
  pure $ listPlan case pattern of
    Just p → filterPlan p plan
    Nothing → plan

listPlan ∷ ∀ bracket test. Plan bracket test -> Array String
listPlan = foldPlan
  (\ { label } -> [ label ])
  (\label -> [ label ])
  (\ { label, value } -> map ((label <> "/") <> _) (listPlan value))
  join

filterPlan ∷ ∀ bracket test. String -> Plan bracket test -> Plan bracket test
filterPlan pattern (Plan items) = Plan (Array.mapMaybe (shouldKeep pattern) items)

unPlan ∷ forall t3 t4. Plan t4 t3 -> Array (PlanItem t4 t3)
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
