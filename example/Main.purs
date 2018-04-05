module Main where

import Prelude

import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Trans.Class (lift)
import Data.String as String
import Data.Traversable (for_, sequence_)
import Mote (MoteT, Plan, foldPlan, group, test)
import MoteRunner (moteCli)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Test.QuickCheck (Result, (===))
import Test.Unit (Test, TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTestWith)
import Test.Unit.Output.Fancy as Fancy

theCommutativeProperty ∷ Int → Int → Result
theCommutativeProperty a b = (a + b) === (b + a)

suite
  ∷ ∀ bracket eff eff1
  . MoteT bracket (Test eff) (Aff (fs ∷ FS.FS | eff1)) Unit
suite = do
  test "basic asserts" do
    Assert.assert "wasn't true" true
    Assert.assertFalse "wasn't false" false
  group "generated from file" do
    lines ← lift $ FS.readTextFile UTF8 "./example/testfile"
    for_ (String.split (String.Pattern "\n") lines) \line ->
      test line do
        TU.success
  group "another group" do
    group "nested group" do
      test "nested test" do
        TU.success
    test "another test" do
      TU.failure "This one fails"

-- | interpret runs a `Plan` to produce a `TestSuite`
interpret
  ∷ ∀ bracket eff
  . Plan bracket (Test eff)
  → TestSuite eff
interpret =
  foldPlan
    (\ { label, value } -> TU.test label value)
    (\label -> TU.testSkip label (pure unit))
    (\ { label, value } -> TU.suite label (interpret value))
    sequence_

main ∷ ∀ e. Eff _ Unit
main = launchAff_ (moteCli (runTestWith Fancy.runTest <<< interpret) suite)
