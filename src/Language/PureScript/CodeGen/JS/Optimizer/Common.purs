-- | Common code generation utility functions
module Language.PureScript.CodeGen.JS.Optimizer.Common where

import Prelude (bind, (||), (==), (<>), ($), (<<<))

import Data.Array (uncons)
import Data.Foldable (any, elem)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (NonEmpty(NonEmpty), foldl1)
import Data.Tuple (Tuple, lookup)

import Language.PureScript.Crash (internalError)
import Language.PureScript.CodeGen.JS.AST
    ( JS( JSBlock, JSAssignment, JSIndexer, JSAccessor, JSVar
        , JSForIn, JSFor, JSVariableIntroduction, JSFunction)
    , everythingOnJS, everywhereOnJS
    )

applyAll :: forall a . Array (a -> a) -> a -> a
applyAll functions = foldl1 (<<<) (toNonEmpty functions)
  where
    toNonEmpty :: Array (a -> a) -> NonEmpty Array (a -> a)
    toNonEmpty fs =
        case uncons fs of
            Just { head: head, tail: tail } -> NonEmpty head tail
            Nothing -> internalError "array of functions passed to applyAll is empty"

replaceIdent :: String -> JS -> JS -> JS
replaceIdent var1 js = everywhereOnJS replace
  where
    replace :: JS -> JS
    replace (JSVar var2) | var1 == var2 = js
    replace other = other

replaceIdents :: Array (Tuple String JS) -> JS -> JS
replaceIdents vars = everywhereOnJS replace
  where
    replace :: JS -> JS
    replace v@(JSVar var) = fromMaybe v $ lookup var vars
    replace other = other

isReassigned :: String -> JS -> Boolean
isReassigned var1 = everythingOnJS (||) check
  where
    check :: JS -> Boolean
    check (JSFunction _ args _) | var1 `elem` args = true
    check (JSVariableIntroduction arg _) | var1 == arg = true
    check (JSAssignment (JSVar arg) _) | var1 == arg = true
    check (JSFor arg _ _ _) | var1 == arg = true
    check (JSForIn arg _ _) | var1 == arg = true
    check _ = false

isRebound :: JS -> JS -> Boolean
isRebound js d = any (\v -> isReassigned v d || isUpdated v d) (everythingOnJS (<>) variablesOf js)
  where
    variablesOf :: JS -> Array String
    variablesOf (JSVar var) = [var]
    variablesOf _ = []

isUsed :: String -> JS -> Boolean
isUsed var1 = everythingOnJS (||) check
  where
    check :: JS -> Boolean
    check (JSVar var2) | var1 == var2 = true
    check (JSAssignment target _) | var1 == targetVariable target = true
    check _ = false

targetVariable :: JS -> String
targetVariable (JSVar var) = var
targetVariable (JSAccessor _ tgt) = targetVariable tgt
targetVariable (JSIndexer _ tgt) = targetVariable tgt
targetVariable _ = internalError "Invalid argument to targetVariable"

isUpdated :: String -> JS -> Boolean
isUpdated var1 = everythingOnJS (||) check
  where
    check :: JS -> Boolean
    check (JSAssignment target _) | var1 == targetVariable target = true
    check _ = false

removeFromBlock :: (Array JS -> Array JS) -> JS -> JS
removeFromBlock go (JSBlock sts) = JSBlock (go sts)
removeFromBlock _  js = js
