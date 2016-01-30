-- | Common code generation utility functions
module Language.PureScript.CodeGen.JS.Optimizer.TCO (tco) where

import Prelude

import Data.Array
import Data.Foldable
import Data.Maybe
import Data.Monoid.Disj

import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.Options

-- | Eliminate tail calls
tco :: Options -> JS -> JS
tco (Options opts) | opts.noTco = id
                   | otherwise = tco'

tco' :: JS -> JS
tco' = everywhereOnJS convert

tcoLabel :: String
tcoLabel = "tco"

tcoVar :: String -> String
tcoVar arg = "__tco_" <> arg

copyVar :: String -> String
copyVar arg = "__copy_" <> arg

convert :: JS -> JS
convert js@(JSVariableIntroduction name (Just fn@(JSFunction _ _ _))) =
    case collectAllFunctionArgs [] id fn of
        { argss: argss, body: body', replace: replace }
            | isTailCall name body' ->
                let allArgs = concat $ reverse argss
                in JSVariableIntroduction name (Just (replace (toLoop name allArgs body')))
            | otherwise -> js
convert js = js

collectAllFunctionArgs :: Array (Array String)
                       -> (JS -> JS)
                       -> JS
                       -> { argss :: Array (Array String), body :: JS, replace :: JS -> JS }
collectAllFunctionArgs allArgs f (JSFunction ident args jsBlockBody@(JSBlock block)) =
    case uncons block of
        Just { head: body@(JSReturn _), tail: _ } ->
            collectAllFunctionArgs (args `cons` allArgs) (\b -> f (JSFunction ident (map copyVar args) (JSBlock [b]))) body
        _ ->
            { argss: args `cons` allArgs, body: jsBlockBody, replace: f <<< JSFunction ident (map copyVar args) }
collectAllFunctionArgs allArgs f (JSReturn (JSFunction ident args (JSBlock [body]))) =
  collectAllFunctionArgs (args `cons` allArgs) (\b -> f (JSReturn (JSFunction ident (map copyVar args) (JSBlock [b])))) body
collectAllFunctionArgs allArgs f (JSReturn (JSFunction ident args body@(JSBlock _))) =
    { argss: args `cons` allArgs, body: body, replace: f <<< JSReturn <<< JSFunction ident (map copyVar args) }
collectAllFunctionArgs allArgs f body = { argss: allArgs, body: body, replace: f }

isTailCall :: String -> JS -> Boolean
isTailCall ident js =
  let
    numSelfCalls = everythingOnJS (+) countSelfCalls js
    numSelfCallsInTailPosition = everythingOnJS (+) countSelfCallsInTailPosition js
    numSelfCallsUnderFunctions = everythingOnJS (+) countSelfCallsUnderFunctions js
    numSelfCallWithFnArgs = everythingOnJS (+) countSelfCallsWithFnArgs js
  in
    numSelfCalls > 0
    && numSelfCalls == numSelfCallsInTailPosition
    && numSelfCallsUnderFunctions == 0
    && numSelfCallWithFnArgs == 0
  where
    countSelfCalls :: JS -> Int
    countSelfCalls (JSApp (JSVar ident') _) | ident == ident' = 1
    countSelfCalls _ = 0

    countSelfCallsInTailPosition :: JS -> Int
    countSelfCallsInTailPosition (JSReturn ret) | isSelfCall ident ret = 1
    countSelfCallsInTailPosition _ = 0

    countSelfCallsUnderFunctions :: JS -> Int
    countSelfCallsUnderFunctions (JSFunction _ _ js') = everythingOnJS (+) countSelfCalls js'
    countSelfCallsUnderFunctions _ = 0

    countSelfCallsWithFnArgs :: JS -> Int
    countSelfCallsWithFnArgs ret = if isSelfCallWithFnArgs ident ret [] then 1 else 0

toLoop :: String -> Array String -> JS -> JS
toLoop ident allArgs js = JSBlock $
    map (\arg -> JSVariableIntroduction arg (Just (JSVar (copyVar arg)))) allArgs ++
    [ JSLabel tcoLabel $ JSWhile (JSBooleanLiteral true) (JSBlock [ everywhereOnJS loopify js ]) ]
  where
    loopify :: JS -> JS
    loopify (JSReturn ret) | isSelfCall ident ret =
      let allArgumentValues = concat $ collectSelfCallArgs [] ret
      in JSBlock $ zipWith (\val arg -> JSVariableIntroduction (tcoVar arg) (Just val)) allArgumentValues allArgs
                <> map (\arg -> JSAssignment (JSVar arg) (JSVar (tcoVar arg))) allArgs
                <> [ JSContinue tcoLabel ]
    loopify other = other

    collectSelfCallArgs :: Array (Array JS) -> JS -> Array (Array JS)
    collectSelfCallArgs allArgumentValues (JSApp fn args') = collectSelfCallArgs (args' : allArgumentValues) fn
    collectSelfCallArgs allArgumentValues _ = allArgumentValues

isSelfCall :: String -> JS -> Boolean
isSelfCall ident (JSApp (JSVar ident') _) = ident == ident'
isSelfCall ident (JSApp fn _) = isSelfCall ident fn
isSelfCall _ _ = false

isSelfCallWithFnArgs :: String -> JS -> Array JS -> Boolean
isSelfCallWithFnArgs ident (JSVar ident') args | ident == ident' && any hasFunction args = true
isSelfCallWithFnArgs ident (JSApp fn args) acc = isSelfCallWithFnArgs ident fn (args <> acc)
isSelfCallWithFnArgs _ _ _ = false

hasFunction :: JS -> Boolean
hasFunction = runDisj <<< everythingOnJS append (Disj <<< isFunction)
  where
    isFunction :: JS -> Boolean
    isFunction (JSFunction _ _ _) = true
    isFunction _ = false
