-- | Common code generation utility functions
module Language.PureScript.CodeGen.JS.Optimizer.Unused where

import Prelude (bind, (==), (<>), not, (<<<), otherwise)

import Data.Array (uncons, span)
import Data.Foldable (any)
import Data.Maybe (Maybe(Nothing, Just))

import Language.PureScript.CodeGen.JS.AST
    ( JS(JSApp, JSVar, JSFunction, JSReturn), everywhereOnJS )
import Language.PureScript.CodeGen.JS.Optimizer.Common (removeFromBlock)
import Language.PureScript.Constants as C
import Language.PureScript.Crash (internalError)

removeCodeAfterReturnStatements :: JS -> JS
removeCodeAfterReturnStatements = everywhereOnJS (removeFromBlock go)
  where
    go :: Array JS -> Array JS
    go jss | not (any isJSReturn jss) = jss
           | otherwise =
        let spans = span (not <<< isJSReturn) jss
        in case spans of
            { init: body, rest: rest } ->
                case uncons rest of
                    Just { head: ret, tail: _ } -> body <> [ret]
                    Nothing -> internalError "rest is empty in removeCodeAfterReturnStatements"

    isJSReturn :: JS -> Boolean
    isJSReturn (JSReturn _) = true
    isJSReturn _ = false

removeUnusedArg :: JS -> JS
removeUnusedArg = everywhereOnJS convert
  where
    convert (JSFunction name [arg] body) | arg == C.__unused = JSFunction name [] body
    convert js = js

removeUndefinedApp :: JS -> JS
removeUndefinedApp = everywhereOnJS convert
  where
    convert (JSApp fn [JSVar arg]) | arg == C.undefined = JSApp fn []
    convert js = js
