-- | Common code generation utility functions
module Language.PureScript.CodeGen.JS.Optimizer.Blocks where

import Data.Array (concatMap)
import Data.Maybe (Maybe(Nothing))

import Language.PureScript.CodeGen.JS.AST
    ( BinaryOperator(And), JS(JSBinary, JSIfElse, JSBlock), everywhereOnJS )

-- | Collapse blocks which appear nested directly below another block
collapseNestedBlocks :: JS -> JS
collapseNestedBlocks = everywhereOnJS collapse
  where
    collapse :: JS -> JS
    collapse (JSBlock sts) = JSBlock (concatMap go sts)
    collapse js = js

    go :: JS -> Array JS
    go (JSBlock sts) = sts
    go s = [s]

collapseNestedIfs :: JS -> JS
collapseNestedIfs = everywhereOnJS collapse
  where
    collapse :: JS -> JS
    collapse (JSIfElse cond1 (JSBlock [JSIfElse cond2 body Nothing]) Nothing) =
        JSIfElse (JSBinary And cond1 cond2) body Nothing
    collapse js = js
