{-# LANGUAGE TemplateHaskell #-}
module Language.ECMAScript3.Syntax.Quoter where

import Language.ECMAScript3.Syntax
import Language.ECMAScript3.Parser
import Language.ECMAScript3.Syntax.Annotations
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.ECMAScript3.Syntax.Quoter.JSExpr

js :: QuasiQuoter
js = QuasiQuoter {quoteExp = \s -> case parseScriptFromString "" s of
                     Left err -> do                   
                       (line,_) <- TH.loc_start <$> TH.location
                       let pos = errorPos err
                       let newPos = setSourceLine pos $ line + sourceLine pos - 1
                       fail (show $ setErrorPos newPos err)
                     Right js -> [| js |]}