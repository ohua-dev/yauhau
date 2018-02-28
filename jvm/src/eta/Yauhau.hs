{-# LANGUAGE MagicHash, ForeignFunctionInterface #-}
module Yauhau where

import qualified Ohua.Compat.JVM.Compiler as C
import Java
import Data.Default.Class
import Ohua.Transform.Yauhau
import Ohua.Compat.JVM.Marshal
import Ohua.Compile
import Control.Monad
import Ohua.Unit


data {-# CLASS "yauhau.Compiler" #-} NCompiler = NCompiler (Object# NCompiler) deriving Class

runYCompM :: C.CompM a -> Object -> C.IsLinker -> Object -> IO a
runYCompM comp = C.runCompM comp def { passAfterDFLowering = cleanUnits
                                     , passAfterNormalize = liftSmap >=> combineIO
                                     }

nativeCompileWSplice = runYCompM C.gNativeCompileWSplice

foreign export java "@static yauhau.Compiler.compileAndSpliceEnv" nativeCompileWSplice :: Object -> C.IsLinker -> Object -> IO (NGraph (NLazy Object))

