{-# LANGUAGE TemplateHaskell, LambdaCase, StandaloneDeriving, DeriveGeneric #-}
module Yauhau.IR.Utils where

import ClassyPrelude
import Data.Aeson
import Data.Aeson.Types (defaultOptions, fieldLabelModifier, camelTo2)
import Data.Aeson.TH
import Control.Lens
import Data.Sequence (Seq)


type Binding = Text
type CtxType = Text


data Arg = Arg
    { _argIndex :: Maybe Int
    , _binding :: Binding
    } deriving (Eq, Show, Generic)


data IRFunc = IRFunc
    { _identifier :: Int
    , _name :: Text
    , _argumentBindings :: [Arg]
    , _returnBindings :: Either Arg [Arg]
    } deriving (Eq, Show, Generic)


data CtxFrame = CtxFrame
    { _ctxType :: CtxType
    , _opId :: Int
    , _outVar :: Int
    } deriving (Eq, Show)


instance Hashable Arg
instance Hashable IRFunc

(let
    dropPrefix p a = fromMaybe a (stripPrefix p a)
    rewriteSome "identity" = "id"
    rewriteSome "ctxType" = "type"
    rewriteSome "returnBindings" = "return"
    rewriteSome "argumentBindings" = "args"
    rewriteSome "argIndex" = "index"
    rewriteSome a = a
    opts = defaultOptions {fieldLabelModifier = camelTo2 '_' . rewriteSome . dropPrefix "_" }
 in
    concat <$> sequence
        [ deriveJSON opts ''Arg
        , deriveJSON opts ''IRFunc
        , deriveJSON opts ''CtxFrame
        ])

makeLenses ''Arg
makeLenses ''IRFunc
makeLenses ''CtxFrame


type IRGraph = Seq IRFunc
type CtxStack = Seq CtxFrame
