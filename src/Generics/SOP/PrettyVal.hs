module Generics.SOP.PrettyVal (
    gprettyVal
    -- * Re-exports
  , PrettyVal(..)
  ) where

import Text.Show.Pretty

import Generics.SOP

gprettyVal :: forall a. (Generic a, HasDatatypeInfo a, All2 PrettyVal (Code a)) => a -> Value
gprettyVal = gprettyVal' (datatypeInfo (Proxy :: Proxy a)) . from

gprettyVal' :: (All2 PrettyVal xss, SingI xss) => DatatypeInfo xss -> SOP I xss -> Value
gprettyVal' (ADT     _ _ cs) = gprettyVal'' cs
gprettyVal' (Newtype _ _ c)  = gprettyVal'' (c :* Nil)

gprettyVal'' :: (All2 PrettyVal xss, SingI xss) => NP ConstructorInfo xss -> SOP I xss -> Value
gprettyVal'' info (SOP sop) =
  unI . hcollapse $ hcliftA2' p prettyValFor info sop

prettyValFor :: All PrettyVal xs => ConstructorInfo xs -> NP I xs -> K Value xs
prettyValFor (Constructor n) = K . Con n . hcollapse . hcliftA p (K . prettyVal . unI)
prettyValFor (Record n fs)   = K . Rec n . hcollapse . hcliftA2 p aux fs
  where
    aux :: forall a. PrettyVal a => FieldInfo a -> I a -> K (Name, Value) a
    aux (FieldInfo f) (I a) = K (f, prettyVal a)

p :: Proxy PrettyVal
p = Proxy
