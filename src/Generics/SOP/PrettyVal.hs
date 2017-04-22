-- | Generic pretty-printer.
--
-- This module defines a generic function that helps in defining class
-- instances for the 'PrettyVal' class from the @pretty-show@ package.
--
module Generics.SOP.PrettyVal (
    gprettyVal
    -- * Re-exports
  , PrettyVal(..)
  ) where

import Text.Show.Pretty

import Generics.SOP

-- | Generic pretty-printer.
--
-- This function turns a value into the uniform representation of type
-- 'Value' that is provided by the @pretty-show@ package. The function
-- has the suitable type to serve as the default implementation of the
-- 'prettyVal' function in the 'PrettyVal' class.
--
-- If you have a datatype @T@ that is an instance of @generic-sop@'s
-- 'Generic' and 'HasDatatypeInfo' classes, you can use 'gprettyVal'
-- as follows:
--
-- > instance PrettyVal T where
-- >   prettyVal = gprettyVal
--
gprettyVal :: forall a. (Generic a, HasDatatypeInfo a, All2 PrettyVal (Code a)) => a -> Value
gprettyVal = gprettyVal' (datatypeInfo (Proxy :: Proxy a)) . from

gprettyVal' :: (All2 PrettyVal xss, All SListI xss) => DatatypeInfo xss -> SOP I xss -> Value
gprettyVal' (ADT     _ _ cs) = gprettyVal'' cs
gprettyVal' (Newtype _ _ c)  = gprettyVal'' (c :* Nil)

gprettyVal'' :: (All2 PrettyVal xss, All SListI xss) => NP ConstructorInfo xss -> SOP I xss -> Value
gprettyVal'' info (SOP sop) =
  hcollapse $ hcliftA2 allp prettyValFor info sop

prettyValFor :: All PrettyVal xs => ConstructorInfo xs -> NP I xs -> K Value xs
prettyValFor (Constructor n) = K . Con n . hcollapse . hcliftA p (K . prettyVal . unI)
prettyValFor (Infix n _ _)   = K . aux . hcliftA p (K . prettyVal . unI)
  where
    aux :: forall x y. NP (K Value) '[x, y] -> Value
    aux (K x :* K y :* Nil) = InfixCons x [(n, y)]
#if __GLASGOW_HASKELL__ < 800
    aux _                   = error "inaccessible"
#endif
prettyValFor (Record n fs)   = K . Rec n . hcollapse . hcliftA2 p aux fs
  where
    aux :: forall a. PrettyVal a => FieldInfo a -> I a -> K (Name, Value) a
    aux (FieldInfo f) (I a) = K (f, prettyVal a)

p :: Proxy PrettyVal
p = Proxy

allp :: Proxy (All PrettyVal)
allp = Proxy
