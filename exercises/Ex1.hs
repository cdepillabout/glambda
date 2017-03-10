{-# LANGUAGE GADTs #-}

module Ex1 where

-- | A very simple newtype wrapper
newtype Wrap a = Wrap a deriving Show

-- | A type-indexed representation of a type
data STy ty where
  SIntTy   :: STy Int
  SBoolTy  :: STy Bool
  SMaybeTy :: STy a -> STy (Maybe a)
  SListTy :: STy a -> STy [a]
  SWrapTy :: STy a -> STy (Wrap a)
  SUnitTy :: STy ()
  SLambdaTy :: STy a -> STy b -> STy (a -> b)

-- | Produce a "zero" of that type
zero :: STy ty -> ty
zero SIntTy       = 0
zero SBoolTy      = False
zero (SMaybeTy _) = Nothing
zero (SListTy _) = []
zero (SWrapTy x) = Wrap $ zero x
zero SUnitTy = ()
zero (SLambdaTy _ y) = const $ zero y


