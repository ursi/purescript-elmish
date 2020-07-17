module Sub where

import Prelude
import Data.Function.Uncurried (Fn2, Fn3, Fn4, Fn5)
import Effect (Effect)
import Effect.Uncurried (EffectFn1)

foreign import data Sub :: Type -> Type

type Callback a
  = EffectFn1 a Unit

type Canceler
  = Effect Unit

type Arg a
  = { arg :: a
    , eq :: a -> a -> Boolean
    }

type SubImpl0 a
  = EffectFn1 (Callback a) Canceler

type SubImpl1 b a
  = b -> (EffectFn1 (Callback a) Canceler)

type SubImpl2 b c a
  = Fn2 b c (EffectFn1 (Callback a) Canceler)

type SubImpl3 b c d a
  = Fn3 b c d (EffectFn1 (Callback a) Canceler)

type SubImpl4 b c d e a
  = Fn4 b c d e (EffectFn1 (Callback a) Canceler)

type SubImpl5 b c d e f a
  = Fn5 b c d e f (EffectFn1 (Callback a) Canceler)

foreign import none :: ∀ a. Sub a

foreign import toSub0 :: ∀ a. String -> SubImpl0 a -> Sub a

foreign import toSub1 :: ∀ a b. String -> Arg b -> SubImpl1 b a -> Sub a

foreign import toSub2 :: ∀ a b c. String -> Arg b -> Arg c -> SubImpl2 b c a -> Sub a

foreign import toSub3 ::
  ∀ a b c d.
  String ->
  Arg b ->
  Arg c ->
  Arg d ->
  SubImpl3 b c d a ->
  Sub a

foreign import toSub4 ::
  ∀ a b c d e.
  String ->
  Arg b ->
  Arg c ->
  Arg d ->
  Arg e ->
  SubImpl4 b c d e a ->
  Sub a

foreign import toSub5 ::
  ∀ a b c d e f.
  String ->
  Arg b ->
  Arg c ->
  Arg d ->
  Arg e ->
  Arg f ->
  SubImpl5 b c d e f a ->
  Sub a
