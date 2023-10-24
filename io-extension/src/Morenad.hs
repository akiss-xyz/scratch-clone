--------------------------------------------------------------------------------
-- This file has a quest for you. Read the notes below!                       --
--------------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Morenad where

import Control.Monad.Trans.Class

-- region Note
-- | Morenad.
-- It's two Monads inside of one another, so... it's a Morenad.
-- Yes, I think I'm funny. This monad helps clean up handling of
-- IO (Either Err Int) and IO (Either Err Storage) in the code.
-- This kind of pattern kept showing up:
-- ```
--     do either <- lookup "thing" memory
--        case either of
--            Left err -> return $ Left err
--            Right va -> dosomething $ with va
-- ```
--
-- And since Either is a monad, what I would like to do is
--
-- ```
--     do either <- lookup "thing" memory
--        value <- either
--        dosomething $ with value
-- ```
--
-- But of course IO doesn't match with Either.
-- With Morenads:
--
-- ```
--     do value <- lift $ lookup "thing" memory
--        dosomething $ with value
-- ```
--
-- See the bottom of the file for your quest (should you choose to accept it).
-- Also, I know this is very much edging on MonadTrans and ExceptT. I just
-- couldn't get those to work as good as this. Also, this has the potential to
-- be more general - see your quest below!
-- endregion

-- | lessnad is the inverse of the Morenad constructor. Yes, I think I'm funny.
newtype Morenad o i v = Morenad { lessnad :: o (i v) }

-- | Applying a function to a Morenad applies it to the inner value.
instance (Monad o, Monad i) => Functor (Morenad o i) where
    fmap fn (Morenad om) = Morenad $ do im <- om
                                        return $ fn <$> im

-- | Applicatives for Morenads do what they say on the tin!
instance (Monad o, Monad i) => Applicative (Morenad o i) where
    -- | pure for Morenads constructs a Morenad by 'default-constructing' both
    -- the inner and outer monads.
    pure = Morenad . pure . pure
    -- | We take the inner monad out for both the function context and the value
    -- context, and then use the Applicative instance for the inner monad to
    -- apply the function.
    (Morenad mf) <*> (Morenad mv) = Morenad $ do v <- mv
                                                 f <- mf
                                                 return $ f <*> v

-- | Morenads are Monads over their inner values.
-- This part can be generalised, but I don't really have enough time (or
-- knowledge) to do so. You can find my thoughts at the bottom of the page.
instance Monad (Morenad IO (Either e)) where
    return = pure
    -- | We bind the Either value from the IO `mv`, and depending on what the
    -- Either value is, we apply the function or propagate the error.
    (Morenad mv) >>= fn = Morenad $ do v <- mv
                                       case v of
                                           Left err -> return $ Left err
                                           Right va -> lessnad $ fn va

instance Applicative o => MonadTrans (Morenad o) where
    -- | Lift takes an inner monad and uses it to construct a Morenad.
    -- For example, for our instance above, we'd apply lift to an Either Err v
    -- value to get a Morenad!
    -- We simply take the inner monad, use `pure` to construct the outer monad,
    -- and wrap that in the type constructor.
    lift = Morenad . pure

-- | wrap is essentially the same as MonadTrans lift, but instead of working
-- with the inner monad, it works with the outer monad.
-- We basically just bind the value of type v from the `o v` we get passed in,
-- and use that to first construct the inner monad with `pure`, and then use
-- that inner monad to construct the outer monad with `pure`.
wrap :: Monad o => Applicative i => o a -> Morenad o i a
wrap om = Morenad $ om >>= pure . pure

--------------------------------------------------------------------------------
--
-- | Your quest.
--
-- There is a better, more general way to express when a Morenad is a Monad.
-- Here, I've just given a specific instance for which a Morenad is a Monad, but
-- that's a copout - really, there's a set of conditions under which any Morenad
-- is a Monad.
--
-- I've gotten a good part through to expressing this in a typeclass, but my
-- ability to magick the Haskell type inference is failing me.
-- In general, Morenad is a Monad if i and o are related by a function `open`,
-- that given an `i v`, tries to put the `v` into an `o` context.  This may
-- succeed or it may fail, but no matter what, the returned value should be
-- inside of `o`.
--
-- Therefore, we need a typeclass Morenadic which says that you can:
--  * make a morenad (the `morenad` function),
--  * take the other monad out of the morenad (the `lessnad` function)
--  * and 'open' the inner monad into the outer monad (the `open` function)
--    - This one is the problem child.
-- 
-- If I were to succeed, it'd look a little like this (but with correct Haskell
-- so that the forall-bound type variables match...)
--
-- class Morenadic m where
--     morenad :: o (i v) -> m o i v
--     lessnad :: m o i v -> o (i v)
--     -- The issue is largely with this function.
--     open :: Monad i => i v -> Either (o (i v)) (o v)
--
--     -- Makes the Morenad from just the inner monad.
--     lift :: Applicative o => Monad i => i v -> m o i v
--     lift = morenad . pure
--     -- Makes the Morenad just from the outer monad.
--     wrap :: Monad o => Applicative i => o a -> m o i v
--     wrap om = morenad $ om >>= pure . pure

-- region Regular functor applicative stuff...
-- instance (Monad o, Monad i, Morenadic m) => Functor (m o i) where
--     fmap fn more = morenad $ do v <- lessnad v
--                                 return $ fn <$> v
-- 
-- instance (Monad o, Monad i, Morenadic m) => Applicative (m o i) where
--     pure = morenad . pure . pure
--     moref <*> morev = morenad $ do v <- lessnad v
--                                    f <- lessnad mf
--                                    return $ f <*> v
-- endregion
--
-- -- This instance is the real issue. We need to use the Morenadic functions to
-- -- express monad bind, but the types here are ambiguous for some reason.
-- instance (Monad o, Monad i, Morenadic m) => Monad (m o i) where
--     return = pure
--     morev >>= moref = morenad $ do v <- lessnad morev
--                                    -- This is what we need to fix.
--                                    -- I can't convince Haskell that (open v)
--                                    -- will give the correct types.
--                                    -- But since v :: i a, 
--                                    -- open v :: Either (o (i a)) (o a), no?
--                                    case (open v) of
--                                        Left err -> return $ Left err
--                                        Right va -> lessnad $ fn va
--
--------------------------------------------------------------------------------
