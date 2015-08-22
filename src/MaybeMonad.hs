module MaybeMonad (mainMaybeMonad) where

-- stuff required by the stuff required to ensure GHC 7.8 compatibility (see below)
-- according to: https://wiki.haskell.org/Functor-Applicative-Monad_Proposal
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)


-- we reinvent the Maybe monad here
-- (the code should work just as well with the default Maybe)
data MyMaybe a = MyJust a | MyNothing
    deriving Show


instance Monad MyMaybe where
    MyNothing >>= _ = MyNothing
    MyJust x >>= f = f x

    return x = MyJust x


-- stuff required to ensure GHC 7.8 compatibility
-- according to: https://wiki.haskell.org/Functor-Applicative-Monad_Proposal
instance Functor MyMaybe where
    fmap = liftM
instance Applicative MyMaybe where
    pure  = return
    (<*>) = ap


-- no monad version

deposit  value account = account + value
withdraw value account = account - value


eligible :: (Num a, Ord a) => a -> Bool
eligible account =
    let account1 = deposit 100 account in
    if account < 0
    then False
    else
        let account2 = withdraw 200 account1 in
        if account2 < 0
        then False
        else
            let account3 = deposit 100 account2 in
            if account3 < 0
            then False
            else
                let account4 = withdraw 300 account3 in
                if account4 < 0
                then False
                else
                    let account5 = deposit 1000 account4 in
                        if account5 < 0
                        then False
                        else True


-- monadic version

withdraw2 :: (Num a, Ord a) => a -> a -> MyMaybe a
withdraw2 value account =
    if account < value
    then MyNothing
    else MyJust (account - value)


deposit2 value account = MyJust (account + value)


eligible2 :: (Num a, Ord a) => a -> MyMaybe Bool
eligible2 account = do
    account1 <- deposit2 100 account
    account2 <- withdraw2 200 account1
    account3 <- deposit2 100 account2
    account4 <- withdraw2 300 account3
    account5 <- deposit2 1000 account4
    MyJust True


eligible2b account =
    deposit2 100 account >>=
    withdraw2 200 >>=
    deposit2 100 >>=
    withdraw2 300 >>=
    deposit2 1000 >>
    return True


-- this shoukd work with any eligible* functions
mainMaybeMonad = do
    print $ eligible2b 300 -- True
    print $ eligible2b 299 -- False
