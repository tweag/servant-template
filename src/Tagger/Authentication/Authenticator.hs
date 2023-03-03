{-# LANGUAGE RankNTypes #-}

module Tagger.Authentication.Authenticator where

import Tagger.Authentication.Credentials (Credentials (..))
import Tagger.Id (Id)
import Tagger.User (User)

-- |
-- 'AuthenticateUser' is a service which exposes the ability to authenticate a 'User' providing her 'Credentials'.
-- It is indexed by a context 'm' which wraps the results.
newtype Authenticator m = Authenticator {authUser :: Credentials -> m (Id User)}

-- |
-- Given a natural transformation between a context 'm' and a context 'n', it allows to change the context where 'AuthenticateUser' is operating
hoist :: (forall a. m a -> n a) -> Authenticator m -> Authenticator n
hoist f (Authenticator auth) = Authenticator $ f . auth
