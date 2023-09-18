module DB.Schema.User
  ( Row (..),
    relation,
    litUser,
    serializeUser,
    unserializeUser,
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Rel8 (Column, Expr, Name, Rel8able, Result, TableSchema (..), lit)
import Tagger.EncryptedPassword (EncryptedPassword)
import Tagger.Id (Id)
import Tagger.User (User (..))

-- |
-- The database representation of a 'User'
data Row f = Row
  { userId :: Column f (Id User),
    userName :: Column f Text,
    userPassword :: Column f EncryptedPassword
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

-- |
-- A description of the schema of the 'User' table
relation :: TableSchema (Row Name)
relation =
  TableSchema
    { name = "users",
      schema = Nothing,
      columns =
        Row
          { userId = "id",
            userName = "name",
            userPassword = "password"
          }
    }

-- |
-- Allows to lift a 'User' with no context into the 'Expr' context
litUser :: Row Result -> Row Expr
litUser (Row id' name' password) = Row (lit id') (lit name') (lit password)

-- |
-- Transform from a domain representation of a 'User' to its underlying database representation
serializeUser :: Id User -> User -> Row Result
serializeUser uuid user =
  Row
    { userId = uuid,
      userName = user.name,
      userPassword = user.password
    }

-- |
-- Transform from the database representation of a 'User' to its domain representation
unserializeUser :: Row Result -> User
unserializeUser user = User (userName user) (userPassword user)
