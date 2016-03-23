{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Data.Int (Int64)
import Database.SQLite.Simple (FromRow, ToRow)

newtype UserId = UserId Int64 deriving (Eq, Show, ToField, FromField)
newtype FullName = FullName Text deriving (Eq, Show, ToField, FromField)
newtype EmailAddress = EmailAddress Text deriving (Eq, Show, ToField, FromField)

data User = User { userId :: UserId
                 , userFullName :: FullName 
                 , userEmailAddress :: EmailAddress } deriving (Eq, Show, FromRow)