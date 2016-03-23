module Database where

--import Data.Int (Int64)
--import Database.SQLite.Simple (
--        Connection, execute, Only (..),
--        execute_, lastInsertRowId, query,
--        SQLError(..), Error(..))

insertUser :: Connection -> FullName -> EmailAddress -> IO User
insertUser conn fn ea = do
  execute conn
    " INSERT INTO `users` \
    \ (full_name, email_address) \
    \ VALUES (?, ?)"
    (fn, ea)
  pk <- lastInsertRowId conn
  return (User pk fn ea)

getUsers :: Connection -> IO [User]
getUsers conn = 
  query_ conn
    " SELECT id, full_name, email_address \
    \ FROM `users`"

deleteUserById :: Connection -> UserId -> IO ()
deleteUserById conn userId =
  void $ execute conn
    " DELETE FROM `developers` \
    \ WHERE id = ?"
    (Only userId)

getUserById :: Connection -> UserId -> IO (Maybe User)
getUserById conn userId = do
  rs <- query conn
    " SELECT id, full_name, email_address \
    \ FROM `users` \
    \ WHERE id = ?"
    (Only userId)
  case rs of
    [] -> return Nothing
    [user] -> return (Just user)

migrateDB :: Connection -> IO ()
migrateDB conn = do
  execute_ conn
    " CREATE TABLE IF NOT EXISTS `users` \
    \ ( id INTEGER PRIMARY KEY \
    \ , full_name VARCHAR NOT NULL \
    \ , email_address VARCHAR NOT NULL) "
