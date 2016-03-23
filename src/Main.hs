module Main where

import Control.Applicative (liftA2)
import Data.Char (isAlpha)
import Data.Either
import Data.List (find)

data GameError = InvalidInput String
               | IncorrectGuess deriving (Show, Eq)

promptForGuess :: IO (Either GameError String)
promptForGuess = do
  putStrLn "Enter a guess:"
  guess <- getLine
  return (validateInput guess)

validateInput :: String -> Either GameError String
validateInput guess = if (foldr (\item acc -> acc && isAlpha item) True guess)
                      then Right guess
                      else Left (InvalidInput guess)

checkGuess :: String -> String -> IO (Either GameError String)
checkGuess fileName guess = do
  contents <- readFile fileName
  return $ maybe (Left IncorrectGuess) (return) $ find (==guess) (lines contents)

printResults :: Either GameError String -> IO ()
printResults (Left (InvalidInput guess)) = putStrLn $ "\"" ++ guess ++ "\" is not a valid guess."
printResults (Left (IncorrectGuess)) = putStrLn $ "Nope. That last guess was wrong."
printResults (Right guesses) = putStrLn ("Your two guesses (" ++ guesses ++ ") were correct.")

game :: IO (Either GameError String)
game = do
  putStrLn "I have lived in many cities. Can you guess one?"
  e1 <- promptForGuess
  case e1 of
    err@(Left _) -> return err
    Right g1 -> do
      r1 <- checkGuess "cities.txt" g1
      case e1 of
        err@(Left _) -> return err
        Right m1 -> do
          putStrLn "I like a lot of bands. Can you guess one?"
          e2 <- promptForGuess
          case e2 of
            err@(Left _) -> return err
            Right g2 -> do
              r2 <- checkGuess "bands.txt" g2
              case r2 of
                Right m2 -> return $ Right $ m1 ++ " and " ++ m2
                err -> return err

main :: IO ()
main = game >>= printResults



------------ EitherIO

data EitherIO e a = EitherIO { runEitherIO :: IO (Either e a) }

instance Functor (EitherIO e) where
  fmap f = EitherIO . fmap (fmap f) . runEitherIO

instance Applicative (EitherIO e) where
  pure    = EitherIO . pure . Right
  f <*> x = EitherIO $ liftA2 (<*>) (runEitherIO f) (runEitherIO x)

instance Monad (EitherIO e) where
  return  = pure
  x >>= f = EitherIO $ runEitherIO x >>= either (return . Left) (runEitherIO . f)

lift' :: IO a -> EitherIO e a
lift' x = EitherIO (fmap Right x)

bad :: EitherIO String Int
bad = EitherIO $ do
  putStrLn "bad!"
  return $ Left "bummer"

good :: EitherIO String Int
good = EitherIO $ do
  putStrLn "good"
  return $ Right 100

ex3 = do
  v1 <- good
  v2 <- bad -- short-circuit here
  v3 <- good
  return (v1, v2)



------------ get ya transform on

data EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f = EitherT . fmap (fmap f) . runEitherT

instance Applicative m => Applicative (EitherT e m) where
  pure    = EitherT . pure . Right
  f <*> x = EitherT $ liftA2 (<*>) (runEitherT f) (runEitherT x)

instance Monad m => Monad (EitherT e m) where
  return  = EitherT . return . Right
  x >>= f = EitherT $ runEitherT x >>= either (return . Left) (runEitherT . f)

lift :: Functor m => m a -> EitherT e m a
lift x = EitherT (fmap Right x)

game' :: IO (Either GameError String)
game' = runEitherT $ do
  lift $ putStrLn "I have lived in many cities. Can you guess one?"
  g1 <- EitherT promptForGuess
  m1 <- EitherT $ checkGuess "cities.txt" g1
  lift $ putStrLn "I like a lot of bands. Can you guess one?"
  g2 <- EitherT promptForGuess
  m2 <- EitherT $ checkGuess "bands.txt" g2
  return (m1 ++ " and " ++ m2)

main' :: IO ()
main' = game' >>= printResults
