# Monad Transformers

Note: This is a paraphrased rip-off of the excellent article [A Gentle Introduction to Monad Transformers](https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md). Almost none of these ideas are my own. I simply paired down the original post to something I thought was manageable for our study group.

## What are we gonna cover?

* What they are
* Why you'd want them
* How they work

## What's a Monad Transformer?

A monad transformer is a specialized version of a monad that allows it to be combined with other monads.

## Why would you want to combine monads?

* To add configuration (`Reader`) capability to an IO-heavy program instead of relying on ENV
* To add "short-circuiting" capabilities (`Either`) to my HTTP request-handling code
* To add logging capability (`Writer`) to my test mocks (i.e. "assert getFooById was called with arg1 and arg2")

## Why can't regular monads be combined with other monads?

Let's take a look at the `Monad` type class:

```haskell
class Applicative m => Monad m where
  (>>=) :: forall a b. m a -> (a -> m b) -> m b
  (>>)  :: forall a b. m a -> m b -> m b
```

Note: When binding (`>>=`) the `m` in the second parameter provided `(a -> m b)` cannot change. Since do-notation is just syntactic sugar around bind, this means that you're stuck using the same `m` throughout a do-block.

For instance, `m` is chosen in the following block to be `IO`, so we can't use the `Maybe` instance of `Monad`:
```haskell
main = do
  contents <- readFile "foo.txt"
  case listToMaybe contents of
    Just _ -> putStrLn "File was not empty."
    Nothing -> ()
```

This would result in a type error:
```haskell
main = do
  contents <- readFile "foo.txt"
  listToMaybe contents -- can't intermix
  putStrLn "File was not empty."
```

## What's the impact of this restriction?

Restricting ourselves to one monad per function can be a bummer. 

```haskell
data GameError = InvalidInput String
               | IncorrectGuess deriving (Show, Eq)

promptForGuess :: IO (Either GameError String)
promptForGuess = do
  putStrLn "Enter a guess:"
  guess <- getLine
  return (validateInput guess)
```

```haskell
validateInput :: String -> Either GameError String
validateInput guess = if (foldr (\item acc -> acc && isAlpha item) True guess)
                      then Right guess
                      else Left (InvalidInput guess)
```

```haskell
checkGuess :: String -> String -> IO (Either GameError String)
checkGuess fileName guess = do
  contents <- readFile fileName
  return $ maybe (Left IncorrectGuess) (return) $ find (==guess) (lines contents)
```

```haskell
printResults :: Either GameError String -> IO ()
printResults (Left (InvalidInput guess)) = putStrLn $ "\"" ++ guess ++ "\" is not valid input."
printResults (Left (IncorrectGuess))     = putStrLn $ "Nope. That last guess was wrong."
printResults (Right guesses)             = putStrLn ("Your two guesses (" ++ guesses ++ ") were correct.")
```

```haskell
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
```

```haskell
main :: IO ()
main = game >>= printResults
```

Note that in the `game` function we are explicitly pattern-matching on the constructor of `Either GameError String` in order to short-circuit our computation. The `Either` `Monad` instance does this for us:

```haskell
instance Monad (Either e) where
  return = Right
  Right m >>= k = k m
  Left e  >>= _ = Left e
```

...but we can't intermix our choice of `m` in the function `k` passed to bind (`>>=`).

## Transformers to the rescue

Monad transformers allow us to add the functionality of one monad into a different monad, producing a new super-awesome monad with the powers of both. We'll begin by adding `Either` functionality to the `IO` monad.

## Rolling our own transformer

Let's create a new type `EitherIO` that represents `IO` combined with `Either`:

```haskell
data EitherIO e a = EitherIO { runEitherIO :: IO (Either e a) }
```

We can create values of type `Either IO e a` from a value of `IO (Either e a)`:

```
*Main> let f = undefined :: IO (Either e a)
*Main> let g = EitherIO f
*Main> :t g
g :: EitherIO e a
```

..and go the other way, too:

```
*Main> :t runEitherIO
runEitherIO :: EitherIO e a -> IO (Either e a)
*Main> :t runEitherIO g
runEitherIO g :: IO (Either e a)
```

I'll refer to converting from `IO (Either e a)` as "wrapping," and converting from `EitherIO e a` as "unwrapping."

### Type classes out the wazzoo

Our new type `EitherIO e a` needs to instantiate `Monad`, and thus needs to instantiate `Applicative` and `Functor` type classes. I'm just gonna copypaste from elsewhere and try to explain.

#### Functor

Given a value of type `EitherIO e a` and function `f` of type `a -> b`, we produce a new value `EitherIO e a` that when run:

1. runs the underlying `IO` computation, producing either `Left e` or `Right a`
2. if `Left e`, leave it alone (results in `IO (Left e)`)
3. if `Right a`, apply `f` to `a` (results in `IO (Right (f a))`)

```haskell
instance Functor (EitherIO e) where
  fmap f = EitherIO . fmap (fmap f) . runEitherIO
```

#### Applicative

I can't explain this one.

```haskell
instance Applicative (EitherIO e) where
  pure    = EitherIO . return . Right
  f <*> x = EitherIO $ liftA2 (<*>) (runEitherIO f) (runEitherIO x)
```

#### Monad

When `>>=` is applied to `x` and `f` a new `EitherIO e a` is produced that when run:

1. Runs the underlying IO action
2. If that action produced a `Left e`, return `IO (Left e)` (resulting in `IO (Left e)`)
3. If that action produced a `Right a`, apply `f` to `a` (resulting in `IO (Right (f a))`)

```haskell
-- f :: a -> Either IO e a
-- runEitherIO :: EitherIO e a -> IO (Either e a)

instance Monad (EitherIO e) where
  return  = pure             
  x >>= f = EitherIO $ runEitherIO x >>= either (return . Left) (runEitherIO . f)
```

### Why?!?

What this enables is for us to build sequences of short-circuiting (like `Either`) computations that interact with the outside world (like `IO`).

```haskell
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
```

```
*Main> runEitherIO ex3
good
bad!
Left "bummer"
```

### Lifting

So, that works great if all of our functions are of the type `IO (Either e a)`. But what happens if we have some functions (like those in the Prelude) that are of the type `IO a`? How can we use them in our new `EitherIO` monad?

To accomplish this, we'll implement `lift`, which allows us "get at" functions in an inner monad from our outer, more powerful monad.

```haskell
lift :: IO a -> EitherIO e a
lift x = EitherIO (fmap Right x)
```

This function is given a value of type `IO a` and produces an `EitherIO e a` that, when run:

1. Runs the original `IO a` computation (producing a)
2. Wraps the results in a `Right` and returns (produces `IO (Right a)`)
3. Wraps that result in `EitherIO`

Let's use `lift` alongside our other functions

```haskell
ex4 = do
  v1 <- good
  v2 <- good
  lift $ putStrLn "Fun in the sun."
  return (v1, v2)
```

### Generalizing

I implemented `EitherIO` for three reasons:

1) [This guy](https://github.com/kqr) did it too
2) Our program used `IO (Either e a)` already
3) Programming in the concrete helps me understand things before generalizing

As it turns out, though, we've implemented something very close to `EitherT`, which allows you to wrap _any_ monad with `Either`-like short-circuiting functionality. *The important thing to note is that `EitherT` does not know anything about the monad that it wraps. In fact, that monad _could itself be a wrapped monad!_*

#### Data Constructor

```haskell
data EitherT e m a = EitherT { runEitherT :: m (Either e a) }

{-
data EitherIO e a = EitherIO { runEitherIO :: IO (Either e a) }
-}
```

#### Functor Instance

```haskell
instance Functor m => Functor (EitherT e m) where
  fmap f = EitherT . fmap (fmap f) . runEitherT
```

#### Applicative Functor Instance

```haskell
instance Applicative m => Applicative (EitherT e m) where
  pure    = EitherT . pure . Right
  f <*> x = EitherT $ liftA2 (<*>) (runEitherT f) (runEitherT x)

{-
instance Applicative (EitherIO e) where
  pure    = EitherIO . pure . Right
  f <*> x = EitherIO $ liftA2 (<*>) (runEitherIO f) (runEitherIO x)
-}
```

#### Monad Instance

```haskell
instance Monad m => Monad (EitherT e m) where
  return  = EitherT . return . Right
  x >>= f = EitherT $ runEitherT x >>= either (return . Left) (runEitherT . f)

{-
instance Monad (EitherIO e) where
  return  = pure
  x >>= f = EitherIO $ runEitherIO x >>= either (return . Left) (runEitherIO . f)
-}
```

#### Lift

```haskell
lift :: Functor m => m a -> EitherT m e a
lift x = EitherT (fmap Right m)

{-
lift :: Functor m => m a -> EitherT e m a
lift x = EitherT (fmap Right x)
-}
```

## Putting It All Together

Using `EitherT` to wrap `IO` with `Either`-like functionality, our right-leaning `game` function cleans up nicely:

```haskell
game :: IO ()
game = do
  results <- runEitherT $ do
    lift $ putStrLn "I have lived in many cities. Can you guess one?"
    g1  <- EitherT promptForGuess
    m1  <- EitherT $ checkGuess "cities.txt" g1
    lift $ putStrLn "I like a lot of bands. Can you guess one?"
    g2  <- EitherT promptForGuess
    m1  <- EitherT $ checkGuess "bands.txt" g2
    return (m1 ++ " and " ++ m1)
  printResults results

{-

}
```