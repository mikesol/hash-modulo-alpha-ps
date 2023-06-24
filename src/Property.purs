module Property where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (class MonadError, class MonadTrans, ExceptT(..), lift)
import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Reader (class MonadAsk, ReaderT(..))
import Control.Monad.Writer (class MonadTell, WriterT(..), tell)
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Effect.Exception (Error)
import Errors (Errors)
import MonadResult (class MonadResult)
import Test.QuickCheck.Gen (Gen)
import Test.Spec.Assertions (shouldEqual)

newtype PropertyM e g a = PropertyM (WriterT (Array (Effect Unit)) (ExceptT e (ReaderT (Errors e) g)) a)
type Property' = PropertyM Error Gen
type Property = PropertyM Error Gen Unit

derive instance Newtype (PropertyM e g a) _
derive newtype instance Functor g => Functor (PropertyM e g)
derive newtype instance Monad g => Apply (PropertyM e g)
derive newtype instance Monad g => Applicative (PropertyM e g)
derive newtype instance Monad g => Bind (PropertyM e g)
derive newtype instance Monad g => Monad (PropertyM e g)
derive newtype instance Monad g => MonadThrow e (PropertyM e g)
derive newtype instance Monad g => MonadError e (PropertyM e g)
derive newtype instance Monad g => MonadAsk (Errors e) (PropertyM e g)
derive newtype instance Monad g => MonadTell (Array (Effect Unit)) (PropertyM e g)
instance MonadTrans (PropertyM e) where
  lift m = PropertyM $ lift $ lift $ lift m

instance Monad g => MonadResult (PropertyM e g) where
  assertEq l r = tell [ l `shouldEqual` r ]


overReaderT :: forall m. (forall b. m b -> m b) -> (forall r a. ReaderT r m a ->  ReaderT r m a)
overReaderT f (ReaderT r) = ReaderT (f <$> r)

overReaderTF :: forall f m. Functor f => (forall b. f (m b) -> m b) -> (forall r a. f (ReaderT r m a) ->  ReaderT r m a)
overReaderTF f fr = ReaderT \a -> f $ fr <#> ((#) a <<< unwrap)

overWriterT :: forall m. (forall b. m b -> m b) -> (forall w a. WriterT w m a ->  WriterT w m a)
overWriterT f (WriterT w) = WriterT (f w)

overWriterTF :: forall f m. Functor f => (forall b. f (m b) -> m b) -> (forall w a. f (WriterT w m a) ->  WriterT w m a)
overWriterTF f fw = WriterT $ f $ fw <#> unwrap

overExceptT :: forall m. (forall b. m b -> m b) -> (forall e a. ExceptT e m a ->  ExceptT e m a)
overExceptT f (ExceptT e) = ExceptT (f e)

overExceptTF :: forall f m. Functor f => (forall b. f (m b) -> m b) -> (forall e a. f (ExceptT e m a) ->  ExceptT e m a)
overExceptTF f fe = ExceptT $ f $ fe <#> unwrap

chooseIntPM :: forall e g. MonadGen g => Int -> Int -> PropertyM e g Int
chooseIntPM l r = lift $ Gen.chooseInt l r

chooseFloatPM :: forall e g. MonadGen g => Number -> Number -> PropertyM e g Number
chooseFloatPM l r = lift $ Gen.chooseFloat l r

chooseBoolPM :: forall e g. MonadGen g => PropertyM e g Boolean
chooseBoolPM = lift $ Gen.chooseBool

resizePM :: forall e g a. MonadGen g => (Int -> Int) -> PropertyM e g a -> PropertyM e g a
resizePM f (PropertyM pm) = PropertyM $ overWriterT (overExceptT (overReaderT (Gen.resize f))) pm

sizedPM :: forall e g a. MonadGen g => (Int -> PropertyM e g a) -> PropertyM e g a
sizedPM f = PropertyM $ overWriterTF (overExceptTF (overReaderTF Gen.sized)) (unwrap <$> f)

instance MonadGen g => MonadGen (PropertyM e g) where
  chooseInt = chooseIntPM
  chooseFloat = chooseFloatPM
  chooseBool = chooseBoolPM
  resize = resizePM
  sized = sizedPM

