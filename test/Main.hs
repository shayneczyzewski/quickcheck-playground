module Main where

import Data.Maybe (catMaybes)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    choose,
    elements,
    generate,
    listOf,
    listOf1,
  )

-- TODO: Goal here is to generate only valid programs.
-- Not sure how to best to generate valid and invalid yet.
data ValidProgram = ValidProgram
  { app :: App,
    pages :: Maybe [Page],
    routes :: Maybe [Route]
  }
  deriving (Show)

instance Arbitrary ValidProgram where
  arbitrary = do
    let app = arbitrary
    (pages, routes) <- genPagesRoutes
    ValidProgram <$> app <*> return pages <*> return routes

data App = App
  { appIdentifier :: ElementIdentifier,
    titleProp :: String,
    headProp :: Maybe [String]
  }
  deriving (Show)

instance Arbitrary App where
  arbitrary = App <$> arbitrary <*> arbitrary <*> arbitrary

data Page = Page
  { pageIdentifier :: ElementIdentifier,
    componentProp :: String,
    authRequiredProp :: Maybe Bool
  }
  deriving (Show)

instance Arbitrary Page where
  arbitrary = Page <$> arbitrary <*> arbitrary <*> arbitrary

data Route = Route
  { routePath :: String,
    pageRef :: ElementIdentifier
  }
  deriving (Show)

-- TODO: Any better way to generate Pages and Routes
-- such that the Routes reference generated Pages?
genPagesRoutes :: Gen (Maybe [Page], Maybe [Route])
genPagesRoutes = do
  pages <- arbitrary :: Gen (Maybe [Page])
  routes <-
    case pages of
      Nothing -> return Nothing
      Just pages' -> Just <$> listOf (Route <$> urlGen <*> pageIdentifierGen pages')
  return (pages, routes)
  where
    urlGen = listOf1 . elements $ '/' : ['a' .. 'z']
    pageIdentifierGen pages' = elements (map pageIdentifier pages')

newtype ElementIdentifier = ElementIdentifier {elementIdentifier :: String} deriving (Show)

instance Arbitrary ElementIdentifier where
  arbitrary = ElementIdentifier <$> listOf1 (choose ('A', 'Z'))

main :: IO ()
main = do
  e <- generate (arbitrary :: Gen ValidProgram)
  print e
  return ()
