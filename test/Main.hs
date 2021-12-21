module Main where

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
    pages :: [Page],
    routes :: [Route]
  }
  deriving (Show)

instance Arbitrary ValidProgram where
  arbitrary = do
    app <- arbitrary
    (pages, routes) <- genPagesRoutes
    return $ ValidProgram app pages routes

data App = App
  { appIdentifier :: ElementIdentifier,
    titleProp :: String,
    headProp :: [String]
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

genPagesRoutes :: Gen ([Page], [Route])
genPagesRoutes = do
  pages <- arbitrary :: Gen [Page]
  routes <-
    if null pages
      then return []
      else listOf (Route <$> urlGen <*> pageIdentifierGen pages)
  return (pages, routes)
  where
    urlGen = listOf1 . elements $ '/' : ['a' .. 'z']
    pageIdentifierGen pages = elements (map pageIdentifier pages)

newtype ElementIdentifier = ElementIdentifier {elementIdentifier :: String} deriving (Show)

instance Arbitrary ElementIdentifier where
  arbitrary = ElementIdentifier <$> listOf1 (choose ('A', 'Z'))

main :: IO ()
main = do
  e <- generate (arbitrary :: Gen ValidProgram)
  print e
  return ()
