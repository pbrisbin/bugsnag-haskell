module Foo where

data What = What
  deriving (Show)

what :: Int -> Maybe What
what 0 = Just What
what _ = Nothing
