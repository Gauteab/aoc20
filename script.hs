{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Relude
import Shelly

main :: IO ()
main = shelly $ do
  for_ [1 .. 9] $ \d -> do
    mv ("Day" <> show d <> ".hs") ("Day0" <> show d <> ".hs")
