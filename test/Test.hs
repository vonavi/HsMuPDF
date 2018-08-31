module Main
  ( main
  ) where

import           Control.Monad      ((>=>))
import           Test.Tasty         (defaultMain)
import           Test.Tasty.HUnit   (testCase, (@?=))

import           MuPDF.Pdf.Document (countPages, withDocument)
import           Paths_HsMuPDF      (getDataFileName)

main :: IO ()
main = defaultMain $ do
  testCase "Check page count" $ do
    pdf <- getDataFileName "adobexmlformssamples.pdf"
    withDocument pdf $ countPages >=> (@?= 10)
