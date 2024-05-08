module Main where
import StringAlignment
import Test.HUnit
import Data.List(sort)

main =
  runTestTT $
    test
      [ "maximaBy" ~: maximaByTest
      , "similarityScore" ~: similarityScoreTest
      , "optAliggments" ~: optAlignmentsTest
      , "newSimilarityScore" ~: newSimilarityScoreTest
      , "newOptAlignments" ~: newOptAlignmentsTest
      ]

similarityScoreTest = test [similarityScore "writers" "vintner" ~=? -5] 
maximaByTest = test [sort (maximaBy length ["cs", "efd", "lth", "it"]) ~=? sort ["efd", "lth"]]
optAlignmentsTest = test [sort (optAlignments "writers" "vintner") ~=? sort [("writ-ers", "vintner-"), ("wri-t-ers", "-vintner-"), ("wri-t-ers", "v-intner-")]]
newSimilarityScoreTest = test [newSimilarityScore "writers" "vintner" ~=? -5]
newOptAlignmentsTest = test [sort(newOptAlignments "writers" "vintner") ~=? sort [("writ-ers", "vintner-"), ("wri-t-ers", "-vintner-"), ("wri-t-ers", "v-intner-")]]