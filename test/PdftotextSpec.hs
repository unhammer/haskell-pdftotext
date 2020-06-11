{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PdftotextSpec (spec) where

import qualified Data.Text.IO as T
import Pdftotext
import Test.Hspec

spec :: Spec
spec = do
  before (openFile "test/simple.pdf") do
    describe "pdftotext with layout `None`" do
      it "produces same output as `pdftotext simple.pdf`" \(Just doc) -> do
        exp <- T.readFile "test/simple_none.txt"
        pdftotext None doc `shouldBe` exp

    describe "pdftotext with layout `Raw`" do
      it "produces same output as `pdftotext -raw simple.pdf`" \(Just doc) -> do
        exp <- T.readFile "test/simple_raw.txt"
        pdftotext Raw doc `shouldBe` exp

    describe "pdftotext with layout `Physical`" do
      it "produces same output as `pdftotext -layout simple.pdf`" \(Just doc) -> do
        exp <- T.readFile "test/simple_physical.txt"
        pdftotext Physical doc `shouldBe` exp

    describe "PDF" do
      it "should have expected number of pages (`pagesTotal`)" \(Just doc) ->
        pagesTotal doc `shouldBe` 4
      it "should contain correct number of pages (`pages`)" \(Just doc) ->
        length (pages doc) `shouldBe` 4

    describe "PDF properties" do
      it "should contain all fields" \(Just doc) -> do
        let Properties {..} = properties doc
        author `shouldBe` Just "G. Eyaeb"
        title `shouldBe` Just "Simple document for testing"
        creator `shouldBe` Just "pdflatex"
        producer `shouldBe` Just "LaTeX with hyperref"
        keywords `shouldBe` Just "haskell,pdf"
        subject `shouldBe` Just "Testing"
