module SessionSpec (spec) where

import           Helper

import           System.Environment.Blank (setEnv)

import           Language.Haskell.GhciWrapper (eval)
import qualified Session
import           Session hiding (withSession, runSpec)

withSession :: [String] -> (Session -> IO a) -> IO a
withSession = Session.withSession ghciConfig

spec :: Spec
spec = do
  describe "withSession" $ do
    it "unsets HSPEC_FAILURES" $ do
      setEnv "HSPEC_FAILURES" "foo" True
      withSession [] $ \ Session{..} -> do
        eval interpreter "System.Environment.lookupEnv \"HSPEC_FAILURES\"" `shouldReturn` "Nothing\n"

    context "with `:set +t +s`" $ do
      it "works just fine" $ do
        withTempDirectory $ \ dir -> do
          let
            config = ghciConfig {
              configIgnoreDotGhci = False
            , configWorkingDirectory = Just dir
            }
          writeFile (dir </> ".ghci") ":set +t +s"
          Session.withSession config [] $ \ Session{..} -> do
            eval interpreter "23" `shouldReturn` "23\n"

    context "with -XOverloadedStrings" $ do
      it "works just fine" $ do
        withSession ["-XOverloadedStrings", "-Wall", "-Werror"] $ \ Session{..} -> do
          eval interpreter "23 :: Int" `shouldReturn` "23\n"

  describe "reload" $ do
    it "reloads" $ do
      withSession [] $ \ session -> do
        Session.reload session `shouldReturn` ("", Ok)

  describe "hasSpec" $ around withSomeSpec $ do
    context "when module contains spec" $ do
      it "returns True" $ \ name -> do
        withSession [name] $ \session -> do
          _ <- Session.reload session
          Session.hasSpec hspecCommand session `shouldReturn` True

    context "when module does not contain spec" $ do
      it "returns False" $ \ name -> do
        withSession [name] $ \session -> do
          writeFile name "module Main where"
          _ <- Session.reload session
          Session.hasSpec hspecCommand session `shouldReturn` False

  describe "hasHspecCommandSignature" $ do
    let signature = "Test.Hspec.Runner.hspecResult spec :: IO Test.Hspec.Core.Runner.Summary"

    context "when input contains qualified Hspec command signature" $ do
      it "returns True" $ do
        Session.hasHspecCommandSignature hspecCommand signature `shouldBe` True

      it "ignores additional output after summary" $ do
        (Session.hasHspecCommandSignature hspecCommand . unlines) [
            "bar"
          , signature
          , "foo"
          ] `shouldBe` True

    context "when input contains unqualified Hspec command signature" $ do
      it "returns True" $ do
        Session.hasHspecCommandSignature hspecCommand "Test.Hspec.Runner.hspecResult spec :: IO Summary" `shouldBe` True

    context "when input dose not contain Hspec command signature" $ do
      it "returns False" $ do
        Session.hasHspecCommandSignature hspecCommand "foo" `shouldBe` False

  describe "runSpec" $ around withSomeSpec $ do
    let runSpec = Session.runSpec hspecCommand
    it "stores summary of spec run" $ \ name -> do
      withSession [name] $ \session -> do
        _ <- runSpec session >> runSpec session
        hspecPreviousSummary session `shouldReturn` Just (Summary 2 0)

    it "accepts Hspec args" $ \ name -> do
      withSession [name, "--no-color", "-m", "foo"] $ \session -> do
        _ <- runSpec session >> runSpec session
        hspecPreviousSummary session `shouldReturn` Just (Summary 1 0)

  describe "extractSummary" $ do
    let
      summary :: Summary
      summary = Summary 0 0

      input :: ByteString
      input = fromString $ show summary

    it "extracts summary" $ do
      extractSummary.parseMessage input `shouldBe` Just (summary, "")

    context "when the input starts with the ANSI \"show cursor\"-sequence" $ do
      it "extracts summary" $ do
        extractSummary.parseMessage (ansiShowCursor <> input) `shouldBe` Just (summary, ansiShowCursor)
