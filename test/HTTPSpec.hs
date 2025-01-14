module HTTPSpec (spec) where

import           Prelude hiding (span)
import           Helper

import           Network.Wai (Application)
import           Test.Hspec.Wai
import qualified System.Console.ANSI as Ansi

import           HTTP
import qualified Trigger

spec :: Spec
spec = do
  describe "app" $ do
    let
      withApp :: (Trigger.Result, String, [Diagnostic]) -> SpecWith ((), Application) -> Spec
      withApp = with . return . app . return

    describe "/" $ do
      context "on success" $ do
        withApp (Trigger.Success, withColor Green "success", []) $ do
          it "returns 200" $ do
            get "/" `shouldRespondWith` fromString (withColor Green "success")

          context "with ?color" $ do
            it "keeps terminal sequences" $ do
              get "/?color" `shouldRespondWith` fromString (withColor Green "success")

          context "with ?color=true" $ do
            it "keeps terminal sequences" $ do
              get "/?color=true" `shouldRespondWith` fromString (withColor Green "success")

          context "with ?color=false" $ do
            it "removes terminal sequences" $ do
              get "/?color=false" `shouldRespondWith` "success"

          context "with an in invalid value for ?color" $ do
            it "returns status 400" $ do
              get "/?color=some%20value" `shouldRespondWith` 400 { matchBody = "invalid value for color: some%20value" }

      context "on failure" $ do
        withApp (Trigger.Failure, withColor Red "failure", []) $ do
          it "return 500" $ do
            get "/" `shouldRespondWith` 500

    describe "/diagnostics" $ do
      let
        start :: Location
        start = Location 23 42

        span :: Maybe Span
        span = Just $ Span "Foo.hs" start start

        err :: Diagnostic
        err = (diagnostic Error) { span, message = ["failure"] }

        expected :: ResponseMatcher
        expected = fromString . decodeUtf8 $ to_json [err]

      withApp (Trigger.Failure, "", [err]) $ do
        it "returns GHC diagnostics" $ do
          get "/diagnostics" `shouldRespondWith` expected

  describe "stripAnsi" $ do
    it "removes ANSI color sequences" $ do
      stripAnsi ("some " <> withColor Green "colorized" <> " text") `shouldBe` "some colorized text"

    it "removes DEC private mode sequences" $ do
      stripAnsi (Ansi.hideCursorCode <> "some text" <> Ansi.showCursorCode) `shouldBe` "some text"
