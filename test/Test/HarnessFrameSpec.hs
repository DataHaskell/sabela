{-# LANGUAGE OverloadedStrings #-}

{- | GHC reports context frames innermost first, so the frames naming the
harness's own wrapper are always the outermost ones. Everything from the first
of them onward is noise the candidate's author cannot act on.
-}
module Test.HarnessFrameSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.Errors (scrubHarnessFrames)

-- | Verbatim from `try {code: "length \"abc\" + True"}`, escaping and all.
realBlob :: Text
realBlob =
    "In the second argument of `(+)', namely `True'\\n\
    \In the first argument of `\\\\ _sabelaCandidate\\n\
    \                            -> if (==)\\n\
    \                                    (Data.Typeable.typeRepTyCon\\n\
    \                                       (Data.Typeable.typeOf _sabelaCandidate))', namely\\n\
    \  `(length \"abc\" + True)'\\n\
    \In the first argument of `ghc-internal-9.1202.0:GHC.Internal.GHCi.ghciStepIO ::\\n\
    \                            IO String -> IO String', namely\\n\
    \  `((\\\\ _sabelaCandidate -> ...) (length \"abc\" + True))'"

{- | What GHC 9.6 actually emits: bulleted, with continuation frames indented
under the bullet. The escaped fixture above lost the indentation, so the
scrubber passed its own tests while never firing on a real diagnostic.
-}
bulletedBlob :: Text
bulletedBlob =
    "<interactive>:1:574: error: [GHC-83865]\n\
    \    \8226 Couldn't match expected type \8216Int\8217 with actual type \8216Bool\8217\n\
    \    \8226 In the second argument of \8216(+)\8217, namely \8216True\8217\n\
    \      In the first argument of \8216\\ _sabelaCandidate\n\
    \                                  -> if (==)\n\
    \                                          (Data.Typeable.typeRepTyCon\n\
    \                                             (Data.Typeable.typeOf _sabelaCandidate))\8217, namely\n\
    \        \8216(length \"abc\" + True)\8217\n\
    \      In the first argument of \8216GHC.GHCi.ghciStepIO ::\n\
    \                                  IO String -> IO String\8217, namely\n\
    \        \8216((\\ _sabelaCandidate -> ...) (length \"abc\" + True))\8217"

spec :: Spec
spec = describe "the harness's own stack frames never reach the model" $ do
    it "fires on GHC's bulleted, indented frames" $ do
        let scrubbed = scrubHarnessFrames bulletedBlob
        scrubbed `shouldSatisfy` T.isInfixOf "Couldn't match expected type"
        scrubbed `shouldSatisfy` T.isInfixOf "In the second argument"
        scrubbed `shouldNotSatisfy` T.isInfixOf "_sabelaCandidate"
        scrubbed `shouldNotSatisfy` T.isInfixOf "ghciStepIO"
        scrubbed `shouldNotSatisfy` T.isInfixOf "Data.Typeable"

    it "keeps every frame about the candidate's own code" $
        scrubHarnessFrames realBlob
            `shouldBe` "In the second argument of `(+)', namely `True'"

    it "drops the wrapper frames whole, not line by line" $ do
        let scrubbed = scrubHarnessFrames realBlob
        scrubbed `shouldNotSatisfy` T.isInfixOf "_sabelaCandidate"
        scrubbed `shouldNotSatisfy` T.isInfixOf "ghciStepIO"
        scrubbed `shouldNotSatisfy` T.isInfixOf "Data.Typeable"

    it "leaves a diagnostic that never touched the wrapper alone" $ do
        let clean =
                "In the second argument of `(+)', namely `True'\n\
                \In the expression: length \"abc\" + True"
        scrubHarnessFrames clean `shouldBe` clean

    it "handles real newlines as well as escaped ones" $ do
        let raw =
                "In the expression: foo\n\
                \In the first argument of `\\ _sabelaCandidate -> bar', namely `foo'"
        scrubHarnessFrames raw `shouldBe` "In the expression: foo"

    it "keeps the headline when every frame is the wrapper's" $ do
        let allNoise =
                "Couldn't match expected type `Int'\\n\
                \In the first argument of `\\\\ _sabelaCandidate -> x', namely `y'"
        scrubHarnessFrames allNoise `shouldBe` "Couldn't match expected type `Int'"

    it "is a no-op on text with no frames at all" $
        scrubHarnessFrames "Variable not in scope: combine"
            `shouldBe` "Variable not in scope: combine"

    it "drops a bare marker with no frame boundary rather than keeping it" $
        scrubHarnessFrames "---SABELA_PURE_ADMITTED---" `shouldBe` ""

    it "keeps the hole fits, which GHC prints after the context frames" $ do
        let holeBlob =
                "Found hole: _ :: Picture -> Picture -> Picture\\n\
                \In the first argument of `displayPicture', namely `(_ s c)'\\n\
                \In the first argument of `\\\\ _sabelaCandidate -> wrapper', namely `x'\\n\
                \Relevant bindings include\\n\
                \  _compileParsedExpr :: IO String\\n\
                \Valid hole fits include\\n\
                \  mappend :: forall a. Monoid a => a -> a -> a\\n\
                \    with mappend @Picture"
            scrubbed = scrubHarnessFrames holeBlob
        scrubbed `shouldSatisfy` T.isInfixOf "Valid hole fits include"
        scrubbed `shouldSatisfy` T.isInfixOf "mappend"
        scrubbed `shouldSatisfy` T.isInfixOf "Found hole"
        scrubbed `shouldSatisfy` T.isInfixOf "namely `(_ s c)'"
        scrubbed `shouldNotSatisfy` T.isInfixOf "_sabelaCandidate"
