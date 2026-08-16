{-# LANGUAGE OverloadedStrings #-}

{- | The provenance kernel: GHC is the identity authority. An origin is the
structured identity GHC printed; a façade claim exists only when a probe in
the same package environment reports the identical defining site.
-}
module Test.TypeOriginSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.TypeOrigin (
    Namespace (..),
    OriginId (..),
    annotateExportedLines,
    facadeClaimKey,
    implFlavoured,
    originsFromText,
    probeAccepts,
    rankVerified,
    renderClaim,
 )

-- | The motivating rejection's NB lines, verbatim (note the wrapped pair).
nbLines :: Text
nbLines =
    T.unlines
        [ "      NB: \8216Vector\8217 is defined in \8216Data.Vector.Storable\8217"
        , "          \8216Data.Vector.Unboxed.Base.Vector\8217"
        , "            is defined in \8216Data.Vector.Unboxed.Base\8217"
        ]

unboxedOrigin :: OriginId
unboxedOrigin = OriginId Nothing "Data.Vector.Unboxed.Base" "Vector" NsType

-- | A real @:info@ shape: the type's own defined-in line, then instances.
unboxedProbe :: Text
unboxedProbe =
    T.unlines
        [ "type Vector :: * -> *"
        , "data family Vector a"
        , "  \t-- Defined in \8216Data.Vector.Unboxed.Base\8217"
        , "instance Show (Vector a) -- Defined in \8216Data.Vector.Unboxed\8217"
        ]

probeAt :: Text -> Text
probeAt m =
    T.unlines
        [ "data Vector a"
        , "  \t-- Defined in \8216" <> m <> "\8217"
        ]

spec :: Spec
spec = describe "type origins and verified facades" $ do
    describe "implFlavoured (module names only)" $ do
        it "flags a Base-final module" $
            implFlavoured "Data.Vector.Unboxed.Base" `shouldBe` True
        it "flags an Internal segment anywhere, including leading" $ do
            implFlavoured "Internal.Matrix" `shouldBe` True
            implFlavoured "Data.Text.Internal.Lazy" `shouldBe` True
        it "does not flag lookalike segments" $ do
            implFlavoured "Data.Base64" `shouldBe` False
            implFlavoured "Data.Vector" `shouldBe` False
            implFlavoured "Data.Vector.Storable" `shouldBe` False
        it "flags Control.Monad.Base (silenced later by facade equality)" $
            implFlavoured "Control.Monad.Base" `shouldBe` True

    describe "originsFromText" $ do
        it "reads the transcript's NB lines to one deduped origin" $
            originsFromText nbLines `shouldBe` [unboxedOrigin]
        it "keeps the unit qualifier GHC printed, hash stripped" $
            originsFromText
                "\8216Internal.Matrix.Matrix\8217 is defined in \8216hmatrix-0.20.2-abc123:Internal.Matrix\8217"
                `shouldBe` [ OriginId
                                (Just "hmatrix-0.20.2")
                                "Internal.Matrix"
                                "Matrix"
                                NsType
                           ]
        it "reads a qualified token spelling without a defined-in line" $
            originsFromText
                "Couldn't match type \8216Data.Vector.Unboxed.Base.Vector Double\8217"
                `shouldBe` [unboxedOrigin]
        it "produces nothing for a public defining module" $
            originsFromText
                "\8216Vector\8217 is defined in \8216Data.Vector.Storable\8217"
                `shouldBe` []

    describe "probeAccepts (the GHC identity check)" $ do
        it "accepts a true re-export: the facade reports the defining site" $
            probeAccepts unboxedOrigin unboxedProbe `shouldBe` True
        it "rejects a different type with the same short name" $
            probeAccepts
                unboxedOrigin
                (probeAt "Graphics.Rendering.Chart.Geometry")
                `shouldBe` False
        it "keeps the three vector flavours distinct" $ do
            probeAccepts unboxedOrigin (probeAt "Data.Vector") `shouldBe` False
            probeAccepts unboxedOrigin (probeAt "Data.Vector.Storable")
                `shouldBe` False
            probeAccepts
                (OriginId Nothing "Data.Vector" "Vector" NsType)
                (probeAt "Data.Vector")
                `shouldBe` True
        it "ignores instance defined-in lines" $
            probeAccepts
                unboxedOrigin
                ( T.unlines
                    [ "data Vector a"
                    , "  -- Defined in \8216Data.Vector.Storable\8217"
                    , "instance Eq (Vector a) -- Defined in \8216Data.Vector.Unboxed.Base\8217"
                    ]
                )
                `shouldBe` False
        it "rejects a probe naming a different unit for the same module" $
            probeAccepts
                unboxedOrigin{oiUnit = Just "vector-0.13.2.0"}
                ( T.unlines
                    [ "data family Vector a"
                    , "  -- Defined in \8216vector-0.12.0.0:Data.Vector.Unboxed.Base\8217"
                    ]
                )
                `shouldBe` False
        it "accepts when units agree, and when only one side states one" $ do
            probeAccepts
                unboxedOrigin{oiUnit = Just "vector-0.13.2.0"}
                ( T.unlines
                    [ "data family Vector a"
                    , "  -- Defined in \8216vector-0.13.2.0:Data.Vector.Unboxed.Base\8217"
                    ]
                )
                `shouldBe` True
            probeAccepts
                unboxedOrigin
                ( T.unlines
                    [ "data family Vector a"
                    , "  -- Defined in \8216vector-0.13.2.0:Data.Vector.Unboxed.Base\8217"
                    ]
                )
                `shouldBe` True
        it "rejects missing or nameless evidence" $ do
            probeAccepts unboxedOrigin "data family Vector a" `shouldBe` False
            probeAccepts
                unboxedOrigin
                "data MVector a\n  -- Defined in \8216Data.Vector.Unboxed.Base\8217"
                `shouldBe` False

    describe "rankVerified (preference over verified facades only)" $ do
        it "prefers the longest shared prefix, then shortest, then name" $
            rankVerified
                "Data.Vector.Unboxed.Base"
                ["Data.Vector", "Data.Vector.Unboxed", "Data.Vector.Strict"]
                `shouldBe` [ "Data.Vector.Unboxed"
                           , "Data.Vector"
                           , "Data.Vector.Strict"
                           ]
        it "silences a facade equal to the origin itself" $
            rankVerified "Control.Monad.Base" ["Control.Monad.Base"]
                `shouldBe` []

    describe "annotateExportedLines" $ do
        it "appends the verified fact to the defining line only" $ do
            let out =
                    annotateExportedLines
                        [
                            ( unboxedOrigin{oiUnit = Just "vector-0.13.2.0"}
                            , "Data.Vector.Unboxed"
                            )
                        ]
                        nbLines
            out
                `shouldSatisfy` T.isInfixOf
                    "\8216Data.Vector.Unboxed.Base\8217 (exported by vector:Data.Vector.Unboxed)"
            out
                `shouldSatisfy` T.isInfixOf
                    "\8216Data.Vector.Storable\8217\n"
        it "leaves text byte-identical without claims" $
            annotateExportedLines [] nbLines `shouldBe` nbLines
        it "annotates the -- Defined in spelling too" $
            annotateExportedLines
                [(unboxedOrigin, "Data.Vector.Unboxed")]
                "  -- Defined in \8216Data.Vector.Unboxed.Base\8217"
                `shouldBe` "  -- Defined in \8216Data.Vector.Unboxed.Base\8217 (exported by Data.Vector.Unboxed)"

    describe "claims" $ do
        it "renders pkg:Module when the unit is known, bare otherwise" $ do
            renderClaim
                unboxedOrigin{oiUnit = Just "vector-0.13.2.0"}
                "Data.Vector.Unboxed"
                `shouldBe` "vector:Data.Vector.Unboxed"
            renderClaim unboxedOrigin "Data.Vector.Unboxed"
                `shouldBe` "Data.Vector.Unboxed"
        it "keys claims by short name until two origins collide" $ do
            let matrix = OriginId Nothing "Internal.Matrix" "Matrix" NsType
                clash = OriginId Nothing "Other.Impl.Base" "Vector" NsType
            facadeClaimKey [unboxedOrigin, matrix] unboxedOrigin
                `shouldBe` "Vector"
            facadeClaimKey [unboxedOrigin, clash] unboxedOrigin
                `shouldBe` "Data.Vector.Unboxed.Base.Vector"
