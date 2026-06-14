{- | Tests for @Sabela.Notebook.Anim@: frame sampling, the embedded player, and
the @animateB == animate . at@ relationship.
-}
module Test.NotebookAnimSpec (spec) where

import Data.List (isInfixOf)
import Sabela.Notebook.Anim
import Sabela.Notebook.Behavior (at, time)
import Sabela.Notebook.Picture (
    Picture,
    circle,
    defaultCanvas,
    fill,
    fromSvg,
    red,
    renderSvg,
 )
import Test.Hspec

-- A simple moving picture: a circle sliding right with time.
slide :: Double -> Picture
slide t = fill red (circle (t, 100) 10)

spec :: Spec
spec = do
    describe "frameSvgs" $ do
        it "produces fps*seconds frames" $
            length (frameSvgs defaultAnim 2 slide) `shouldBe` 60 -- 2s * 30fps
        it "honours a custom fps" $
            length (frameSvgs (AnimOpts defaultCanvas 10) 3 slide) `shouldBe` 30
        it "always yields at least one frame" $
            length (frameSvgs (AnimOpts defaultCanvas 30) 0 slide) `shouldBe` 1
        it "samples evenly from t=0" $
            take 1 (frameSvgs (AnimOpts defaultCanvas 10) 1 slide)
                `shouldBe` [renderSvg defaultCanvas (slide 0)]

    describe "renderAnimation HTML" $ do
        let html = renderAnimation defaultAnim 1 slide
        it "embeds a requestAnimationFrame player" $
            html `shouldSatisfy` isInfixOf "requestAnimationFrame"
        it "carries the snapshots as SVG" $ html `shouldSatisfy` isInfixOf "<svg"
        it "neutralises a </script> in a frame (escapes it as <\\/script>)" $
            renderAnimation defaultAnim 1 (const (fromSvg "</script>"))
                `shouldSatisfy` isInfixOf "<\\/script>"

    describe "animateB matches animate over the sampled behaviour" $
        it "renders identically to the plain time function" $
            renderAnimation defaultAnim 1 (at (fmap (fill red . circle (5, 5)) (10 + time)))
                `shouldBe` renderAnimation defaultAnim 1 (\t -> fill red (circle (5, 5) (10 + t)))
