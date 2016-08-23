module Data.Sample.Chart where

import Control.Monad

import Data.Foldable

import Data.Sample.Types

import qualified Data.Sequence as S

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

-- | Create lines with the same title.
seqLines :: String -> S.Seq (S.Seq Double) -> EC l (PlotLines Integer Double)
seqLines t ss = line t $ toList $ fmap (\x -> zip [1..] $ toList x) ss

-- | Easy function to make a 
mcToChart :: String  -- | Title of the plot.
          -> S.Seq (S.Seq Double)  -- | Lines to plot.
          -> EC (Layout Integer Double) ()
mcToChart t ss = do
    layout_title .= t
    plot $ seqLines t ss
