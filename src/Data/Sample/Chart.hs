{-|
 Module         : Data.Sample.Chart
 Description    : Quick and dirty plotting functions.
 License        : GPL-3
 Maintainer     : hackage@mail.kevinl.io
 Stability      : experimental

 This module contains some very quick and dirty
 functions to plot the results obtained from
 running stochastic processes.

-}
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

-- | Easy function to make a plot. Really awful, don't use this.
processToChart :: String  -- ^ Title of the plot.
          -> S.Seq (S.Seq Double)  -- ^ Lines to plot.
          -> EC (Layout Integer Double) ()
processToChart t ss = do
    layout_title .= t
    plot $ seqLines t ss

-- | Convenience function to make a histogram.
histogram :: String -> [Double] -> Layout Double Double
histogram title values = layout
    where hist = plot_hist_values  .~ values
                 $ plot_hist_drop_lines .~ True
                 $ defaultFloatPlotHist
          layout :: Layout Double Double
          layout = layout_title .~ title
                 $ layout_plots .~ [ histToPlot hist ]
                 $ def
