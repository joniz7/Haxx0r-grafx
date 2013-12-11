module Main where
import Data.Maybe
import Expr
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC

main :: IO ()
main =
   do initGUI
      win <- windowNew
      windowSetTitle win "Calculator !!!!!"
      win `onDestroy` mainQuit
      
      can <- drawingAreaNew   
      can `onSizeRequest` return (Requisition 500 500)
      
      calc <- buttonNewWithLabel "Draw"
      
      entry <- entryNew
      
      calc `onClicked` parseAndDraw entry can
      
      buttons <- hBoxNew False 5
      layout <- vBoxNew False 5
      
      containerAdd buttons entry
      containerAdd buttons calc
      
      containerAdd layout can
      containerAdd layout buttons
      
      containerAdd win layout
      
      widgetShowAll win
      mainGUI

parseAndDraw :: Entry -> DrawingArea -> IO ()
parseAndDraw en can = 
   do dw <- widgetGetDrawWindow can
      drawWindowClear dw
      s <- entryGetText en
      drawFunction can (points (fromJust (readExpr s)) 0.05 (250, 250))
      return ()
      
drawFunction :: DrawingArea -> [Point] -> IO ()
drawFunction  _  []        = return ()
drawFunction  _  (p:[])      = return ()
drawFunction can (p:(p':ps)) = 
   do dw <- widgetGetDrawWindow can
      gc <- gcNew dw
      drawLine dw gc p p'
      drawFunction can (p':ps)
      
points :: Expr -> Double -> (Int, Int) -> [Point]
points e s (maxX, maxY) = [(round(x/s) + maxX, round ((eval e x)/s) + maxY) | x <- interval (-maxX) maxX s, 
                           eval e (x*s) < fromIntegral maxY, eval e (x*s) > (fromIntegral (-maxY))]
                           
interval :: Int -> Int -> Double -> [Double]
interval l u i = [l', l'+i..u'-i]
   where 
      l' = (fromIntegral l)*i
      u' = (fromIntegral u)*i