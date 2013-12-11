module Main where
import Data.Maybe
import Expr
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC

-- Our main function which initializes the program
main :: IO ()
main =
   do initGUI
      --Creates the main window
      win <- windowNew
      windowSetTitle win "Calculator"
      win `onDestroy` mainQuit
      
      --Creates our drawing area
      can <- drawingAreaNew   
      can `onSizeRequest` return (Requisition 500 500)
      
      --Creates the buttons
      calc <- buttonNewWithLabel "Draw"
      der <- buttonNewWithLabel "Derive"
      
      --Creates our text-field
      entry <- entryNew
      
      --Determines what functions will be called 
      --upon when pushing the buttons
      calc `onClicked` parseAndDraw entry can
      der `onClicked` deriveAndDraw entry can
      
      --Defines the layout
      buttons <- hBoxNew False 5
      layout <- vBoxNew False 5
      containerAdd buttons entry
      containerAdd buttons calc
      containerAdd buttons der
      containerAdd layout can
      containerAdd layout buttons
      containerAdd win layout
      
      widgetShowAll win
      mainGUI

--A function that will parse what is in the text-field
--and draw the function on the canvas.
parseAndDraw :: Entry -> DrawingArea -> IO ()
parseAndDraw en can = 
   do dw <- widgetGetDrawWindow can
      drawWindowClear dw
      s <- entryGetText en
      (drawFunction can (points (safeFromJust (readExpr s)) 0.05 (250, 250)))
      return ()
      
--A function that will parse what is in the text-field
--derive it, and draw the function on the canvas.
deriveAndDraw :: Entry -> DrawingArea -> IO ()
deriveAndDraw en can =
   do dw <- widgetGetDrawWindow can
      drawWindowClear dw
      s <- entryGetText en
      entrySetText en (showExpr(derive (safeFromJust (readExpr s))))
      drawFunction can (points (derive (safeFromJust (readExpr s))) 
                                                      0.05 (250,250))
      return ()
      
--A wrapper for fromJust that returns a number larger that the drawing
--area if the expression is Nothing

safeFromJust :: Maybe Expr -> Expr
safeFromJust e | isNothing e = Num 501
               | otherwise   = fromJust e
      
--Main drawing function. Takes a drawing area and a list of points, and
--draws the points on the drawing area.
drawFunction :: DrawingArea -> [Point] -> IO ()
drawFunction  _  []          = return ()
drawFunction  _  (p:[])      = return ()
drawFunction can (p:(p':ps)) = 
   do dw <- widgetGetDrawWindow can
      gc <- gcNew dw
      drawLine dw gc p p'
      drawFunction can (p':ps)

--Takes an expression, a scale and dimension for a coordinate system
--and generates a list of points from the expression with the right
--scale.
points :: Expr -> Double -> (Int, Int) -> [Point]
points e s (maxX, maxY) = [(round(x/s) + maxX, 
                           toBottomLeft (round ((eval e x)/s)+maxY) (maxY*2))
                           | x <- interval (-maxX) maxX s, 
                           eval e (x*s) < fromIntegral maxY, 
                           eval e (x*s) > (fromIntegral (-maxY))]

--Generates a list of doubles between two values with a given interval
interval :: Int -> Int -> Double -> [Double]
interval l u i = [l', l'+i..u'-i]
   where 
      l' = (fromIntegral l)*i
      u' = (fromIntegral u)*i
      
--Transforms a given y-coordinate from a coordinate system with the
--origin at the top left corner to one with the origin in the bottom right
toBottomLeft :: Int -> Int -> Int
toBottomLeft y max = (max - y)