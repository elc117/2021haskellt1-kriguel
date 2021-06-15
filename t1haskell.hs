{- ################## TRABALHO 1 - PARADIGMAS DE PROGRAMACAO #####################
######################## Prof ANDREA SCHWERTNER CHARAO ###########################
####################### PROGRAMACAO FUNCIONAL - HASKELL ##########################

NOME: LEONARDO CARGNIN KRUGEL
-}

import Text.Printf


type Point     = (Float,Float)
type Rect      = (Point,Float,Float)
type Circle    = (Point,Float)
type Diamond   = (Point,Point,Point,Point)


---------------------------------------------------------------------------------------------------

invertRGB :: (Int,Int,Int) -> (Int,Int,Int)
invertRGB (r,g,b) = (255-r,255-g,255-b)


---------------------------------------------------------------------------------------------------

svgRect :: Rect -> String -> String 
svgRect ((x,y),w,h) style = 
    printf "<rect x='%.3f' y='%.3f' width='%.2f' height='%.2f' style='%s' />\n" x y w h style


svgCirc :: Circle -> String -> String
svgCirc ((x,y),r) style =
    printf "<circle cx='%.3f' cy='%.3f' r='%.3f' style='%s' />\n" x y r style


svgLine :: Point -> Point -> String -> String
svgLine (x1,y1) (x2,y2) style =
    printf "<line x1='%.3f' y1='%.3f' x2='%.3f' y2='%.3f' style='%s' />\n" x1 y1 x2 y2 style


svgFourPointPath :: Diamond-> String -> String
svgFourPointPath ((x0,y0),(x1,y1),(x2,y2),(x3,y3)) style =
    printf "<path d= 'M%.3f %.3f L%.3f %.3f L%.3f %.3f L%.3f %.3f Z' style='%s' />\n" x0 y0 x1 y1 x2 y2 x3 y3 style


---------------------------------------------------------------------------------------------------


svgBegin :: Float -> Float -> String
svgBegin w h = 
    printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 


svgEnd :: String
svgEnd = "</svg>"


svgStyle :: (Int,Int,Int) -> String
svgStyle (r,g,b) = printf "fill:rgb(%d,%d,%d); mix-blend-mode: normal;" r g b


svgElements :: (a -> String -> String) -> [a] -> [String] -> String
svgElements func elements styles = concat $ zipWith func elements styles


---------------------------------------------------------------------------------------------------

main :: IO ()
main = do
    writeFile "image.svg" $ svgstrs
    where svgstrs = svgBegin w h ++ svgfigs ++ svgEnd
          svgfigs = rect ++ diamond ++ circle
          rect = svgRect ((0,0),w,h) (svgStyle (brGreen))
          diamond = svgFourPointPath ((1.7*n,h/2),(w/2,1.7*n),(w-1.7*n,h/2),(w/2,h-1.7*n)) (svgStyle (brYellow))
          circle = svgCirc ((w/2,h/2),3.5*n) (svgStyle (brBlue))
          brGreen = (0,168,89)
          brYellow = (255,204,41)
          brBlue = (62,64,149)
          n = w/20
          (w,h) = (1500,(14/20)*w)
