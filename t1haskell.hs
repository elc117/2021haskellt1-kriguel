{--------------------------------------------------------------------------------------------------------------------
|                                     TRABALHO 1 - PARADIGMAS DE PROGRAMACAO                                        |
|                                         PROGRAMACAO FUNCIONAL - HASKELL                                           | 
---------------------------------------------------------------------------------------------------------------------
    Prof Andrea Schwertner Charao
    Aluno: Leonardo Cargnin Krugel
-}-------------------------------------------------------------------------------------------------------------------

{--------------------------------------------------------------------------------------------------------------------
|                                          Imports e definicoes                                                     |
-}-------------------------------------------------------------------------------------------------------------------

import Text.Printf

type Color     = (Int,Int,Int)
type Point     = (Float,Float)
type Rect      = (Point,Float,Float)
type Circle    = (Point,Float,Int)
type Triangle  = (Point,Point,Point)


{--------------------------------------------------------------------------------------------------------------------
|                                       Funcoes que manipulam cores                                                 |
-}-------------------------------------------------------------------------------------------------------------------

invertRGB :: Color -> Color
invertRGB (r,g,b) = (255-r,255-g,255-b)


colorRGB :: Color -> [Color]
colorRGB (r,g,b) = [original,inverted]
        where original = (r,g,b)
              inverted = invertRGB (r,g,b)

{--------------------------------------------------------------------------------------------------------------------
|                                 Funcoes que constroem a bandeira do Brasil                                        |
-}-------------------------------------------------------------------------------------------------------------------

flagBR :: (Float,Float) -> String
flagBR (w,h) = rectangle ++ diamond ++ circle
        where rectangle = svgElements svgRect (flagBRRects (w,h)) (map svgStyle (colorRGB brGreen))
              diamond = svgElements svgThreePointPath (flagBRTriangs (w,h)) (map svgStyle (colorRGB brYellow))
              circle = svgElements svgSemiCircle (flagBRCircs (w,h)) (map svgStyle (colorRGB brBlue))
              brGreen = (0,168,89)
              brYellow = (255,204,41)
              brBlue = (62,64,149)


flagBRRects :: (Float,Float) -> [Rect]
flagBRRects (w,h) = [rectangleLeft,rectangleRight]
        where rectangleLeft = ((0.0,0.0),w/2,h)
              rectangleRight = ((w/2,0),w,h)


flagBRCircs :: (Float,Float) -> [Circle]
flagBRCircs (w,h) = [circleLeft,circleRight]
        where circleLeft = ((w/2,h/2),(3.5*n),1)
              circleRight = ((w/2,h/2),(3.5*n),0)
              n = w/20


flagBRTriangs :: (Float,Float) -> [Triangle]
flagBRTriangs (w,h) = [triangleLeft,triangleRight]
        where triangleLeft = ((1.7*n,h/2),(w/2,1.7*n),(w/2,h-1.7*n))
              triangleRight = ((w-1.7*n,h/2),(w/2,1.7*n),(w/2,h-1.7*n))
              n = w/20


{--------------------------------------------------------------------------------------------------------------------
|                                      Funcoes das figuras svg                                                      |
-}-------------------------------------------------------------------------------------------------------------------

svgRect :: Rect -> String -> String 
svgRect ((x,y),w,h) style = 
    printf "<rect x='%.3f' y='%.3f' width='%.2f' height='%.2f' style='%s' />\n" x y w h style


svgThreePointPath :: Triangle -> String -> String
svgThreePointPath ((x0,y0),(x1,y1),(x2,y2)) style =
    printf "<path d='M%.3f,%.3f L%.3f,%.3f L%.3f,%.3f Z' style='%s' />\n" x0 y0 x1 y1 x2 y2 style


svgSemiCircle :: Circle -> String -> String
svgSemiCircle ((xc,yc),r,side) style =
    printf "<path d='M%.3f,%.3f L%.3f,%.3f A%.1f,%.1f 0 1,%d %.3f,%.3f' style='%s' />\n" 
            xc (yc-r) xc (yc+r) r r side xc (yc-r) style


{--------------------------------------------------------------------------------------------------------------------
|                                          Funcoes gerais svg                                                       |
-}-------------------------------------------------------------------------------------------------------------------


svgBegin :: Float -> Float -> String
svgBegin w h = 
    printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 


svgEnd :: String
svgEnd = "</svg>"


svgStyle :: (Int,Int,Int) -> String
svgStyle (r,g,b) = printf "fill:rgb(%d,%d,%d); mix-blend-mode: normal;" r g b


svgElements :: (a -> String -> String) -> [a] -> [String] -> String
svgElements func elements styles = concat $ zipWith func elements styles


{--------------------------------------------------------------------------------------------------------------------
|                                           Funcao main                                                             |
-}-------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do
    writeFile "image.svg" $ svgstrs
    where svgstrs = svgBegin w h ++ svgfigs ++ svgEnd
          svgfigs = flagBR (w,h)
          (w,h) = (1500,(14/20)*w)
