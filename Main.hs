import Graphics.UI.GLUT
import Linear

delta = 1/4 :: GLfloat
dx = V3 delta 0 0
dy = V3 0 delta 0
o = (V3 0 0 1)
b = o - 1.5*dx + 2*dy
a = b + dy
c = b + dx

reflectX :: M33 GLfloat
reflectX = V3 (V3 (-1) 0 0) (V3 0 1 0) (V3 0 0 1)

reflectY :: M33 GLfloat
reflectY = V3 (V3 1 0 0) (V3 0 (-1) 0) (V3 0 0 1)

-- Wiggle rates
kX = 5
kY = 5
wX = 1
wY = 1

wiggle :: GLfloat -> V3 GLfloat -> M33 GLfloat
wiggle t (V3 x y _) = V3 (V3 1 0 dx') (V3 0 1 dy') (V3 0 0 1)
    where offset r k w = sin (k*r+w*t)
          dx' = offset x kX wX
          dy' = offset y kY wY
          
    
base :: [V3 GLfloat]    
base = [a, b, c]
quad = base ++ map (reflectX !*) base
myPoints = quad ++ map (reflectY !*) quad
myPoints2 = map move myPoints
    where move point = (wiggle 0 point) !* point

 
main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "MP1"
  displayCallback $= display
  mainLoop
 
display :: DisplayCallback
display = do 
  clear [ColorBuffer]
  renderPrimitive Points $
     mapM_ (\(V3 x y z) -> vertex $ Vertex3 x y z) myPoints2
  flush
