import Graphics.UI.GLUT
import Linear
import Data.IORef

epsilon = 1.9
dx = V3 (1/4) 0 0
dy = V3 0 (1/4) 0
o = V3 0 0 1
a = o - 2*dx + 3*dy
b = o + 2*dx + 3*dy
c = o - 2*dx + 2*dy
d = o + 2*dx + 2*dy
e = o - dx + epsilon*dy
f = o + dx + epsilon*dy
g = o - dx - epsilon*dy
h = o + dx - epsilon*dy
i = o - 2*dx - 2*dy
j = o + 2*dx - 2*dy
k = o - 2*dx - 3*dy
l = o + 2*dx - 3*dy


w = 1
wiggle :: GLfloat -> V3 GLfloat -> M33 GLfloat
wiggle t (V3 x y _) = V3 (V3 1 0 offset) (V3 0 1 offset) (V3 0 0 1)
    where offset = sin (w*t) * x * y 

render primative points t = renderPrimitive primative $
                            mapM_ (\(V3 x y z) -> vertex $ Vertex3 x y z) (genPoints t)
    where genPoints t = map move points
              where move p = wiggle t p !* p

render1 = render TriangleStrip points
          where points :: [V3 GLfloat]
                points = [a, b, c, d, e, f, g, h, i, j, k, l]

render2 = render Lines points
          where points :: [V3 GLfloat]
                points = [a, b, a, c , b, c , b, d, c, d, c, e, d, e , d, f , e
                         ,f , e, g, f, h, g, h, g, i, h, i, h, j, i, j, i, k
                         ,j , k, j, l, k, l]

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "MP1"
  tp <- newIORef (0 :: GLfloat)
  displayCallback $= display tp
  idleCallback $= Just (idle tp)
  mainLoop
 
idle :: IORef GLfloat -> IdleCallback
idle tp = do
  tp $~! (+ 0.001)
  postRedisplay Nothing

display :: IORef GLfloat-> DisplayCallback
display tp = do 
  clear [ColorBuffer]
  t <- readIORef tp
  (if round t `mod` 5 < 2 then render1 else render2) t
  swapBuffers
