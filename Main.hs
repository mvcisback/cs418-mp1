import Graphics.UI.GLUT
import Linear
import Data.Time.Clock.POSIX

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


w = 2*pi
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
  let _progName = "MP1"
  _args <- initialize _progName []
  _window <- createWindow _progName
  displayCallback $= display
  idleCallback $= Just idle
  mainLoop
 
idle :: IdleCallback
idle = do
  postRedisplay Nothing

display :: DisplayCallback
display = do 
  clear [ColorBuffer]
  t <- getPOSIXTime
  (if round t `mod` 5 < 2 then render1 else render2) (stepSize t)
  swapBuffers

stepSize :: POSIXTime -> GLfloat
stepSize t = (fromRational . toRational) x :: GLfloat
    where x = abs $ (fromIntegral . truncate $ t) - t
