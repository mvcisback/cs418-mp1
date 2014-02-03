import Graphics.UI.GLUT
import Linear
import Data.IORef

delta = 1/4 :: GLfloat
dx = V3 delta 0 0
dy = V3 0 delta 0
o = (V3 0 0 1)
b = o - 1.5*dx + 2*dy
a = b + dy
c = b + dx
d = b + 2*dx
e = b + 3*dx
f = a + 3*dx
g = c - 2*dy
h = d - 2*dy
i = b - 2*dy
j = e - 2*dy
k = i - dy
l = j - dy

myPoints :: [V3 GLfloat]    
myPoints = [a, b, c, g, i, k, l, j, h, d, e, f]
genPoints t = map move myPoints
    where move p = (wiggle t p) !* p

w = 1
wiggle :: GLfloat -> V3 GLfloat -> M33 GLfloat
wiggle t (V3 x y _) = V3 (V3 1 0 offset) (V3 0 1 offset) (V3 0 0 1)
    where offset = (sin (w*t)) * x * y

 
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
  renderPrimitive LineLoop $
     mapM_ (\(V3 x y z) -> vertex $ Vertex3 x y z) $ (genPoints t)
  flush
