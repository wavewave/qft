module Main where

import Tensor



a = Tensor (s "X") [i "I"]

b = Tensor (s "Y") [i "J"]

c = Tensor (s "X") [i "K"]

xl = Tensor (s "X") [i "L"]



prod1 = Product [a,b,c] [] 


yukawa :: Graph
yukawa = Graph [ Tensor (s "y") [i "I", i "J"]
               , Tensor (s "Q") [i "alpha", i "I"]
               , Tensor (s "U") [i "beta", i "J"]
               , Tensor (s "H") []
               , Tensor (s "Gamma") [i "alpha", i "beta"]
               ] 
               [ mkEdge (i "I") (s "y") (s "Q")
               , mkEdge (i "J") (s "y") (s "U")
               , mkEdge (i "alpha") (s "Q") (s "Gamma")
               , mkEdge (i "beta") (s "U") (s "Gamma")
               ] 




main :: IO ()
main = do 
  print prod1  
  print (product2Tensor (s "C") prod1)

  print (getHoles [] [1,2,3,4,5])
  (mapM_ print . unSum . diff prod1) xl

  putStrLn "------__"

  print $ concatMap indices (tensorlist yukawa)

  print (checkSinglet yukawa)

  print (graph2Product yukawa)