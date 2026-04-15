-- Hello World on the console

include Prelude

data SF(A) -> C = 
      SS :: A -> C 
      FF :: -> C 

codata S -> Stack(A) = 
      Push :: A, S -> S 
      Pop :: S -> SF((A,S))

fun listStack ::[A] -> Stack(A) = 
      cs -> ( Push := c -> listStack(c:cs), 
              Pop := -> case cs of 
                  [] -> FF
                  b:bs -> SS((b,listStack(bs)))
            )

--fun listStack ::[A] -> Stack(A) = 
--      cs -> unfold cs of 
--            ds => Push: case ds of 
--                   x:- ->    
                  

-- Pops the topmost element of the stack
fun pop :: Stack([Char]) -> [Char] = 
      s -> case Pop(s) of 
            SS((a,_)) -> a 
            _ -> "Fail" 

-- Recursively pops and concatenates it to a list until no elements left
fun flatten :: Stack([Char]) -> [Char] =
  s -> case Pop(s) of
         SS((a, st)) -> a ++ " " ++ flatten(st)
         _ -> ""

proc run =
      | console =>
       -> do on console do
              hput ConsolePut
              put flatten(Push("Hello ", listStack(["Priyaa"])))
	      hput ConsoleClose
	      halt