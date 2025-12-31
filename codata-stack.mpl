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

fun pop :: Stack([Char]) -> [Char] = 
      s -> case Pop(s) of 
            SS((a,_)) -> a 
            _ -> "Fail" 

proc run =
      | console =>
       -> do on console do
              hput ConsolePut
              put pop(Push("Hello world", listStack([])))
	      hput ConsoleClose
	      halt