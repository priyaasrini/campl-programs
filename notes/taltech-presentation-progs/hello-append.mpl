include Prelude

-- The 'append' function
fun append :: [A],[A] -> [A] =
    [],a      -> a
    (b:bs),cs -> b : append(bs, cs)

proc helloworld :: | Console =>  =
    | console => -> do
        on console do 
            hput ConsolePut 
            put "What is your name?" 
            hput ConsoleGet 
            get name 

            hput ConsolePut 
            put append("Hello ", name)

            hput ConsoleClose 
            halt 

proc run =
    | console => -> helloworld( |console=>)  -- create a process helloworld