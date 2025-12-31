coprotocol S => Console = 
        ConsoleGet :: S => Put( [Char] | S)
        ConsolePut :: S => Get( [Char] | S)
        ConsoleClose :: S => TopBot 

{- codata S -> InfList(A) = 
        Head :: S -> A 
        Tail :: S -> S

fun nats :: Int -> InfList(Int) = 
        n -> (Head := -> n, Tail := -> nats(n+1)) -}

proc helloworld :: | Console => = 
    | console => -> do
        hput ConsoleGet on console
        get x on console

        hput ConsolePut on console 
        put x on console

        hput ConsoleClose on console
        halt console

proc run = 
    | console => -> helloworld( |console =>)