include Prelude 

fun isExit :: [Char] -> Bool = 
    [] -> False
    x:xs -> if x == 'q' then True else False

proc getSomethingUntil :: | Console =>  = 
    | console => -> do 
        on console do
            hput ConsolePut 
            put "Enter any string (Enter q to exit)" 

            hput ConsoleGet 
            get input

        case isExit(input) of 
            True -> do 
                on console do 
                    hput ConsolePut 
                    put "Exiting..." 
                    hput ConsoleClose
                    halt  
            False -> do 
                on console do 
                    hput ConsolePut 
                    put ("I received " ++ input) 
                getSomethingUntil( | console => )

proc run = 
    | console =>  -> getSomethingUntil( | console => )