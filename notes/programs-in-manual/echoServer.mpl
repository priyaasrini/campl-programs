include Prelude 

fun isExit :: [Char] -> Bool = 
    [] -> False
    x:xs -> if x == 'q' then True else False

protocol Echo => S =
    EchoSend :: Put( [Char] | S) => S
    EchoClose :: TopBot => S

proc client :: | => Echo, StringTerminal =
    | => ch, term -> do
        on term do 
            hput StringTerminalPut 
            put "Enter any string (Enter q to exit)" 
            hput StringTerminalGet 
            get input 

        case isExit(input) of 
            True -> do 
                on term do 
                    hput StringTerminalClose
                    close 
                on ch do 
                    hput EchoClose
                    halt 
            False -> do 
                on ch do 
                    hput EchoSend 
                    put input 
                client( |  => ch, term )

proc server :: | Echo, Console => =
    | ch, console => -> do
        hcase ch of
            EchoSend -> do
                get input on ch 
                on console do
                    hput ConsolePut 
                    put ("I received " ++ input) 

                server( | ch, console => )                 
            EchoClose -> do
                on console do 
                    hput ConsolePut
                    put "Done"
                    hput ConsoleClose
                    close 
                halt ch

proc run = 
    | console => term -> plug 
        client( | => ch, term )
        server( | ch, console => )