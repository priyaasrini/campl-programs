include Prelude

proc client :: [Char] | => Put( [Char] | TopBot), StringTerminal =
    tag | => ch, term -> do
        on term do
            hput StringTerminalPut
            put tag ++ "Input a string"
            hput StringTerminalGet
            get message

        on ch do
            put tag ++ message
            close

        on term do 
            hput StringTerminalPut           
            put "Press Enter to close"
            hput StringTerminalGet
            get _ 
            hput StringTerminalClose
            halt 

proc receive_message_and_continue :: | Put([Char] | TopBot), Put([Char] | TopBot), Console => = 
    | winner, loser, console => -> do
        get message on winner
        hput ConsolePut on console
        put message on console
        close winner 

        get message on loser
        hput ConsolePut on console
        put message on console
        close loser  

        on console do
            hput ConsolePut 
            put "Done. Halting server."   
            hput ConsoleClose
            halt
                       
proc server :: | Put([Char] | TopBot), Put([Char] | TopBot), Console => =
    | ch1, ch2, console => -> race
        ch1 -> do
            receive_message_and_continue( | ch1, ch2, console => )
        ch2 -> do 
            receive_message_and_continue( | ch2, ch1, console => )

proc run :: | Console => StringTerminal, StringTerminal =
    | console => term1, term2 -> plug
        client("1:"| => ch1, term1 )
        client("2:"| => ch2, term2)
        server( |ch1, ch2, console => )