include Prelude

proc subclient :: [Char] | => Put([Char] | Get([Char] | TopBot)), StringTerminal =
    tag | => ch, term -> do
        hput StringTerminalPut on term
        put tag on term
        hput StringTerminalGet on term
        get fruit on term                        -- Reading input from the user

        put fruit on ch                          -- send message to server
        get echoed on ch                         -- receive message from server 
        hput StringTerminalPut on term           
        put "Server echo ~> " ++ echoed on term  -- print message on the terminal 

        hput StringTerminalPut on term           
        put "Press Enter to close" on term
        hput StringTerminalGet on term
        get _ on term 

        hput StringTerminalClose on term        -- close terminal  
        close term
        halt ch


proc client :: | => Put([Char] | Get([Char] | TopBot)) (*) Put([Char] | Get([Char] | TopBot)), StringTerminal, StringTerminal =
    | => _2_ch, term1, term2 -> fork _2_ch as
        ch1 -> subclient("Client 1: " | => ch1, term1)
        ch2 -> subclient("Client 2: " | => ch2, term2)

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
    | ch1, ch2, console => -> do 
        split _2_ch into ch1, ch2
        race
            ch1 -> do
                receive_message_and_continue( | ch1, ch2, console => )
            ch2 -> do 
                receive_message_and_continue( | ch2, ch1, console => )

proc run :: | Console => StringTerminal, StringTerminal =
    | console => term1, term2 -> plug
        client( | => _2_ch, term1, term2 )
        server( | _2_ch, console => )