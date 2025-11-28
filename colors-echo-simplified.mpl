-- A simple echo server. 
-- The Client relays data to server. The server echoes it back to the client. 

protocol Echo => S =
    EchoSend :: Put( [Char] | Get( [Char] | S)) => S
    EchoClose :: TopBot => S

protocol ReadConsole => S =
    StringTerminalGet :: Get( [Char] | S) => S
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

coprotocol S => LogConsole =
    ConsolePut :: S => Get( [Char] | S)
    ConsoleClose :: S => TopBot

proc client :: | => Echo, ReadConsole =
    | => ch, console -> do
        hput StringTerminalGet on console
        get fruit on console

        case fruit of
            [] -> do
                hput EchoClose on ch
                hput StringTerminalClose on console
                close console
                halt ch

            _:_ -> do
                hput EchoSend on ch
                -- send data
                put fruit on ch 
                -- receive echoed data
                get echoed on ch  
                -- print received data
                hput StringTerminalPut on console
                put ('>':' ':echoed) on console

                -- loop
                client( | => ch, console)

proc server :: | Echo, LogConsole => =
    | ch, console => -> do
        hcase ch of
            EchoSend -> do
                -- receive data
                get fruit on ch

                -- print data
                hput ConsolePut on console
                put fruit on console

                -- echo (send) data 
                put fruit on ch
                server( | ch, console => )
            EchoClose -> do
                hput ConsolePut on console
                put "Done" on console
                hput ConsoleClose on console
                close console
                halt ch

proc run :: | LogConsole => ReadConsole =
    | console_s => console_c -> plug
        client( | => ch, console_c )
        server( | ch, console_s => )