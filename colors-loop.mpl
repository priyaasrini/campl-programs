-- Simple client-server communication where the client loops until it encounters an empty string. 

-- Connects Client and Server
protocol
    Echo => S =
        EchoSend :: Put( [Char] | S) => S
        EchoClose :: TopBot => S

protocol ReadConsole => S =
    StringTerminalGet :: Get( [Char] | S) => S
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
                -- Close channels and halt -- 
                hput EchoClose on ch
                hput StringTerminalClose on console
                close console
                halt ch

            _:_ -> do
                -- Send handle and data to server
                hput EchoSend on ch
                put fruit on ch

                -- loop -- 
                client( | => ch, console) 

proc server :: | Echo, LogConsole => =
    | ch, console => -> do
        hcase ch of
            EchoSend -> do
                -- receive data from client -- 
                get fruit on ch

                hput ConsolePut on console
                put fruit on console

                server( | ch, console => )
            EchoClose -> do
                -- close channels and halt -- 
                hput ConsolePut on console
                put "Done" on console
                hput ConsoleClose on console
                close console
                halt ch

proc run :: | LogConsole => ReadConsole =
    | console_s => console_c -> plug
        client( | => ch, console_c )
        server( | ch, console_s => )