{- Echo Server using coprotocol with terminals -}

-- coProtocol connecting the client and the server
coprotocol
    S => Echo =
        EchoSend :: S => Get( [Char] | Put( [Char] | S))
        EchoClose :: S => TopBot

protocol ReadConsole => S =
    StringTerminalGet :: Get( [Char] | S) => S
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

coprotocol S => LogConsole =
    ConsolePut :: S => Get( [Char] | S)
    ConsoleClose :: S => TopBot

{-
    1. Client reads data from the terminals
    2. Sends to server
    3. Waits for server to echo it back
    4. Once it hears the echo, prints on the console. 

    Client closes channel, sends close handle to server and halts on receiving an empty string.
-}
proc client :: | Echo => ReadConsole =
    | ch => console -> do
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
                put fruit on ch
                get echoed on ch
                hput StringTerminalPut on console
                put ('>':echoed) on console

                client( | ch => console)

{-  Loop: 
    1. Server waits to hear data from client
    2. Prints and echoes back the data to the client
     
    Server closes channel and halts when it receives close handle. 
-}
proc server :: | LogConsole => Echo =
    | console => ch -> do
        hcase ch of
            EchoSend -> do
                get fruit on ch

                hput ConsolePut on console
                put fruit on console

                put fruit on ch
                server( | console => ch)

            EchoClose -> do
                hput ConsolePut on console
                put "Done" on console
                hput ConsoleClose on console
                close console
                halt ch

proc run :: | LogConsole => ReadConsole =
    | console_s => console_c -> plug
        client( | ch => console_c )
        server( | console_s => ch )