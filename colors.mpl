-- Simple client-server communication: Client reads from terminal once. 

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

-- Client reads data from StringTerminal, sends it to server, sends a close handle, closes channels and halts. 
proc client :: | => Echo, ReadConsole =
    | => ch, console -> do
        hput StringTerminalGet on console
        get fruit on console

        hput EchoSend on ch
        put fruit on ch

        hput EchoClose on ch

        hput StringTerminalClose on console
        close console

        halt ch

-- Server receives data from server and prints on console. On receiving a  close handle, closes channels and halts. 
proc server :: | Echo, LogConsole => =
    | ch, console => -> do
        hcase ch of
            EchoSend -> do
                get fruit on ch

                hput ConsolePut on console
                put fruit on console

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