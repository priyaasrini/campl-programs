{- Echo server: Client forks into two. Each client races to send data to the server. 
   Note that the race is on the data (so Put) and not on handles. 
   The race begins only when both the handles are received. -}

protocol Echo => S =
    EchoSend :: Put( [Char] | Get( [Char] | S)) => S
    EchoClose :: TopBot => S

protocol Terminal => S =
    StringTerminalGet :: Get( [Char] | S) => S
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

coprotocol S => LogConsole =
    ConsolePut :: S => Get( [Char] | S)
    ConsoleClose :: S => TopBot

fun append :: [A], [A] -> [A] =
    [], ts -> ts
    s:ss, ts -> s : append(ss,ts)

proc subclient :: [Char] | => Echo, Terminal =
    tag | => ch, term -> do
        hput StringTerminalPut on term
        put tag on term

        hput EchoSend on ch

        hput StringTerminalGet on term
        get fruit on term

        put append(tag, fruit) on ch

        get echoed on ch
        hput StringTerminalPut on term
        put append("~> ", echoed) on term

        hput EchoClose on ch
        hput StringTerminalClose on term
        close term
        halt ch

proc client :: | => Echo (*) Echo, Terminal, Terminal =
    | => _2_ch, term1, term2 -> fork _2_ch as
        ch1 -> subclient("1: " | => ch1, term1)
        ch2 -> subclient("2: " | => ch2, term2)

-- Looping on one client; another client has halted.
proc server_loop_1 :: [Char] | Put([Char] | Get([Char] | Echo)), LogConsole => =
    tag | ch, console => -> do
        get fruit on ch

        hput ConsolePut on console
        put append(tag, append(": ", fruit)) on console

        put fruit on ch

        hcase ch of 
            EchoSend -> server_loop_1( tag | ch, console => )
            EchoClose -> do
                hput ConsolePut on console
                put append("Done ", tag) on console

                hput ConsoleClose on console
                close console
                halt ch

-- Closing channels and halting server when the last client halts. 
proc server_close_then_loop1 :: [Char], [Char] | TopBot, Put([Char] | Get([Char] | Echo)), LogConsole => =
    topbot_tag, tag | topbot, ch, console => -> do
        hput ConsolePut on console
        put append("Done ", topbot_tag) on console

        close topbot
        server_loop_1( tag | ch, console => )

-- Set up a race to receive data from either of the clients. 
proc server_loop :: | Put([Char] | Get([Char] | Echo)), Put([Char] | Get([Char] | Echo)), LogConsole => =
    | ch1, ch2, console => -> race
        ch1 -> do
            get fruit on ch1

            hput ConsolePut on console
            put append("1: ", fruit) on console

            put fruit on ch1
            hcase ch1 of
                EchoSend -> server_loop( | ch1, ch2, console => )
                EchoClose -> server_close_then_loop1( "1", "2" | ch1, ch2, console => )
        ch2 -> do
            get fruit on ch2

            hput ConsolePut on console
            put append("2: ", fruit) on console

            put fruit on ch2
            hcase ch2 of
                EchoSend -> server_loop( | ch1, ch2, console => )
                EchoClose -> server_close_then_loop1( "2", "1", | ch2, ch1, console => )

proc server :: | Echo (*) Echo, LogConsole => =
    | _2_ch, console => -> do
        split _2_ch into ch1, ch2
        hcase ch1 of
            EchoSend -> hcase ch2 of
                EchoSend -> server_loop( | ch1, ch2, console => )
                EchoClose -> server_close_then_loop1( "2", "1" | ch2, ch1, console => )
            EchoClose -> hcase ch2 of
                EchoSend -> server_close_then_loop1( "1", "2", | ch1, ch2, console => )
                EchoClose -> do
                    hput ConsolePut on console
                    put "Done 1&2" on console

                    hput ConsoleClose on console
                    close console

                    close ch1
                    halt ch2

proc run :: | LogConsole => Terminal, Terminal =
    | console => term1, term2 -> plug
        client( | => _2_ch, term1, term2 )
        server( | _2_ch, console => )