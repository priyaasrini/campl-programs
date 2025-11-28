-- See colors-splitfork-Put.mpl for "race" working

protocol
    Echo => S =
        EchoSend :: Put( [Char] | R) => S
        EchoClose :: TopBot => S

    and

    EchoReceives => R =
        EchoReceive :: Get( [Char] | S) => R

protocol ReadConsole => S =
    StringTerminalGet :: Get( [Char] | S) => S
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

coprotocol S => LogConsole =
    ConsolePut :: S => Get( [Char] | S)
    ConsoleClose :: S => TopBot


fun append :: [A], [A] -> [A] =
    [], ts -> ts
    s:ss, ts -> s : append(ss,ts)

proc subclient :: [Char] | => Echo, ReadConsole =
    tag | => ch, console -> do
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
                put append(tag, fruit) on ch

                hput EchoReceive on ch
                get echoed on ch
                hput StringTerminalPut on console
                put append("> ", echoed) on console

                subclient(tag | => ch, console)

proc client :: | => Echo (*) Echo, ReadConsole, ReadConsole =
    | => _2_ch, console1, console2 -> do
        fork _2_ch as
            ch1 -> do subclient("1: " | => ch1, console1)
            ch2 -> do subclient("2: " | => ch2, console2)

proc server_loop_1 :: | Echo, LogConsole => =
    | ch, console => -> do
        hcase ch of
            EchoSend -> do
                get fruit on ch

                hput ConsolePut on console
                put fruit on console

                hcase ch of
                    EchoReceive -> do
                        put fruit on ch
                        server_loop_1( | ch, console => )
            EchoClose -> do
                hput ConsolePut on console
                put "Done" on console
                hput ConsoleClose on console
                close console
                halt ch

proc server_close_then_loop1 :: [Char] | TopBot, Echo, LogConsole => =
    log | topbot, ch, console => -> do
        hput ConsolePut on console
        put log on console
        close topbot
        server_loop_1( | ch, console => )

defn
    proc server_loop :: | Echo, Echo, LogConsole => =
        | ch1, ch2, console => -> do
            -- Version A: does not compile
            -- Because, race expects a Put type channel
            race
                ch1 -> hcase ch1 of
                    EchoSend -> server_echo( False | ch1, ch2, console =>)
                    EchoClose -> server_close_then_loop1("Done 1" | ch1, ch2, console =>)
                ch2 -> hcase ch2 of
                    EchoSend -> server_echo( True | ch2, ch1, console =>)
                    EchoClose -> server_close_then_loop1("Done 2" | ch2, ch1, console =>)
            -- Version B: compiles (no race), but it is not what we want
            {- hcase ch1 of
                EchoSend -> server_echo( False | ch1, ch2, console =>)
                EchoClose -> server_close_then_loop1("Done 1" | ch1, ch2, console =>)
            -}

    proc server_echo :: Bool | Put([Char] | EchoReceives), Echo, LogConsole => =
        swtch | fruits, ch, console => -> do
            get fruit on fruits

            hput ConsolePut on console
            -- TODO: Use swtch
            put fruit on console

            hcase fruits of
                EchoReceive -> do
                    put fruit on fruits
                    case swtch of
                        False -> server_loop( | fruits, ch, console => )
                        True -> server_loop( | ch, fruits, console => )

proc server :: | Echo (*) Echo, LogConsole => =
    | _2_ch, console => -> do
        split _2_ch into ch1, ch2
        server_loop( | ch1, ch2, console => )

proc run :: | LogConsole => ReadConsole, ReadConsole =
    | console_s => console_c1, console_c2 -> plug
        client( | => _2_ch, console_c1, console_c2 )
        server( | _2_ch, console_s => )