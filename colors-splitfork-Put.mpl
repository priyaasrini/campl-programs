protocol
    Colors => S =
        ColorPutRed :: Put( [Char] | R) => S
        ColorClose :: TopBot => S

    and

    RedFruits => R =
        RedFruit :: Get( [Char] | S) => R

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

proc subclient :: [Char] | => Colors, ReadConsole =
    tag | => colors, console -> do
        hput StringTerminalGet on console
        get fruit on console

        case fruit of
            [] -> do
                hput ColorClose on colors
                hput StringTerminalClose on console
                close console
                halt colors

            _:_ -> do
                hput ColorPutRed on colors
                put append(tag, fruit) on colors

                hput RedFruit on colors
                get echoed on colors
                hput StringTerminalPut on console
                put append("> ", echoed) on console

                subclient(tag | => colors, console)

proc client :: | => Colors (*) Colors, ReadConsole, ReadConsole =
    | => _2_colors, console1, console2 -> do
        fork _2_colors as
            colors1 -> do subclient("1: " | => colors1, console1)
            colors2 -> do subclient("2: " | => colors2, console2)

proc server_loop_1 :: | Put([Char] | RedFruits), LogConsole => =
    | colors, console => -> do
        get fruit on colors

        hput ConsolePut on console
        put fruit on console

        hcase colors of
            RedFruit -> do
                put fruit on colors
                hcase colors of 
                    ColorPutRed -> server_loop_1( | colors, console => )
                    ColorClose -> do
                        hput ConsolePut on console
                        put "Done" on console
                        hput ConsoleClose on console
                        close console
                        halt colors

proc server_close_then_loop1 :: [Char] | TopBot, Put([Char] | RedFruits), LogConsole => =
    log | topbot, colors, console => -> do
        hput ConsolePut on console
        put log on console
        close topbot
        server_loop_1( | colors, console => )

defn
    proc server_loop :: | Put([Char] | RedFruits), Put([Char] | RedFruits), LogConsole => =
        | colors1, colors2, console => -> race
            colors1 -> server_echo( False | colors1, colors2, console =>)
            colors2 -> server_echo( True  | colors2, colors1, console =>)

    proc server_echo :: Bool | Put([Char] | RedFruits), Put([Char] | RedFruits), LogConsole => =
        swtch | winner, loser, console => -> do
            get fruit on winner

            hput ConsolePut on console
            -- TODO: Use swtch
            put fruit on console

            hcase winner of
                RedFruit -> do
                    put fruit on winner
                    hcase winner of
                        ColorPutRed -> case swtch of
                            False -> server_loop( | winner, loser, console => )
                            True -> server_loop( | loser, winner, console => )
                        ColorClose -> server_close_then_loop1( | winner, loser, console => )

proc server :: | Colors (*) Colors, LogConsole => =
    | _2_colors, console => -> do
        split _2_colors into colors1, colors2
        hcase colors1 of
            ColorPutRed -> hcase colors2 of
                ColorPutRed -> server_loop(| colors1, colors2, console =>)
                ColorClose -> server_close_then_loop1("Done 2" | colors2, colors1, console =>)
            ColorClose -> server_close_then_loop1("Done 1" | colors1, colors2, console =>)

proc run :: | LogConsole => ReadConsole, ReadConsole =
    | console_s => console_c1, console_c2 -> plug
        client( | => _2_colors, console_c1, console_c2 )
        server( | _2_colors, console_s => )