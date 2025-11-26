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

proc client :: | => Colors, ReadConsole =
    | => colors, console -> do
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
                put fruit on colors

                hput RedFruit on colors
                get echoed on colors
                hput StringTerminalPut on console
                put ('>':echoed) on console

                client( | => colors, console)

proc server :: | Colors, LogConsole => =
    | colors, console => -> do
        hcase colors of
            ColorPutRed -> do
                get fruit on colors

                hput ConsolePut on console
                put fruit on console

                hcase colors of
                    RedFruit -> do
                        put fruit on colors
                        server( | colors, console => )
            ColorClose -> do
                hput ConsolePut on console
                put "Done" on console
                hput ConsoleClose on console
                close console
                halt colors

proc run :: | LogConsole => ReadConsole =
    | console_s => console_c -> plug
        client( | => colors, console_c )
        server( | colors, console_s => )