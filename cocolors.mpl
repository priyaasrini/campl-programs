coprotocol
    S => Colors =
        ColorPutRed :: S => Get( [Char] | R)
        ColorClose :: S => TopBot

    and

    R => RedFruits =
        RedFruit :: R => Put( [Char] | S)

protocol ReadConsole => S =
    StringTerminalGet :: Get( [Char] | S) => S
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

coprotocol S => LogConsole =
    ConsolePut :: S => Get( [Char] | S)
    ConsoleClose :: S => TopBot

proc client :: | Colors => ReadConsole =
    | colors => console -> do
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

proc server :: | LogConsole => Colors =
    | console => colors -> do
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
        client( | colors => console_c )
        server( | console_s => colors )