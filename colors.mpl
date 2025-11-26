protocol
    Colors => S =
        ColorPutRed :: Put( [Char] | R) => S
        ColorClose :: TopBot => S

    and

    RedFruits => R =
        RedFruit :: S => R

protocol ReadConsole => S =
    StringTerminalGet :: Get( [Char] | S) => S
    StringTerminalClose :: TopBot => S

coprotocol S => LogConsole =
    ConsolePut :: S => Get( [Char] | S)
    ConsoleClose :: S => TopBot

proc client :: | => Colors, ReadConsole =
    | => colors, console -> do
        hput StringTerminalGet on console
        get fruit on console

        hput ColorPutRed on colors
        put fruit on colors

        hput RedFruit on colors
        hput ColorClose on colors

        hput StringTerminalClose on console
        close console

        halt colors

proc server :: | Colors, LogConsole => =
    | colors, console => -> do
        hcase colors of
            ColorPutRed -> do
                get fruit on colors

                hput ConsolePut on console
                put fruit on console

                hcase colors of
                    RedFruit -> server( | colors, console => )
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