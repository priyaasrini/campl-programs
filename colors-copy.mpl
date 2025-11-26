protocol
    Colors => S =
        ColorPutRed :: Put( [Char] | R) => S
        ColorClose :: TopBot => S

    and

    RedFruits => R =
        RedFruit :: S => R

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S)
    ConsoleClose :: S => TopBot

proc client :: [Char] | => Colors =
    fruit | => colors -> do
        hput ColorPutRed on colors
        put fruit on colors

        hput RedFruit on colors
        hput ColorClose on colors
        halt colors

proc server :: | Colors, Console => =
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

proc run :: | Console => =
    | console => -> plug
        client( "apple" | => colors )
        server( | colors, console => )