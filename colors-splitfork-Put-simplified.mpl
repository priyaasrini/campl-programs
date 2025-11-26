protocol Echo => S =
    EchoSend :: Put( [Char] | Get( [Char] | S)) => S
    EchoClose :: TopBot => S

fun append :: [A], [A] -> [A] =
    [], ts -> ts
    s:ss, ts -> s : append(ss,ts)

proc subclient :: [Char] | => Echo =
    tag | => ch -> do
        hput EchoSend on ch
        put append(tag, "fruit") on ch
        get echoed on ch

        hput EchoClose on ch
        halt ch

proc client :: | => Echo (*) Echo =
    | => _2_ch -> fork _2_ch as
        ch1 -> subclient("1: " | => ch1)
        ch2 -> subclient("2: " | => ch2)

proc server_loop_1 :: | Put([Char] | Get([Char] | Echo)) => =
    | ch => -> do
        get fruit on ch
        put fruit on ch

        hcase ch of 
            EchoSend -> server_loop_1( | ch => )
            EchoClose -> halt ch

proc server_close_then_loop1 :: | TopBot, Put([Char] | Get([Char] | Echo)) => =
    | topbot, ch => -> do
        close topbot
        server_loop_1( | ch => )

proc server_loop :: | Put([Char] | Get([Char] | Echo)), Put([Char] | Get([Char] | Echo)) => =
    | ch1, ch2 => -> race
        ch1 -> do
            get fruit on ch1
            put fruit on ch1
            hcase ch1 of
                EchoSend -> server_loop( | ch1, ch2 => )
                EchoClose -> server_close_then_loop1( | ch1, ch2 => )
        ch2 -> do
            get fruit on ch2
            put fruit on ch2
            hcase ch2 of
                EchoSend -> server_loop( | ch1, ch2 => )
                EchoClose -> server_close_then_loop1( | ch2, ch1 => )

proc server :: | Echo (*) Echo => =
    | _2_ch => -> do
        split _2_ch into ch1, ch2
        hcase ch1 of
            EchoSend -> hcase ch2 of
                EchoSend -> server_loop(| ch1, ch2 =>)
                EchoClose -> server_close_then_loop1( | ch2, ch1 =>)
            EchoClose -> hcase ch2 of
                EchoSend -> server_close_then_loop1( | ch1, ch2 =>)
                EchoClose -> do
                    close ch1
                    halt ch2

proc run :: | => =
    | => -> plug
        client( | => _2_ch )
        server( | _2_ch => )