protocol Echo => S =
        EchoSend :: S => S
        EchoClose :: TopBot => S

fun append :: [A], [A] -> [A] =
    [], ts -> ts
    s:ss, ts -> s : append(ss,ts)

proc subclient :: | => Echo =
    | => ch -> do
        hput EchoSend on ch
        hput EchoClose on ch
        halt ch

proc client :: | => Echo (*) Echo =
    | => _2_ch -> fork _2_ch as
        ch1 -> subclient(| => ch1)
        ch2 -> subclient(| => ch2)

proc server_loop_1 :: | Echo => =
    | ch => -> hcase ch of
        EchoSend -> do
            server_loop_1( | ch => )
        EchoClose -> do
            halt ch

proc server_loop :: | Echo, Echo => =
    | ch1, ch2 => -> do
        -- Version A: does not compile
        race
            ch1 -> server_loop(| ch1, ch2 => )
            ch2 -> server_loop(| ch1, ch2 => )
        -- Version B: compiles, but it is not what we want
        {-
        server_loop(| ch1, ch2 => )
        -}

proc server :: | Echo (*) Echo => =
    | _2_ch => -> do
        split _2_ch into ch1, ch2
        server_loop( | ch1, ch2 => )

proc run :: | => =
    | => -> plug
        client( | => _2_ch )
        server( | _2_ch => )