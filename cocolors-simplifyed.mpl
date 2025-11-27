coprotocol S => Echo =
    EchoSend :: S => Get( [Char] | Put( [Char] | S))
    EchoClose :: S => TopBot

proc client :: | Echo => =
    | ch => -> do
        hput EchoSend on ch
        put "hello" on ch
        get echoed on ch
        hput EchoClose on ch
        halt ch

proc server :: | => Echo =
    | => ch -> hcase ch of
        EchoSend -> do
            get val on ch
            put val on ch
            server( | => ch )
        EchoClose -> halt ch

proc run :: | => =
    | => -> plug
        client( | ch => )
        server( | => ch )