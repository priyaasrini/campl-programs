protocol Passer( | M) => S =
    Passer :: M (+) Neg(S) => S

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

defn
    proc circular_corner_left :: [Char] | Passer( | Echo) => Echo =
        tag | pass => ch -> do
            on ch do
                hput EchoSend
                put append("| ", tag)
                get _
            hcase pass of
                Passer -> do
                    fork pass as
                        ch2 with ch -> ch2 |=| ch
                        neg_new_pass -> plug
                            neg_new_pass, new_pass => -> neg_new_pass |=| neg new_pass
                            circular_corner_right( tag | => new_pass )

    proc circular_corner_right :: [Char] | => Passer( | Echo) =
        tag | => pass -> do
            on pass do hput Passer
            split pass into ch, neg_new_pass
            plug
                => neg_new_pass, new_pass -> neg_new_pass |=| neg new_pass
                new_pass => ch -> do
                    on ch do
                        hput EchoSend
                        put append(tag, " |")
                        get _
                    circular_corner_left( tag | new_pass => ch )

proc circular_client_send_then_close :: [Char] | Passer(| Echo) => Neg(Passer(| Echo)), Echo, Terminal =
    tag | send => neg_recv, ch, term -> plug
        send, dummy => neg_recv, ch -> do
            close dummy
            hcase send of
                Passer -> do
                    fork send as
                        ch2 with ch -> ch2 |=| ch
                        neg_new_send with neg_recv -> neg_new_send |=| neg_recv

        => dummy, term -> do
            close dummy
            on term do
                hput StringTerminalPut
                put append(tag, " Closing Everything")

                hput StringTerminalGet
                get _

                hput StringTerminalClose
            halt term

defn
    proc circular_client_wait :: [Char] | Passer(| Echo) => Passer(| Echo), Terminal =
        tag | send => recv, term -> do
            on term do
                hput StringTerminalPut
                put "Waiting for channel . . ."

            on recv do hput Passer
            split recv into ch, neg_new_recv
            circular_client_go(tag | send => neg_new_recv, ch, term )

    proc circular_client_go :: [Char] | Passer(| Echo) => Neg(Passer(| Echo)), Echo, Terminal =
        tag | send => neg_recv, ch, term -> do
            on term do
                hput StringTerminalPut
                put tag

                hput StringTerminalGet
                get input

            case input of
                [] -> circular_client_send_then_close( tag | send => neg_recv, ch, term )
                _:_ -> do
                    on ch do
                        hput EchoSend
                        put append(tag, append(": ", input))
                        get echoed

                    on term do
                        hput StringTerminalPut
                        put append(tag, append(" > ", echoed))

                    circular_client_pass( tag | send => neg_recv, ch, term )

    proc circular_client_pass :: [Char] | Passer(| Echo) => Neg(Passer(| Echo)), Echo, Terminal =
        tag | send => neg_recv, ch, term -> do
            on term do
                hput StringTerminalPut
                put "Passing channel around to the >> right >>"

            hcase send of
                Passer -> fork send as
                    ch2 with ch -> ch2 |=| ch
                    neg_new_send with neg_recv, term -> circular_client_passback( tag | neg_new_send => neg_recv, term)
                    

    proc circular_client_passback :: [Char] | Neg(Passer(| Echo)) => Neg(Passer(| Echo)), Terminal =
        tag | neg_send => neg_recv, term -> plug
            neg_send, send => -> neg_send |=| neg send
            => neg_recv, recv -> neg_recv |=| neg recv
            recv => send, term -> do
                on term do
                    hput StringTerminalPut
                    put "Passing channel around to the << left  <<"

                on send do hput Passer
                split send into ch2, neg_new_send
                hcase recv of
                    Passer -> fork recv as
                        ch3 with ch2 -> ch3 |=| ch2
                        neg_new_recv with neg_new_send, term -> plug
                            => neg_new_send, new_send -> neg_new_send |=| neg new_send
                            neg_new_recv, new_recv => -> neg_new_recv |=| neg new_recv
                            circular_client_wait(tag | new_send => new_recv, term)

proc server :: | Echo, LogConsole => =
    | ch, console => -> do
        hcase ch of
            EchoSend -> do
                get fruit on ch

                hput ConsolePut on console
                put fruit on console

                put fruit on ch
                server( | ch, console => )
            EchoClose -> do
                hput ConsolePut on console
                put "Done" on console
                hput ConsoleClose on console
                close console
                halt ch

proc run :: | LogConsole => Terminal, Terminal, Terminal =
    | console => term1, term2, term3 -> plug
        circular_corner_left( "<left>" | pass0 => ch )
        circular_client_wait( "A" | pass1 => pass0, term1 )
        circular_client_wait( "B" | pass2 => pass1, term2 )
        -- circular_client_wait( "C" | pass3 => pass2, term3 )
        -- circular_corner_right( "<right>" | => pass3 )
        circular_corner_right( "<right>" | => pass2 )
        server( | ch, console => )
