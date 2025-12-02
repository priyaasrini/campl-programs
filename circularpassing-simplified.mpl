{- Pass echo server around a circle. 
   The echo server is passed from left to right (during which it interacts with each client.) 
   When it reaches the rightmost end, it is passed right to left.
   Input hardcoded - no terminals.
-}

protocol Passer( | M) => S =
    Passer :: M (+) Neg(S) => S

protocol Echo => S =
    EchoSend :: Put( [Char] | Get( [Char] | S)) => S
    EchoClose :: TopBot => S

coprotocol S => LogConsole =
    ConsolePut :: S => Get( [Char] | S)
    ConsoleClose :: S => TopBot

fun append :: [A], [A] -> [A] =
    [], ts -> ts
    s:ss, ts -> s : append(ss,ts)

fun echo_from_left :: [Char] -> [Char] =
    tag -> append(" >> ", append(tag, " >> "))
fun echo_from_right :: [Char] -> [Char] =
    tag -> append(" << ", append(tag, " << "))

defn
    proc circular_corner_left :: [Char] | Passer( | Echo) => Echo =
        tag | pass => ch -> do
            on ch do
                hput EchoSend
                put echo_from_left(tag)
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
                        put echo_from_right(tag)
                        get _
                    circular_corner_left( tag | new_pass => ch )

proc circular_client_send_then_close :: [Char] | Passer(| Echo) => Neg(Passer(| Echo)), Echo =
    tag | send => neg_recv, ch -> plug
        send, dummy => neg_recv, ch -> do
            close dummy
            hcase send of
                Passer -> do
                    fork send as
                        ch2 with ch -> do
                            on ch do
                                hput EchoSend
                                put echo_from_left('*' : tag)
                                get echoed
                            ch2 |=| ch 
                        neg_new_send with neg_recv ->
                            -- Version(A): |=| negated channels (does not work at runtime)
                            -- for this program, closing a terminal leads to deadlock
                            neg_new_send |=| neg_recv
                            -- Version(B): we need to unnegate both channels (neg_new_send and neg_recv), before |=| them
                            {- plug 
                                => neg_recv, recv -> neg_recv |=| neg recv
                                neg_new_send, new_send => -> neg_new_send |=| neg new_send 
                                recv => new_send -> new_send |=| recv
                            -}
        => dummy -> halt dummy

defn
    proc circular_client_wait :: [Char], [[Char]] | Passer(| Echo) => Passer(| Echo) =
        tag, reqs | send => recv -> do
            on recv do hput Passer
            split recv into ch, neg_new_recv
            circular_client_go(tag, reqs | send => neg_new_recv, ch )

    proc circular_client_go :: [Char], [[Char]] | Passer(| Echo) => Neg(Passer(| Echo)), Echo =
        tag, reqs | send => neg_recv, ch -> case reqs of
            [] -> circular_client_send_then_close( tag | send => neg_recv, ch )
            req:reqs_left -> do
                on ch do
                    hput EchoSend
                    put append(tag, append(": ", req))
                    get _

                circular_client_pass( tag, reqs_left | send => neg_recv, ch )

    proc circular_client_pass :: [Char], [[Char]] | Passer(| Echo) => Neg(Passer(| Echo)), Echo =
        tag, reqs | send => neg_recv, ch -> do
            on ch do
                hput EchoSend
                put echo_from_left(tag)
                get echoed

            hcase send of
                Passer -> fork send as
                    ch2 with ch -> ch2 |=| ch
                    neg_new_send with neg_recv -> circular_client_passback( tag, reqs | neg_new_send => neg_recv )

    proc circular_client_passback :: [Char], [[Char]] | Neg(Passer(| Echo)) => Neg(Passer(| Echo)) =
        tag, reqs | neg_send => neg_recv  -> plug
            neg_send, send => -> neg_send |=| neg send
            => neg_recv, recv -> neg_recv |=| neg recv
            recv => send -> do
                on send do hput Passer
                split send into ch2, neg_new_send

                on ch2 do
                    hput EchoSend
                    put echo_from_right(tag)
                    get echoed

                hcase recv of
                    Passer -> fork recv as
                        ch3 with ch2 -> ch3 |=| ch2
                        neg_new_recv with neg_new_send -> plug
                            => neg_new_send, new_send -> neg_new_send |=| neg new_send
                            neg_new_recv, new_recv => -> neg_new_recv |=| neg new_recv
                            circular_client_wait( tag, reqs | new_send => new_recv )

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

proc run :: | LogConsole => =
    | console => -> plug
        circular_corner_left( "l" | pass0 => ch )
        circular_client_wait( "A", ["apple", "a bee"] | pass1 => pass0 )
        circular_client_wait( "B", ["banana"] | pass2 => pass1 )
        circular_corner_right( "r" | => pass2 )
        server( | ch, console => )
