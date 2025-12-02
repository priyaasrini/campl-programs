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

proc client :: [Char] | => Passer( | Echo), Terminal = 
    tag | => passer, term -> do
        on term do 
            hput StringTerminalPut 
            put tag
            hput StringTerminalGet 
            get input 
            
        on passer do hput Passer 

        split passer into echo, neg_newPasser
        on echo do
            hput EchoSend
            put append(tag, append(": ", input)) 
            get echoed 

        on term do 
            hput StringTerminalPut 
            put append("> ", echoed)
            
        plug 
            => neg_newPasser, new_Passer -> neg_newPasser |=| neg new_Passer
            new_Passer => echo, term ->  hcase new_Passer of 
                Passer -> fork new_Passer as 
                    new_ch with echo -> do new_ch |=| echo {- Sending echo back to the ticket manager -}
                    neg_new_new_passer with term -> plug 
                        neg_new_new_passer, new_new_passer => -> neg_new_new_passer |=| neg new_new_passer
                        client( tag | => new_new_passer, term)
                                

proc ticketManager ::  | Passer( | Echo), Passer (| Echo ) => Echo =
    | client1_ch, client2_ch => echo -> hcase client1_ch of 
        Passer -> fork client1_ch as 
            echo1 with echo -> echo1 |=| echo  {- Passing the echo server to Client 1 -}
            neg_newPasser with client2_ch -> plug
                neg_newPasser, newPasser => -> neg_newPasser |=| neg newPasser
                client2_ch => newPasser -> do 
                    hput Passer on newPasser 
                    split newPasser into new_echo, neg_new_new_passer
                    {- unnegate the new_new_passer -}
                    plug 
                        => neg_new_new_passer, new_new_passer -> neg_new_new_passer |=| neg new_new_passer
                        ticketManager( | client2_ch, new_new_passer => new_echo)

proc run :: | LogConsole => Terminal, Terminal =  
  | console => termA, termB -> plug
    client("A" | => passA, termA)
    client("B" | => passB, termB)
    ticketManager( | passA, passB => echo)
    server( | echo, console => )




