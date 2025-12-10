proc equate_2 :: | M, P => M, P =
    | in1, in2 => out1, out2 -> plug 
        in1 => dummy, out1 -> do
            on dummy do close
            in1 |=| out1
        in2, dummy => out2 -> do
            on dummy do close
            in2 |=| out2

-- we cannot race on handles, if we could then replace Put(() | Passer( | Echo)) with Passer( | Echo)
protocol Passer( | M) => S =
    Passer :: M (+) Neg(Put(() | S)) => S
    PasserClose :: TopBot => S

protocol Echo => S =
    EchoSend :: Put( [Char] | Get( [Char] | S)) => S
    EchoClose :: TopBot => S

protocol PList( | M) => S =
    PListEmpty :: TopBot => S
    PListCons :: M (*) S => S

proc plist_cons :: | M, PList( | M) => PList( | M) =
    | ch, chs => ret_chs -> do
        on ret_chs do hput PListCons
        fork ret_chs as
            ret_ch with ch -> ret_ch |=| ch
            ret_list with chs -> ret_list |=| chs

-- because we only can race on Put: M = Put(A | P)
proc p_listrace :: | Put(A | P), PList( | Put(A | P)) => Put(A | P) (*) PList( | Put(A | P)) =
    | ch1, chs => ret_winner_and_losers -> hcase chs of
        PListEmpty -> do
            on chs do close
            fork ret_winner_and_losers as
                ret_winner with ch1 -> ret_winner |=| ch1
                ret_losers -> on ret_losers do
                    hput PListEmpty
                    halt

        PListCons -> do
            on chs do split into ch2,new_chs
            plug
                p_listrace( | ch2, new_chs => rec_winner_and_losers )
                -- racer
                ch1, rec_winner_and_losers => ret_winner_and_losers -> do
                    on rec_winner_and_losers do split into rec_winner, rec_losers 
                    race
                        rec_winner -> fork ret_winner_and_losers as
                            ret_winner with rec_winner -> rec_winner |=| ret_winner
                            ret_losers with ch1, rec_losers -> plist_cons( | ch1, rec_losers => ret_losers )
                        ch1 -> fork ret_winner_and_losers as
                            ret_winner with ch1 -> ch1 |=| ret_winner
                            ret_losers with rec_winner, rec_losers -> plist_cons( | rec_winner, rec_losers => ret_losers )

protocol Terminal => S =
    StringTerminalGet :: Get( [Char] | S) => S
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S)
    ConsoleStringTerminal :: S => S (*) Neg(Terminal)
    ConsoleClose :: S => TopBot

fun append :: [A], [A] -> [A] =
    [], ts -> ts
    s:ss, ts -> s : append(ss,ts)

proc client :: [Char] | => Put( () | Passer( | Echo)), Terminal = 
    tag | => passer, term -> do
        on term do 
            hput StringTerminalPut 
            put tag
            hput StringTerminalGet 
            get input 

        case input of 
            [] -> do 
                on passer do
                    put ()
                    hput PasserClose 
                on term do 
                    hput StringTerminalPut
                    put "Closing everything..."
                    hput StringTerminalGet 
                    get _ 
                    hput StringTerminalClose 
                    close  
                halt passer

            _:_ -> do 
                on passer do
                    put ()
                    hput Passer

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
                    new_Passer => echo, term -> do
                        on new_Passer do get _
                        hcase new_Passer of 
                            Passer -> fork new_Passer as 
                                new_ch with echo -> do new_ch |=| echo {- Sending echo back to the ticket manager -}
                                neg_new_new_passer with term -> plug 
                                    neg_new_new_passer, new_new_passer => -> neg_new_new_passer |=| neg new_new_passer
                                    client( tag | => new_new_passer, term)

proc server :: | Echo, Console => =
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

proc ticketManager :: | PList( | Put( () | Passer( | Echo))) => Echo =
    | pass_list => echo -> hcase pass_list of
        PListEmpty -> do
            close pass_list
            on echo do
                hput EchoClose
                halt

        PListCons -> do
            on pass_list do split into pass_head, pass_tail
            plug
                p_listrace( | pass_head, pass_tail => pass_winner_and_losers )
                pass_winner_and_losers => echo -> do
                    on pass_winner_and_losers do split into pass, pass_list
                    on pass do get _
                    hcase pass of
                        Passer -> fork pass as
                            echo1 with echo -> echo1 |=| echo
                            neg_newPasser with pass_list -> plug
                                neg_newPasser, newPasser => -> neg_newPasser |=| neg newPasser
                                pass_list => newPasser -> do
                                    on newPasser do
                                        put ()
                                        hput Passer
                                    split newPasser into new_echo, neg_new_new_passer
                                    plug
                                        => neg_new_new_passer, new_new_passer -> neg_new_new_passer |=| neg new_new_passer
                                        new_new_passer, pass_list => new_echo -> plug
                                            plist_cons( | new_new_passer, pass_list => new_pass_list )
                                            ticketManager( | new_pass_list => new_echo )
                        PasserClose -> do
                            on pass do close
                            ticketManager( | pass_list => echo )

proc main :: [[Char]] | Echo, Console => PList( | Put(() | Passer( | Echo))) =
  tags | echo, console => pass_list -> case tags of
    [] -> do
        on pass_list do
            hput PListEmpty
            close
        server( | echo, console => )
    tag:tags1 -> do
        on console do
            hput ConsoleStringTerminal
            split into new_console, neg_term
        on pass_list do hput PListCons
        fork pass_list as
            pass with neg_term -> plug
                neg_term, term => -> neg_term |=| neg term
                client( tag | => pass, term )
            new_pass_list with echo, new_console -> main( tags1 | echo, new_console => new_pass_list )

proc run :: | Console => =
    | console => -> plug
        ticketManager( | pass_list => echo )
        main ( ["A", "B", "C"] | echo, console => pass_list )