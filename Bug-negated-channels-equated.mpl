{- 
    Try emulating bug but no bug.
-}

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

proc client ::  | => Echo = 
    | => ch -> on ch do
        hput EchoSend 
        put "Apple"
        get echoed 

        hput EchoClose
        halt

proc neg_client :: | Neg(Echo) => = 
    | neg_ch =>  -> plug 
      ch, neg_ch => -> neg_ch |=| neg ch
      client( | => ch)

proc neg_server :: | LogConsole => Neg(Echo) = 
    | console => neg_ch -> plug 
        => ch, neg_ch -> neg_ch |=| neg ch 
        server( | ch, console => )

proc run_works  = 
    | console => -> plug 
        server(| ch2, console => )
        ch1 => ch2 -> ch1 |=| ch2  
        client( | => ch1 )

proc run2_works  = 
    | console => -> plug 
        neg_server(| console => neg_ch1 )
        neg_client( | neg_ch1 =>  )    
        
proc run3_works  = 
    | console => -> plug 
        neg_server(| console => neg_ch2 )
        neg_ch2 => neg_ch1 -> neg_ch1 |=| neg_ch2
        neg_client( | neg_ch1 =>  )
    
proc run  = 
    | console => -> plug 
        neg_server(| console => neg_ch2 )
        dummy, neg_ch2 => neg_ch1 -> do
            close dummy
            neg_ch1 |=| neg_ch2
        neg_client( | neg_ch1 =>  )
        => dummy -> halt dummy