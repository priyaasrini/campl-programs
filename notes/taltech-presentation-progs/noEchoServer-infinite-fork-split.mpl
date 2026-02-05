{- 
A client_spawner runs in the console. 
Each new client gets its own terminal. 
Each new client sends a message and closes. 
The server receives a message and has its own terminal. 
-}

include Prelude 

fun isExit :: [Char] -> Bool = 
    [] -> True
    x:xs -> if x == 'y' then False  else True 

protocol Echo => S =
    EchoSend :: Put([Char] | TopBot) => S

protocol Clients (  | T ) => S = 
    Cons ::  T (*) S => S 
    Nil  :: TopBot => S

proc client ::  |  => Echo, StringTerminal =
    | => ch, term -> do
            on term do 
                hput StringTerminalPut
                put "Client Window "
                hput StringTerminalPut
                put "Message to server :- "
                hput StringTerminalGet
                get message

            on ch do 
                hput EchoSend 
                put message 
                close  

            on term do
                hput StringTerminalPut
                put "Press any key to close "
                hput StringTerminalGet
                get _
                hput StringTerminalClose 
                halt 

proc client_spawner :: | Console => Clients( | Echo) =
    | console => clients -> do
        on console do 
            hput ConsolePut 
            put "Spawn a client? (y for Yes, press any other key to exit)" 
            hput ConsoleGet 
            get input 

        case isExit(input) of 
            True -> do 
                on console do 
                    hput ConsolePut
                    put "Done"
                    hput ConsoleClose
                    close 
                on clients do 
                    hput Nil
                    halt 
            False -> do 
                hput ConsoleStringTerminal on console
                split console into new_console, neg_term

                hput Cons on clients
                fork clients as 
                    new_client with neg_term -> do 
                        plug 
                            client( | => new_client, term ) 
                            term, neg_term => -> neg_term |=| neg term
                    more_clients with new_console -> client_spawner( | new_console => more_clients )

proc server :: | Clients(  | Echo ) => StringTerminal =
    | ch => term -> do
        hcase ch of
            Cons -> do
                split ch into new_client, more_clients

                hcase new_client of 
                    EchoSend -> do 
                        get input on new_client 
                        on term do
                            hput StringTerminalPut 
                            put ("Server: I received " ++ input) 
                        close new_client

                        server( | more_clients => term )                 
            Nil -> do
                on term do 
                    hput StringTerminalPut
                    put "Done"
                    hput StringTerminalClose
                    close 
                halt ch

proc run :: | Console => StringTerminal =
    | console => term -> do 
            hput StringTerminalPut on term
            put "Server Window:" on term
            plug 
                client_spawner( | console => ch )
                server( | ch => term )