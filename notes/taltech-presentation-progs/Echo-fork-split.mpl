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
                put "Message to server :- "
                hput StringTerminalGet
                get message
                hput StringTerminalClose 
                close 

            on ch do 
                hput EchoSend 
                put message 
                halt  

proc client_spawner :: | => Clients( | Echo), StringTerminal =
    | => clients, term -> do
        on term do 
            hput StringTerminalPut 
            put "Spawn a client? (y for Yes, press any other key to exit)" 
            hput StringTerminalGet 
            get input 

        case isExit(input) of 
            True -> do 
                on term do 
                    hput StringTerminalClose
                    close 
                on clients do 
                    hput Nil
                    halt 
            False -> do 
                hput StringTerminalDup on term 
                split term into term1, term2
                hput Cons on clients
                fork clients as 
                    new_client with term1 -> client( | => new_client, term1 ) 
                    more_clients with term2 -> client_spawner( |  => more_clients, term2 )

proc server :: | Clients(  | Echo ), Console => =
    | ch, console => -> do
        hcase ch of
            Cons -> do
                split ch into new_client, more_clients

                hcase new_client of 
                    EchoSend -> do 
                        get input on new_client 
                        on console do
                            hput ConsolePut 
                            put ("I received " ++ input) 
                        close new_client

                        server( | more_clients, console => )                 
            Nil -> do
                on console do 
                    hput ConsolePut
                    put "Done"
                    hput ConsoleClose
                    close 
                halt ch

proc run = 
    | console => term -> plug 
        client_spawner( | => ch, term )
        server( | ch, console => )