{- 
    This program demonstrates a client which requests message from the server
-}

include Prelude

protocol MessageReq => S =
    SendReq :: Get([Char]|TopBot) => S -- handles have new names

proc client ::  |  => MessageReq , StringTerminal =
    | => ch, term -> do
            on ch do 
                hput SendReq             --request message 
                get message              --receive message 
                close                    --close channel

            on term do                   --print and halt     
                hput StringTerminalPut
                put "Message from server :- " ++ message
                hput StringTerminalGet
                get _
                hput StringTerminalClose 
                halt    

proc server :: | MessageReq, Console => = 
    | ch, console => -> do 
        hcase ch of                         --server is blocked until it receives a session handle from the client
            SendReq -> do                   --client has requested message 
                on ch do                      
                    put "Hello World!"      --send a message to client 

                on console do 
                    hput ConsolePut 
                    put "Sent message to client"
                    hput ConsoleClose
                    close
                
                halt ch

proc run :: | Console => StringTerminal = 
    | console => term -> 
        plug 
            client( | => ch, term) 
            server( | ch, console => )

