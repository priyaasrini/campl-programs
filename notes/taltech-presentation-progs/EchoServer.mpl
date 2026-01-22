include Prelude

{-- 
------------| -----------------------------------| ----------------| -----------------------|
Polarity    |    Channel type                    | Process command | Resulting channel type |
------------| -----------------------------------| ----------------| -----------------------|
output      | Put([Char] | Get([Char] | TopBot)) | put "message"   | Get([Char] | TopBot)   | 
------------| -----------------------------------| ----------------| -----------------------|
output      | Get([Char] | TopBot)               | get message     | TopBot                 | 
------------| -----------------------------------| ----------------| -----------------------|
(in/out)put | TopBot                             | close/halt      |           --           | 
------------| -----------------------------------| ----------------| -----------------------|
input       | Put([Char] | Get([Char] | TopBot)) | get message     | Get([Char] | TopBot)   | 
------------| -----------------------------------| ----------------| -----------------------|
input       | Get([Char] | TopBot)               | put message     | TopBot                 | 
------------| -----------------------------------| ----------------| -----------------------| --}

proc client ::  | => Put([Char] | Get([Char] | TopBot)), StringTerminal =
     | => ch, term -> do
        hput StringTerminalPut on term
        put "Client terminal" on term
        hput StringTerminalGet on term
        get fruit on term                        -- Reading input from the user

        put fruit on ch                          -- send message to server
        get echoed on ch                         -- receive message from server 
        hput StringTerminalPut on term           
        put "Server echo ~> " ++ echoed on term  -- print message on the terminal 

        hput StringTerminalPut on term           
        put "Press Enter to close" on term
        hput StringTerminalGet on term
        get _ on term 

        hput StringTerminalClose on term         -- close terminal  
        close term
        halt ch

proc server :: | Put([Char] | Get([Char] | TopBot)), Console => =
    | ch, console => -> do

        get fruit on ch
        hput ConsolePut on console
        put "Client: " ++ fruit  on console
        put fruit on ch 
        close ch

        hput ConsoleClose on console
        halt console
        
proc run :: | Console => StringTerminal =
    | console => term -> plug
        client( | => _2_ch, term )
        server( | _2_ch, console => )