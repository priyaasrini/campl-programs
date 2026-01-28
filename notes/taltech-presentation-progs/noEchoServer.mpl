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

proc client ::  | => Put([Char] | TopBot), StringTerminal =
     | => ch, term -> do
        hput StringTerminalPut on term
        put "Client terminal" on term
        hput StringTerminalGet on term
        get fruit on term                        -- Reading input from the user

        put fruit on ch                          -- send message to server

        hput StringTerminalPut on term           
        put "Press Enter to close" on term
        hput StringTerminalGet on term
        get _ on term 

        hput StringTerminalClose on term         -- close terminal  
        close term

        halt ch

proc server :: | Put([Char] | TopBot), Console => =
    | ch, console => -> do

        get fruit on ch
        close ch

        hput ConsolePut on console
        put fruit on console
        hput ConsoleClose on console
        halt console
        
proc run :: | Console => StringTerminal =
    | console => term -> plug
        client( | => ch, term )
        server( | ch, console => )