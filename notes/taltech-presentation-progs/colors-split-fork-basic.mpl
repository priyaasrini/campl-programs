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

proc subclient :: [Char] | => Put([Char] | Get([Char] | TopBot)), StringTerminal =
    tag | => ch, term -> do
        hput StringTerminalPut on term
        put tag on term
        hput StringTerminalGet on term
        get fruit on term

        put fruit on ch 
        get echoed on ch
        hput StringTerminalPut on term
        put "Server echo ~> " ++ echoed on term

        hput StringTerminalPut on term
        put "Press Enter to close" on term
        hput StringTerminalGet on term
        get _ on term 

        hput StringTerminalClose on term
        close term
        halt ch

proc client :: | => Put([Char] | Get([Char] | TopBot)) (*) Put([Char] | Get([Char] | TopBot)), StringTerminal, StringTerminal =
    | => _2_ch, term1, term2 -> fork _2_ch as
        ch1 -> subclient("Client 1: " | => ch1, term1)
        ch2 -> subclient("Client 2: " | => ch2, term2)

proc server :: | Put([Char] | Get([Char] | TopBot)) (*) Put([Char] | Get([Char] | TopBot)), Console => =
    | _2_ch, console => -> do
        split _2_ch into ch1, ch2
        
        get fruit on ch1
        hput ConsolePut on console
        put "Client 1: " ++ fruit  on console
        put fruit on ch1 
        close ch1

        get fruit on ch2
        hput ConsolePut on console
        put "Client 2: " ++ fruit on console
        put fruit on ch2 
        close ch2 

        hput ConsoleClose on console
        halt console
        
proc run :: | Console => StringTerminal, StringTerminal =
    | console => term1, term2 -> plug
        client( | => _2_ch, term1, term2 )
        server( | _2_ch, console => )