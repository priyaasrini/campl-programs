-- protocol Input => Output =
--    InputRead :: Input => Get( [Char] | Output)

coprotocol
    S => ReadConsole = 
    --    ConsolePut :: S => Get( [Char] | S)
    --    ConsoleGet :: S => Put( [Char] | S)
        ReadConsoleGet :: S => Put( [Char] | T)
        ReadConsoleClose :: S => TopBot 

    and

    T => WriteConsole =
        WriteConsolePut :: T => Get( [Char] | S)

proc helloworld :: | ReadConsole => = 
    | console => -> do
--        hput ConsoleGet on console
--        get x on console
--        -- put "Hello World" on console

--        hput ConsolePut on console
--        put x on console

        hput ReadConsoleGet on console
        get x on console

        hput WriteConsolePut on console
        put x on console

        hput ReadConsoleClose on console
        halt console

proc run = 
    | console => -> helloworld( |console=>)