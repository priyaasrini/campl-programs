include Prelude

-- process helloworld
proc helloworld :: | Console =>  =
    | console => -> do
        on console do 
            hput ConsolePut     -- tell the console process that we intend to send a message of type [Char]
            put "Hello World"   -- send the message 

            hput ConsoleClose   -- tell the console process that we intend to close the channel 
            halt                -- close the channel and halt

proc run =
    | console => -> helloworld( |console=>)  -- create and run the process helloworld