include Prelude

proc subclient :: [Char] | => P =
    tag | => ch -> do
        -- rest of the code

proc client :: | => P (*) P =
    | => _2_ch -> 
        fork _2_ch as
            ch1 -> subclient("Client 1: " | => ch1)
            ch2 -> subclient("Client 2: " | => ch2)

proc server :: | P (*) P => =
    | _2_ch => -> do
        split _2_ch into ch1, ch2
        
        -- rest of the code
        
proc run :: | =>  =
    |  =>  -> plug
        client( | => _2_ch )
        server( | _2_ch => )