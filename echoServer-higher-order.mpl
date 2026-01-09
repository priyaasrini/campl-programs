include Prelude

proc hello_world :: | Console =>  =
     | console =>  ->  do 
        on console do 
            hput ConsolePut
            put "Stored process says: Hello World "
            hput ConsoleClose 
            halt

proc client :: | => Put( Store( | Console => ) | TopBot  ) =
    | => ch -> do
        on ch do 
            put store(hello_world) 
            halt

proc server :: | Put( Store( | Console => ) | TopBot  ), Console => =
    | ch, console => -> do
        on ch do
            get stored_process 
            close 

        on console do 
            hput ConsolePut 
            put ("Server says: Running the stored process")
        
        use(stored_process)( | console => )

proc run = 
    | console =>  -> plug 
        client( | => ch )
        server( | ch, console => )