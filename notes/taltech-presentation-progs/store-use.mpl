include Prelude

proc hello_world :: Int | Console =>  =
    counter | console =>  ->  do 
        on console do 
            hput ConsolePut
            put ("Hello World ")
            hput IntConsolePut
            put counter
            hput ConsoleClose 
            halt
        
proc hello_runner :: Store( Int | Console => ) | Console =>  =
    stored_process | console =>  -> do 
        use(stored_process)( 5 | console => ) 

proc run :: | Console =>  =
    | console =>  -> hello_runner( store(hello_world) | console => )