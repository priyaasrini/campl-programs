-- Hello World on the console

include Prelude

proc run =
      | console =>
       -> do on console do
              hput ConsolePut
              put "Hello World!"
	      hput ConsoleClose
	      halt