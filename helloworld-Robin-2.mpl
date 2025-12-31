-- Hello World on a terminal

include Prelude

proc run =
      | console =>
       -> do  hput ConsoleStringTerminal on console
              split console into cons,termneg
	      hput ConsoleClose on cons
	      close cons
	      plug
	        term, termneg => ->
		      termneg |=| neg term
	        => term ->
		     do on term do
		          hput StringTerminalPut
                          put "Hello World!"
			  hput StringTerminalGet
			  get _ 
	                  hput StringTerminalClose
	                  halt