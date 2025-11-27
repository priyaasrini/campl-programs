protocol StringTerminal => S =
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

protocol Passer( | M ) => S =
    Passer :: M (+) (Neg(M) (*) S)  => S
    PasserStop :: TopBot => S

protocol MemCell (A | ) => S =
    MemPut :: Put(A|S) => S
    MemGet :: Get(A|S) => S
    MemCls :: TopBot => S

proc memCell :: A | MemCell(A | ) => =
    val | ch => -> hcase ch of
        MemPut -> do
            get nval on ch
            memCell(nval | ch => )
        MemGet -> do
            put val on ch
            memCell(val | ch => )
        MemCls -> do
            halt ch
        
proc p1 :: |  Passer( | MemCell([Char] | ) ) => MemCell([Char]| ), StringTerminal =
    | passer => mem, _strterm -> do

        hput MemGet on mem
        get inp on mem

        hput StringTerminalPut on _strterm
        put "(p1) Current value in memcell: " on _strterm

        hput StringTerminalPut on _strterm
        put inp on _strterm

        hput StringTerminalPut on _strterm
        put "(p1) Enter a string: " on _strterm

        hput StringTerminalGet on _strterm
        get ninp on _strterm

        hput MemPut on mem
        put ninp on mem

        hcase passer of 
            Passer -> fork passer as 
                mmem with mem -> mmem |=| mem

                negmemandnpasser with _strterm -> do
                    split negmemandnpasser into negmem, npasser
                    plug 
                        p1( | npasser => z, _strterm)
                        z, negmem => -> negmem |=| neg z
            PasserStop -> do
                hput StringTerminalPut on _strterm
                put "(p1) Closing everything" on _strterm

                -- Stop closing the terminal too fast
                hput StringTerminalGet on _strterm
                get _ on _strterm

                hput StringTerminalClose on _strterm
                close _strterm
                hput MemCls on mem
                close mem

                halt passer


proc p2 :: |  => Passer( | MemCell([Char] | )), StringTerminal =
    | => passer, _strterm -> do
        hput StringTerminalPut on _strterm
        put "(p2) Enter a string or stop with <Enter>: " on _strterm

        hput StringTerminalGet on _strterm
        get ninp on _strterm

        case ninp of
            [] -> do
                hput PasserStop on passer

                hput StringTerminalPut on _strterm
                put "(p2) Closing everything" on _strterm

                -- Stop closing the terminal too fast
                hput StringTerminalGet on _strterm
                get _ on _strterm

                hput StringTerminalClose on _strterm
                close _strterm

                halt passer
                
            _:_ -> do
                hput Passer on passer
                split passer into mem, negmemandnpasser

                hput MemGet on mem
                get inp on mem

                hput StringTerminalPut on _strterm
                put "(p2) Old value read from memcell:" on _strterm

                hput StringTerminalPut on _strterm
                put inp on _strterm

                hput MemPut on mem
                put ninp on mem

                hput StringTerminalPut on _strterm
                put "(p2) New value written to memcell:" on _strterm

                hput StringTerminalPut on _strterm
                put ninp on _strterm

                fork negmemandnpasser as 
                    negmem with mem -> negmem |=| neg mem 
                    npasser with _strterm -> p2( | => npasser, _strterm )
                    
proc run =
    | => _strterm0, _strterm1 -> do
        plug 
            p1( | passer => mem, _strterm0)
            p2( |        => passer, _strterm1)
            memCell( "I like dogs" | mem => )