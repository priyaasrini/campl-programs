protocol StringTerminal => S =
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

protocol Passer( | M ) => S =
    Passer :: M (+) Neg(S)  => S
    -- Passer :: M (+) (Neg(M) (*) S) => S

protocol MemCell (A | ) => S =
    MemPut :: Put(A|S) => S
    MemGet :: Get(A|S) => S
    MemCls :: TopBot => S

fun append :: [A], [A] -> [A] =
    [], ts -> ts
    s:ss, ts -> s : append(ss,ts)

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

defn
    proc p1 :: [Char] | Passer( | MemCell([Char] | ) ) => MemCell([Char]| ), StringTerminal =
        tag | passer => mem, _strterm -> hcase passer of 
            Passer -> do 
                hput MemGet on mem
                get inp on mem

                hput StringTerminalPut on _strterm
                put append(tag, " Receiving: ") on _strterm

                hput StringTerminalPut on _strterm
                put inp on _strterm

                hput StringTerminalPut on _strterm
                put append(tag, " Enter a string: ") on _strterm

                hput StringTerminalGet on _strterm
                get ninp on _strterm

                hput MemPut on mem
                put ninp on mem

                fork passer as
                    mmem with mem -> mmem |=| mem

                    negpasser with _strterm -> do
                        plug
                            p2(tag | => npasser, _strterm)
                            npasser, negpasser => -> negpasser |=| neg npasser


    proc p2 :: [Char] |  => Passer( | MemCell([Char] | )), StringTerminal =
        tag | => passer, _strterm -> do
            hput Passer on passer
            split passer into mem, negpasser

            plug
                p1(tag | npasser => mem, _strterm)
                => negpasser, npasser -> negpasser |=| neg npasser
                    
proc run =
    | => _strterm0, _strterm1 -> do
        plug 
            p1("A:" | passer => mem, _strterm0)
            p2("B:" |        => passer, _strterm1)
            memCell( "I like dogs" | mem => )