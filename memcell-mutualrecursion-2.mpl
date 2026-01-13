include Prelude

protocol StringTerminal => S =
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

protocol Mutex( | M ) => S =
    Pass :: M (+) Neg(S)  => S

protocol MemCh (A | ) => S =
    MemPut :: Put(A|S) => S
    MemGet :: Get(A|S) => S
    MemCls :: TopBot => S

fun append :: [A], [A] -> [A] =
    [], ts -> ts
    s:ss, ts -> s : append(ss,ts)

proc memCell :: A | MemCh(A | ) => =
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
    proc memAccess :: [Char] | Mutex( | MemCh([Char] | ) ) => MemCh([Char]| ), StringTerminal =
        tag | mutex => mem, _strterm -> hcase mutex of 
            Pass -> do 
                hput MemGet on mem
                get inp on mem

                on _strterm do 
                    hput StringTerminalPut
                    put (tag++ " Receiving: ")

                    hput StringTerminalPut
                    put inp

                    hput StringTerminalPut
                    put append(tag, " Enter a string: ")

                    hput StringTerminalGet
                    get ninp

                hput MemPut on mem
                put ninp on mem

                fork mutex as
                    mmem with mem -> mmem |=| mem

                    negmutex with _strterm -> do
                        plug
                            memWait(tag | => nmutex, _strterm)
                            nmutex, negmutex => -> negmutex |=| neg nmutex


    proc memWait :: [Char] |  => Mutex( | MemCh([Char] | )), StringTerminal =
        tag | => mutex, _strterm -> do
            hput Pass on mutex
            split mutex into mem, negmutex

            plug
                memAccess(tag | nmutex => mem, _strterm)
                => negmutex, nmutex -> negmutex |=| neg nmutex
                    
proc run =
    | => _strterm0, _strterm1 -> do
        plug 
            memAccess("A:" | mutex => mem, _strterm0)
            memWait("B:" |        => mutex, _strterm1)
            memCell( "I like dogs" | mem => )