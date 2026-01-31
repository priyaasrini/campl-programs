coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S)
    ConsoleClose :: S => TopBot

proc equate_2 :: | M, P => M, P =
    | in1, in2 => out1, out2 -> plug 
        in1 => dummy, out1 -> do
            on dummy do close
            in1 |=| out1
        in2, dummy => out2 -> do
            on dummy do close
            in2 |=| out2

data List(A) -> S =
    ListEmpty :: -> S
    ListCons :: A, S -> S

codata S -> Fun(A, B) =
    App :: A, S -> B

fun from_list :: [A] -> List(A) =
    [] -> ListEmpty
    a:s -> ListCons(a, from_list(s) )

{- equivalent append/3 implementations in prolog
% option 1
append(Xs0, Ys, Zs0) :-
    ( Xs0 = [], Ys = Zs0
    ; Xs0 = [X | Xs], Zs0 = [X | Zs], append(Xs, Ys, Zs)
    ).

% option 2
append([], Ys, Ys).
append([X | Xs], Ys, Zs0) :- Zs0 = [X | Zs], append(Xs, Ys, Zs).

% option 3
append([], Ys, Ys).
append([X | Xs], Ys, [X | Zs]) :- append(Xs, Ys, Zs).
-}

fun append :: List(A), List(A) -> List(A) =
    ListEmpty, list2 -> list2
    ListCons(head1, list1), list2 -> ListCons(head1, append(list1, list2))


fun interleave :: List(A), List(A) -> List(A) =
    ListEmpty, list2 -> list2
    ListCons(head1, list1), list2 -> ListCons(head1, interleave(list2, list1))

fun map :: Fun(A, B), List(A) -> List(B) =
    _, ListEmpty -> ListEmpty
    f, ListCons(x, xs) -> ListCons(App(x, f), map(f, xs))


protocol PList( | M) => S =
    PListEmpty :: TopBot => S
    PListCons :: M (*) S => S

proc plist_cons :: | M, PList( | M) => PList( | M) =
    | ch, chs => ret_chs -> do
        on ret_chs do hput PListCons
        fork ret_chs as
            ret_ch with ch -> ret_ch |=| ch
            ret_list with chs -> ret_list |=| chs

proc p_append :: | PList( | M), PList( | M) => PList( | M) =
    | chs1, chs2 => ret_chs -> hcase chs1 of
        PListEmpty -> do
            close chs1
            chs2 |=| ret_chs
        PListCons -> do
            split chs1 into ch1,new_chs1
            plug
                plist_cons( | ch1, step_chs => ret_chs )
                p_append( | new_chs1, chs2 => step_chs )

proc p_interleave :: | PList( | M), PList( | M) => PList( | M) =
    | chs1, chs2 => ret_chs -> hcase chs1 of
        PListEmpty -> do
            close chs1
            chs2 |=| ret_chs
        PListCons -> do
            split chs1 into ch1,new_chs1
            plug
                plist_cons( | ch1, step_chs => ret_chs )
                p_interleave( | chs2, new_chs1 => step_chs ) 


protocol Proc( | M, P) => S =
    ProcRun :: M (*) Neg(P) => S
    ProcDup :: S (+) S => S
    ProcClose :: TopBot => S

coprotocol S => CoProc( | M, P) =
    CoProcRun :: S => Neg(M) (+) P
    CoProcDup :: S => S (*) S
    CoProcClose :: S => TopBot

proc p_map :: | PList( | M) => Proc( | M, P), PList( | P) =
    | chs => p, ret_chs -> hcase chs of
        PListEmpty -> do
            on chs do close
            on p do
                hput ProcClose
                close
            on ret_chs do
                hput PListEmpty
                halt
        PListCons -> do
            split chs into ch,new_chs
            plug
                plist_cons( | applied_ch, step_chs => ret_chs )
                ch, step_p => p, applied_ch -> do
                    hput ProcDup on p
                    split p into p1, new_step_p
                    hput ProcRun on p1
                    fork p1 as
                        ch1 with ch, step_p, new_step_p -> equate_2( | ch, step_p => ch1, new_step_p )
                        neg_applied_ch with applied_ch -> neg_applied_ch |=| neg applied_ch
                p_map( | new_chs => step_p, step_chs )


-- homework for priyaa: simplify using plist_cons (and perhas equate_2)
{- proc cp_map :: | CoProc( | M, P), PList( | M) => PList( | P) =
    | cp, chs => ret_chs -> hcase chs of
        PListEmpty -> do
            on chs do close
            on cp do
                hput CoProcClose
                close
            on ret_chs do
                hput PListEmpty
                halt
        PListCons -> do
            split chs into ch,new_chs
            on cp do
                hput CoProcDup
                split into cp1, new_cp
            on cp1 do hput CoProcRun
            fork cp1 as
                neg_ch with ch -> neg_ch |=| neg ch
                r1 with new_chs, new_cp, ret -> do
                    on ret do hput PListCons
                    fork ret as
                        r with r1 -> r1 |=| r
                        new_ret with new_chs, new_cp -> cp_map( | new_cp, new_chs => new_ret ) 
-}

            -- this should work, but the compliler complains that plug has a cycle
            {-
            split chs into ch,new_chs
            plug 
                plist_cons( | applied_ch, step_chs => ret_chs ) 
                cp, ch => step_cp, applied_ch -> do
                    on cp do
                        hput CoProcDup
                        split into cp1, new_step_cp
                    on cp1 do hput CoProcRun
                    fork cp1 as 
                        neg_ch with ch -> neg_ch |=| neg ch
                        applied_ch1 with applied_ch, step_cp, new_step_cp -> equate_2( | applied_ch1, new_step_cp => applied_ch, step_cp )
                cp_map( | step_cp, new_chs => step_chs )
            -}

-- because we only can race on Put: M = Put(A | P)
 proc p_listrace :: | Put(A | P), PList( | Put(A | P)) => Put(A | P), PList( | Put(A | P)) =
    | ch1, chs => ret_winner, ret_losers -> hcase chs of
        PListEmpty -> do
            on chs do close
            on ret_losers do
                hput PListEmpty
                close
            ret_winner |=| ch1

        PListCons -> do
            on chs do split into ch2,new_chs
            plug
                -- subprocess call
                p_listrace( | ch2, new_chs => rec_winner, rec_losers )
                -- racer
                ch1, rec_winner => ret_winner, step_loser -> race
                    rec_winner -> equate_2( | rec_winner, ch1 => ret_winner, step_loser )
                    ch1 -> equate_2( | ch1, rec_winner => ret_winner, step_loser ) 
                -- subprocess call
                plist_cons( | step_loser, rec_losers => ret_losers )

proc print_list :: List([Char]) | Console => =
    ListEmpty | console => -> on console do
        hput ConsolePut
        put "Empty"
        hput ConsoleClose
        halt
    ListCons(x,xs) | console => -> do
        on console do
            hput ConsolePut
            put x
        print_list(xs | console => )

{-
proc test = -- test plug 
    | => -> do 
        plug 
            => ch1, ch2 -> do 
                close ch1 
                halt ch2 
            ch2 => ch3 -> do 
                close ch2 
                halt ch3
            ch1, ch3 => -> do 
                close ch1 
                halt ch3 
-}

proc main =
    | console => -> do 
         case ["list1", "asdf"] of
            left -> case ["list2"] of
                right -> case (App := _ -> "app") of
                    f -> print_list(append(map(f, from_list(["list1", "asdf"])), from_list(["list2"])) | console =>)

proc run :: | Console => =
    | console => -> main(| console => )
