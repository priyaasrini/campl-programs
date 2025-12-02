{-  The compiler does not check for exhaustive hcases and produces run-time error as a result -}
{-  mpl: Ix{Int}.index: Index (1) out of range ((0,0)) -}

protocol Loop => S =
    Continue :: S => S
    Stop :: TopBot => S

proc looper :: | Loop => =
    | ch => -> hcase ch of
        Continue -> looper(| ch =>)
        -- Stop -> on ch do halt

proc run :: | => =
    | => -> plug
        => ch -> on ch do
            hput Stop
            halt
        ch => -> looper( | ch => )