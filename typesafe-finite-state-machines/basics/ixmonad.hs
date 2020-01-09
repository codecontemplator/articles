{-# LANGUAGE RebindableSyntax #-}

import IxMonadCore
import IxMonadDoNotation
import Prelude hiding ((>>=), (>>), return)

-- actual program
data Initial = Initial
data Intermediate = Intermediate
data Final = Final

type EffectMonad = IO

transitionToIntermediate :: IxStateT EffectMonad Initial Intermediate ()
transitionToIntermediate = imodify (\_ -> Intermediate) >> return ()

transitionToFinal ::IxStateT EffectMonad Intermediate Final ()
transitionToFinal = imodify (\_ -> Final) >> return ()

program = runIxStateT program' Initial
    where program' = do
                        ilift $ putStrLn "Initial"
                        transitionToIntermediate
                        ilift $ putStrLn "Intermediate"
                        transitionToFinal
                        ilift $ putStrLn "Final"
