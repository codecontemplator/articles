data Initial = Initial
data Intermediate = Intermediate
data Final = Final

initial :: Monad m => m Initial
initial = return Initial

transitionToIntermediate :: Monad m => Initial -> m Intermediate
transitionToIntermediate _ = return Intermediate

transitionToFinal :: Monad m => Intermediate -> m Final
transitionToFinal _ = return Final

{-
-- compiler rejects invalid program (good)
program quick = do
    initialState <- initial
    if quick then do
        transitionToFinal initialState  -- compile time error: initial state does not match the type Intermediate
    else do
        intermediateState <- transitionToIntermediate initialState
        transitionToFinal intermediateState
-}

{-
-- compiler accepts invalid program (bad)
program quick = do
    initialState <- initial
    ignored <- transitionToIntermediate initialState
    intermediateState <- transitionToIntermediate initialState  -- re-trigger the transition from initial to intermediate
    transitionToFinal intermediateState        
-}
