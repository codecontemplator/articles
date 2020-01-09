data State = Initial | Intermediate | Final deriving Show
data Event = TransitionToIntermediate | TransitionToFinal deriving Show

fsm :: Monad m => State -> Event -> m State  
fsm Initial TransitionToIntermediate = return Intermediate
fsm Intermediate TransitionToFinal = return Final
fsm state _ = return state -- catch all fallback