# Type safe finite state machines

I was working on a C# project that contained an implementation of a finite state machine, fsm for short, that expressed some business rules. The fsm was implemented using the [state pattern](https://en.wikipedia.org/wiki/State_pattern) described by the Gang of Four.

Let's, for the sake of argument, assume that we have the fsm as below

```csharp
interface IState 
{ 
    void TransitionToIntermediate(StateContext context);
    void TransitionToFinal(StateContext context);
}

class StateInitial : IState
{
    public void TransitionToIntermediate(StateContext context) => context.SetState(new StateIntermediate());
    public void TransitionToFinal(StateContext context) => throw new Exception("Invalid state transition");
}

class StateIntermediate : IState
{
    public void TransitionToIntermediate(StateContext context) => throw new Exception("Invalid state transition");
    public void TransitionToFinal(StateContext context) => context.SetState(new StateFinal());
}

class StateFinal : IState
{
    public void TransitionToIntermediate(StateContext context) => throw new Exception("Invalid state transition");
    public void TransitionToFinal(StateContext context) => throw new Exception("Invalid state transition");
}

class StateContext
{
    private IState state = new StateInitial();

    public void SetState(IState newState) => state = newState;
    public void TransitionToIntermediate() => state.TransitionToIntermediate(this);
    public void TransitionToFinal() => state.TransitionToFinal(this);
}
```

Three states, **Initial**, **Intermediate** and **Final** and two transitions **Initial → Intermediate** and **Intermediate → Final**. It is not allowed to transition from directly from **Initial** to **Final**. My task was to add some new code which would require the said transition. 

```csharp
void Program(bool quick) 
{
    var context = new StateContext();
    if (quick) {  // the new case
        context.TransitionToFinal();
    }  else {  // the old case
        context.TransitionToIntermediate();
        context.TransitionToFinal();
    }
}    
```

I forgot to modify the *Initial* state to actually allow the transition. I practice the code was a lot more complicated that this of course, and even though I thought I had test coverage, I had not. The code passed code reviews and was set into production where it failed. The thing is; **the code compiles**. For all the compiler knows, the code is just fine, since our way of preventing illegal transitions is using **runtime exceptions**. That is a weakness of the state pattern when used to implement a fsm. It has other weaknesses, such as being rather verbose among other things, but lets keep our focus on compile time verification.

## Can we do better? 

Spoiler alert: **yes we can**

At some point I ended up at Oskar Wickströms blog posts on the subject. He had produced two posts which are both good and worth reading. 

* [Finite-State Machines, Part 1: Modeling with Haskell Data Types](https://wickstrom.tech/finite-state-machines/2017/11/10/finite-state-machines-part-1-modeling-with-haskell.html)
* [Finite-State Machines, Part 2: Explicit Typed State Transitions](https://wickstrom.tech/finite-state-machines/2017/11/19/finite-state-machines-part-2.html)

The first post presents a simple but straight forward approach. The idea is to have a single function that takes a state, an event and then produces an new state (along with some action).

```haskell
data State = Initial | Intermediate | Final
data Event = TransitionToIntermediate | TransitionToFinal

fsm :: Monad m => State -> Event -> m State  
fsm Initial TransitionToIntermediate = return Intermediate
fsm Intermediate TransitionToFinal = return Final
fsm state _ = error "invalid state transition"
```

Easy to understand and quick to implement. Note that the monad *m* in the code above is some kind of effect monad, such as the IO monad that allows us to express actions as side effects.
However, illegal state transitions are still not caught at compile time. 

In the second post Oskar takes a more advanced approach. The article involves some other techniques but the core idea is that, since our states are different we should use different types to model them so that the compiler is also aware of the different kinds of states. A simplified version is shown below.

```haskell
data Initial = Initial
data Intermediate = Intermediate
data Final = Final

initial :: Monad m => m Initial
initial = return Initial

transitionToIntermediate :: Monad m => Initial -> m Intermediate
transitionToIntermediate _ = return Intermediate

transitionToFinal :: Monad m => Intermediate -> m Final
transitionToFinal _ = return Final
```

Now illegal transitions cannot be expressed (without writing new transition functions which is okay). Our initial program would not compile.

```haskell
program quick = do
    initialState <- initial
    if quick then do
        transitionToFinal initialState  -- compile time error: initial state does not match the type Intermediate
    else do
        intermediateState <- transitionToIntermediate initialState
        transitionToFinal intermediateState
```

However, as Oskar points out, we need to manually pass the state around which is prone to errors. We might accidentally reuse an old state. The following program compiles without errors even though it transitions to intermediate state twice (and thus triggers the monad side effect twice as well)

```haskell
program quick = do
    initialState <- initial
    ignored <- transitionToIntermediate initialState
    intermediateState <- transitionToIntermediate initialState  -- re-trigger the transition from initial to intermediate
    transitionToFinal intermediateState
```

## Can we do even better? 

Spoiler alert: **yes we can**

The problem with the second approach above is that we need to manually pass the state around. If that was done by the framework we could just focus on the program we want to write. 

The standard approach to avoid explicitly passing state around is to leverage a **state monad**. A limitation of the ordinary state monad is that the state type cannot be changed through the computation. Luckily, some smart people came up with the **indexed state monad** which is a generalization of the state monad that allows just that.

It might be worth pointing out that the indexed state monad here should not be confused with the effect monad introduced earlier. Their purposes are entirely different. The purpose of the indexed state monad is to make sure we only carry out valid state transitions, while the purpose of the effect monad is the allow our code to execute actions. However, to get a working solution we need both. This means that we need to combine the indexed state monad and an effect monad. Monads are generally combined using **monad transformers** which is just the right tool. In our case we need an **index state monad transformer** that accepts an ordinary effect monad. Beautiful! Let's get to work.

## Monad definitions

Most of the definitions below comes from [Kwang Yul Seo's article on Indexed monads](https://kseo.github.io/posts/2017-01-12-indexed-monads.html) and from [Benjamin Hodgson excellent stackoverflow post](https://stackoverflow.com/questions/39328175/type-safe-flow-state-machine).

```haskell
module IxMonadCore where

-- define indexed monads
class IxMonad m where
    ireturn :: a -> m p p a
    ibind :: m p q a -> (a -> m q r b) -> m p r b

-- define indexed state monad transformer
newtype IxStateT m si so v = IxStateT { runIxStateT :: si -> m (so,v) }

instance Monad m => IxMonad (IxStateT m) where
  ireturn x = IxStateT (\si -> return (si,x))
  ibind (IxStateT m) f = IxStateT (\si -> m si >>= (\ (sm,x) -> runIxStateT (f x) sm))

imodify :: Monad m => (si -> so) -> IxStateT m si so ()
imodify f = IxStateT (\si -> return (f si,()))
```

Indexed monads are not really monads so the do-notation does not work out-of-the-box. Luckily, haskell allows us to override the do notation be leveraging the RebindableSyntax extension.

```haskell
{-# LANGUAGE RebindableSyntax #-}
module IxMonadDoNotation where

import Prelude hiding ((>>=), (>>), return)
import IxModuleCore

return :: (Monad m) => a -> IxStateT m si si a
return = ireturn

(>>=) :: (Monad m) => IxStateT m p q a -> (a -> IxStateT m q r b) -> IxStateT m p r b
(>>=) = ibind

(>>) :: (Monad m) => IxStateT m p q a -> IxStateT m q r b -> IxStateT m p r b
v >> w = v >>= \_ -> w
```

## Implementation

With the index monad definitions in place we can apply it to the problem. The type safe fsm is given below.

```haskell
data Initial = Initial
data Intermediate = Intermediate
data Final = Final

type EffectMonad = IO

transitionToIntermediate :: IxStateT EffectMonad Initial Intermediate ()
transitionToIntermediate = imodify (\_ -> Intermediate) >> return ()

transitionToFinal ::IxStateT EffectMonad Intermediate Final ()
transitionToFinal = imodify (\_ -> Final) >> return ()
```

Assuming above definitions the program is translated to

```haskell
program = runIxStateT program' Initial
    where program' = do
                        transitionToIntermediate
                        transitionToFinal
```

Repeated transitions are now invalid as well. The following code does not compile.

```haskell
program = runIxStateT program' Initial
    where program' = do
        transitionToIntermediate
        transitionToIntermediate  -- invalid repeated transition gives a compile time error
        transitionToFinal
```

To actually perform actions with side effects we need to lift the operations into the indexed state monad. Here is a quick example of this for completeness sake.

```haskell
class IxMonadTrans t where
    ilift :: Monad m => m a -> t m i i a

instance IxMonadTrans IxStateT where
    ilift m = IxStateT $ \s -> m >>= \x -> return (s, x)

programWithIO = runIxStateT programWithIO' Initial
    where programWithIO' = do
                            ilift $ putStrLn "Initial"
                            transitionToIntermediate
                            ilift $ putStrLn "Intermediate"
                            transitionToFinal
                            ilift $ putStrLn "Final"
```

The source code for the examples can be found [here](basics).

A full implementation of the state machine in Oskar Wickström's examples is provided [here](full).

Finally, I note that Oskar implemented an even more advanced fsm library called [motor](https://github.com/owickstrom/motor) which is inspired by [the idris ST library](http://docs.idris-lang.org/en/latest/st/state.html).