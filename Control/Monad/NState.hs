{- |
  Module      :  Control.Monad.NState
  Copyright   :  (c) 2013, Stuart A. Kurtz
  License     :  BSD (3-Clause) (see ./LICENSE)
  
  Maintainer  :  stuart@cs.uchicago.edu
  Stability   :  experimental
 
  A nondeterministic state monad.
-}

module Control.Monad.NState(NState,get,put,state,modify,run,eval,exec,branch) where

import Control.Monad.State

{- |
  A non-deterministic state monad parameterized by the type s of the state to carry.
-}

type NState s = StateT s []

{- |
	Unwrap an "NState" computation as a function.
-}

run :: NState s a  -- ^ nondeterministic state computation
	-> s		   -- ^ initial state
	-> [(a,s)]     -- ^ list of return values and final states
run = runStateT

{- |
  Evaluate a non-deterministic state computation with the given initial
  state and return the list of final values, discarding the final states.
-}

eval :: NState s a  -- ^ nondeterministic state computation
	 -> s		    -- ^ initial state
	 -> [a]         -- ^ list of return values
eval = evalStateT

{- |
  Evaluate a non-deterministic state computation with the given initial
  state and return the list of final states, discarding the final vailes.
-}

exec :: NState s a  -- ^ nondeterministic state computation
	 -> s		    -- ^ initial state
	 -> [s]         -- ^ list of return states
exec = execStateT

{- |
  Non-deterministically select a value from the argument list.
-}

branch :: [a]            -- ^ list of alternatives
          -> NState s a  -- ^ nondeterministic state computation
branch = lift
