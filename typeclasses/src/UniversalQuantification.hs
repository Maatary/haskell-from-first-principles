{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeApplications #-}

module UniversalQuantification where


-- :set -XExplicitForAll -- Enable to write explicitly with the forall Syntax from the Repl
-- :set -fprint-explicit-foralls -- The Repl Write Back with explicit quantification
-- :set -XTypeApplications -- (Only works when the type signature is defined explicitly) -- To Apply the Type explicitly.

{-
  The implicit for all made explicit.
  Using :set -XExplicitForAll in the REPL we can get it to write all signature with the explicit quantification.
-}

-- The id function
id' ::  a -> a
id' = id
-- is equivalent to
id'' :: forall a. a -> a
id'' = id