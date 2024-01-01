module Inst (
    Inst (..),
    Code (..)
) where

-- Definition of the 'Inst' data type representing individual instructions.
data Inst =
    Push Integer      -- Pushes an integer onto the stack
  | Add               -- Addition operation
  | Mult              -- Multiplication operation
  | Sub               -- Subtraction operation
  | Tru               -- Pushes True to the stack
  | Fals              -- Pushes False to the stack
  | Equ               -- Equality comparison
  | Le                -- Less than or equal comparison
  | And               -- Logical AND operation
  | Neg               -- Logical NOT operation
  | Fetch String      -- Fetches the value of a variable from the state
  | Store String      -- Stores a value into a variable in the state
  | Noop              -- No-operation
  | Branch Code Code  -- Conditional branching based on the top of the stack
  | Loop Code Code    -- Loop construction
  deriving Show

-- Definiton of alias for a list of instructions.
type Code = [Inst]