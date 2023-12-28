module Inst (
    Inst (..),
    Code (..)
) where

data Inst =
    Push Integer
    | Add
    | Mul
    | Sub
    | PushTrue
    | PushFalse
    | Eq
    | Le
    | And
    | Neg
    | Fetch String
    | Store String
    | Noop
    | Branch Code Code
    | Loop Code Code
    deriving Show

type Code = [Inst]