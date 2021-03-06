-- Program which produces a time series simulating the "flip" transducer
-- of Holmes and Isbell ("Looping Suffix Tree-Based Inference of Partially
-- Observable Hidden State", ICML 2006)
-- They present the model as a transducer with two hidden states, three
-- inputs or actions, "u", "r" and "l", and two outputs or observations,
-- "0" and "1".  (They do not name their states.)  In the first state,
-- the inputs l and u loop back, and produce a 0, but the action r goes
-- to the other state and produces a 1.  In that state, r and u loop
-- back, emitting 0s, and l switches states, producing a 1.
-- They claim to run CSSR on this, with the "actions [i.e., inputs] chosen
-- at random".  If one did that and just observed the outputs, both
-- states would produce 0 with probability 2/3, 1 with probability 1/3,
-- and this would just be a biased coin, so their simulations would make
-- no sense at all.  I think they must give it input-output pairs to
-- predict.  Then we have a five-symbol alphabet, "l0", "l1", "r0", "r1"
-- and "u0", with probabilities and transitions as follows:
--
-- In state A:
--     "l0" -> A, prob. 1/3
--     "l1" forbidden
--     "r0" forbidden
--     "r1" -> B, prob. 1/3
--     "u0" -> a, prob. 1/3
-- In state B:
--     "l0" forbidden
--     "l1" -> A prob. 1/3
--     "r0" -> B prob. 1/3
--     "r1"  forbidden
--     "u0" -> B prob. 1/3
-- To accomodate CSSR, I will use a five-letter alphabet, where lower-case
-- letters are accompanied by 0s, upper-case by 1s, i.e.
-- "l0" <-> "l", "l1" <-> "L", "r0" <-> "r", "r1" <-> "R", "u0" <-> "u"
-- Thus, the suffixes l, L, r and R are all resolving, and only u* is
-- ambiguous.
-- We pick an initial state equiprobably.
--
-- The first input is the number of steps to simulate.  That is the only
-- input.
-- The output symbols are unspaced.
-- Output is to files named TMD_foo.
-- To be used in conjunction with Kris's state-inference program.

module Test.Machines.FlipPerHolmesAndIsabell where

data FlipOutput = R0 | R1 | L0 | L1 | U
  deriving (Eq, Ord)

instance Show FlipOutput where
  show o = case o of
    R0 -> "r"
    R1 -> "R"
    L0 -> "l"
    L1 -> "L"
    U  -> "u"

data FlipState = A | B
  deriving (Eq, Ord, Show)

class SeriesGenerating FlipState where
  type Symbol FlipState = FlipOutput
  -- FIXME: this state should be equi-probabilistic
  initial = A

  -- FIXME: come up with a better way of handling this
  move A L0 = A
  move A L1 = undefined
  move A R0 = undefined
  move A R1 = B
  move A U  = A

  move B L0 = undefined
  move B L1 = A
  move B R0 = B
  move B R1 = undefined
  move B U  = B

  distribution A =
    [ (1/3, L0)
    , (0.0, L1)
    , (0.0, R0)
    , (1/3, R1)
    , (1/3, U )
    ]

  distribution B =
    [ (0.0, L0)
    , (1/3, L1)
    , (1/3, R0)
    , (0.0, R1)
    , (1/3, U )
    ]
