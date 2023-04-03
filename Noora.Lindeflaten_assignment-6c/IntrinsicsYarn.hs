-- | A yarn calculator (faithfully following P8T): signature, model and calculator.
--
-- Author Magne Haveraaen
-- Since 2021-05-22 (yarn API) and 2022-03-31 (personnel calculator)

module IntrinsicsYarn where

-- Use signatures
import Pam8TSignature

-- Use the calculator template for signatures and function models.
import Pam8TCalculatorTemplate

import Pam8TSignatureAST

-----------------------
-- | Declaration of Signature and their argument list and return type.
yarnSignature :: Signature
yarnSignature
  = ([("Amount","The number of rolls of yarn (amount)"),
      ("Cost","The total cost for a purchase of yarn (NOK)"),
      ("Density","Density of thread (gram/meter)"),
      ("Length","Length of a roll of yarn (meter)"),
      ("UnitCost","The unit cost for yarn (NOK/meter)"),
      ("Weight","Weight of a roll of yarn (gram)")
     ],
     [ 
      ("Add", ["Cost","Cost"],"Cost","Add two costs"),
      ("Sub", ["Cost","Cost"],"Cost","Subtract two costs"),
      ("Mult", ["Density","Length"],"Weight","Compute weight from density and length"),
      ("Slash", ["Weight","Density"],"Length","Compute length from weight and density"),
      ("Slash", ["Weight","Length"],"Density","Compute density"),
      ("Add", ["Length","Length"],"Length","Add two lengths"),
      ("Sub", ["Length","Length"],"Length","Subtract two lengths"),
      ("Mult", ["Length","Amount"],"Length","Multiply length by amount"),
      ("Slash", ["Length","Length"],"Amount","Compute amount"),
      ("Mult", ["UnitCost","Length"],"Cost","Compute cost based on length"),
      ("Slash", ["Cost","Length"],"UnitCost","Compute unit cost from cost and length of yarn"),
      ("Add", ["Weight","Weight"],"Weight","Add two weights"),
      ("Sub", ["Weight","Weight"],"Weight","Subtract two weights"),
      ("Mult", ["Weight","Amount"],"Weight","Multiply weight by amount")
    ])

-----------------------
-- | Semantics of chosen yarn Signature.
yarnSemantics :: FunModel undefined
yarnSemantics = undefined 

-----------------------
-- | Function creating test data.
yarnTestData :: [TypeName] -> [undefined]
yarnTestData = undefined

-- | Inferring the type of a value
-- typeOfValue :: ValueType undefined
--typeOfValue = undefined


-----------------------
-- | Unit test of the yarn signature:
-- • For each type in the signature, check that test data function generates data of the expected type.
-- • For each function declaration in the signature check that there is a corresponding function model.
unittestIntrinsicsYarn = do
  print $ "did nothing"


-----------------------
-- | Interactive calculator with variables and given selection of yarn Signature.
{- | Run the following commands in sequence at the prompt
SetVar "a" (Fun ...)
show
-}
main = do
  putStrLn $ "-- Calculator for yarn --"
  -- calculatorTemplate yarnSignature yarnSemantics yarnTestData typeOfValue

