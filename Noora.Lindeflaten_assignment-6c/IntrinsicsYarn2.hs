-- | A yarn calculator (faithfully following P8T): signature, model and calculator.
--
-- Author Magne Haveraaen
-- Since 2021-05-22 (yarn API) and 2022-03-31 (personnel calculator)

module IntrinsicsYarn2 where

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
yarnSemantics :: FunModel (Double, UnitName)
yarnSemantics "Add" [(e, "NOK"), (e', "NOK")] = (e + e', "NOK")
yarnSemantics "Add" [(e, "meter"), (e', "meter")] = (e + e', "meter")
yarnSemantics "Add" [(e, "gram"), (e', "gram")] = (e + e', "gram")
yarnSemantics "Sub" [(e, "NOK"), (e', "NOK")] = (e - e', "NOK")
yarnSemantics "Sub" [(e, "meter"), (e', "meter")] = (e - e', "meter")
yarnSemantics "Sub" [(e, "gram"), (e', "gram")] = (e - e', "gram")
yarnSemantics "Mult" [(e, "gram/meter"), (e', "meter")] = (e * e', "gram")
yarnSemantics "Mult" [(e, "meter"), (e', "amount")] = (e * e', "meter")
yarnSemantics "Mult" [(e, "NOK/meter"), (e', "meter")] = (e * e', "NOK")
yarnSemantics "Mult" [(e, "gram"), (e', "amount")] = (e * e', "gram")
yarnSemantics "Slash" [(e, "gram"), (e', "gram/meter")] = (e / e', "meter")
yarnSemantics "Slash" [(e, "gram"), (e', "meter")] = (e / e', "gram/meter")
yarnSemantics "Slash" [(e, "meter"), (e', "meter")] = (e / e', "amount")
yarnSemantics "Slash" [(e, "NOK"), (e', "meter")] = (e / e', "NOK/meter")

-----------------------
-- | Function creating test data.
yarnTestData :: [TypeName] -> [(Double, UnitName)]
yarnTestData [] = []
yarnTestData ("Amount" : xs) = (5.4, "amount") : yarnTestData xs
yarnTestData ("Cost" : xs) = (7.8, "NOK") : yarnTestData xs
yarnTestData ("Density" : xs) = (9.2, "gram/meter") : yarnTestData xs
yarnTestData ("Length" : xs) = (9.1, "meter") : yarnTestData xs
yarnTestData ("UnitCost" : xs) = (1.4, "NOK/meter") : yarnTestData xs
yarnTestData ("Weight" : xs) = (3.8, "gram") : yarnTestData xs
yarnTestData _ = error $ "You did something wierd"

-- | Inferring the type of a value
typeOfValue :: ValueType (Double, UnitName)
typeOfValue (_, a) = case a of
  "amount" -> "Amount"
  "NOK" -> "Cost"
  "gram/meter" -> "Density"
  "meter" -> "Length"
  "NOK/meter" -> "UnitCost"
  "gram" -> "Weight"
  _ -> error $ "Oh no you did a wrong thing"


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
  calculatorTemplate yarnSignature yarnSemantics yarnTestData typeOfValue

