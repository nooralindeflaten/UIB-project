module IntrinsicsYarn0 where
-- | A yarn calculator (faithfully following P8T): signature, model and calculator.
--
-- Author Magne Haveraaen
-- Since 2021-05-22 (yarn API) and 2022-03-31 (personnel calculator)

-- Use signatures
import Pam8TSignature

-- Use the calculator template for signatures and function models.
import Pam8UCalculatorTemplate

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
-- Semantics domain for yarn data, The ValueDomain being Yarn 

data Yarn = Cost Double
    | Amount Double
    | Density Double
    | Length Double
    | UnitCost Double 
    | Weight Double
    deriving(Show,Eq,Read)
-----------------------
-- | Semantics of chosen yarn Signature.
yarnSemantics :: FunModel Yarn
yarnSemantics "Add" [Cost c1, Cost c2] = Cost c3 where 
    c3 = c1 + c2
yarnSemantics "Sub" [Cost c1, Cost c2] = Cost c3 where
    c3 = c1 - c2
yarnSemantics "Mult" [Density d1, Length l1] = Weight i where
    i = d1 * l1
yarnSemantics "Slash" [Weight w1, Density d1] = Length i where 
    i = w1 / d1
-- compute weight (gram) from density (gram/meters)
yarnSemantics "Slash" [Weight w1, Length l1] =  Density c3 where
    c3 = w1 / l1
yarnSemantics "Add" [Length l1, Length l2] = Length i where
    i = l1 + l2
yarnSemantics "Sub" [Length l1, Length l2] = Length i where
    i = l1-l2
yarnSemantics "Mult" [Length li, Amount a1] = Length i where
    i = li * a1
yarnSemantics "Slash" [Length l1, Length l2] = Amount i where
    i = l1 / l2
yarnSemantics "Mult" [UnitCost uc, Length l] = Cost i where
    i = uc * l
yarnSemantics "Slash" [Cost c, Length l] = UnitCost i where
    i = c / l
yarnSemantics "Add" [Weight w1, Weight w2] = Weight w3 where
    w3 = w1 + w2
yarnSemantics "Sub" [Weight w1, Weight w2] = Weight w3 where
    w3 = w1 - w2
yarnSemantics "Mult" [Weight w1, Amount a1] = Weight w3 where
    w3 = w1 * a1
yarnSemantics fname alist = error $ "try again wrong type: " ++ (show alist)


{-

yarnTestData :: [TypeName] -> [Double]
yarnTestData args = take (length args) [10..20]

getArgs :: FunName -> [FunDeclaration] -> [TypeName]
getArgs name ((fname,args,res,doc):funs) = 
    if name == fname then args else getArgs name funs

getRets :: FunName -> [FunDeclaration] -> [TypeName]
getRets name ((fname,args,res,doc):funs) =
    if name == fname then res else getRets name funs

typeOfV :: UnitName -> TypeName
typeOfV name = case name of
  "amount" -> "Amount"
  "NOK" -> "Cost"
  "gram/meter" -> "Density"
  "meter" -> "Length"
  "NOK/meter" -> "UnitCost"
  "gram" -> "Weight"
  _ -> error "Wrong"

typeOfVal :: ValueType Yarn
typeOfVal (Cost _) = "Cost"
typeOfVal (Amount _) = "Amount"
typeOfVal (Density _) = "Density"
typeOfVal (Length _) = "Length"
typeOfVal (UnitCost _) = "UnitCost"
typeOfVal (Weight _) = "Weight"
-}
-----------------------
-- | Function creating test data.

yarnTestData :: [TypeName] -> [Yarn]
yarnTestData [] = []
yarnTestData ("Cost":typs) = Cost 30.0:yarnTestData typs
yarnTestData ("Amount":typs) = Amount 4.5:yarnTestData typs
yarnTestData ("Density":typs) = Density 5.0:yarnTestData typs
yarnTestData ("Length":typs) = Length 8.0:yarnTestData typs
yarnTestData ("UnitCost":typs) = UnitCost 7.8:yarnTestData typs
yarnTestData ("Weight":typs) = Weight 5.6:yarnTestData typs

typeOfValue :: ValueType valuedomain
typeOfValue _ = undefined
{-

typeOfValue :: ValueType Yarn
typeOfValue a 0 = case a of
    Amount ->"Amount"
    Cost -> "Cost"
    Density -> "Density"
    Length -> "Length"
    UnitCost -> "UnitCost"
    Weight -> "Weight"
  _ -> error "Wrong"


-- from the valuedomain we can now search for the unit name.
-- in the valuedomain yarn we find the typenames of the valuedomain.
-- now map the type of the value to  


-----------------------
-- | Unit test of the yarn signature:
-- â€¢ For each type in the signature, check that test data function generates data of the expected type.
-- â€¢ For each function declaration in the signature check that there is a corresponding function model.
unittestIntrinsicsYarn0 = do
  print $ "Nothing"

-}
-----------------------
-- | Interactive calculator with variables and given selection of yarn Signature.
{- | Run the following commands in sequence at the prompt
SetVar "a" (Fun ...)
show
-}
main = do
  putStrLn $ "-- Calculator for yarn --"
  calculatorTemplate yarnSignature yarnSemantics yarnTestData typeOfValue



-----------------------
-- | Unit test of the personnelOperations:
-- • For each type in the signature, check that test data function generates data of the expected type.
-- • For each function declaration in the signature check that there is a corresponding function model.
