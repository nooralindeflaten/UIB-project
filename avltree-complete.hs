import Prelude hiding (elem)


data BalanceFactor = LeftHeavy
                   | Balanced
                   | RightHeavy
    deriving (Eq, Show)

data AVLTree a = Empty
               | Branch BalanceFactor
                        (AVLTree a)
                        a
                        (AVLTree a)
  deriving (Eq, Show)


leaf :: a -> AVLTree a
leaf x = Branch Balanced Empty x Empty

height Empty = 0
height (Branch _ l _ r) = 1 + max (height l) (height r)

size Empty = 0
size (Branch _ l _ r) = 1 + size l + size r

elem :: (Ord a) => a -> AVLTree a -> Bool
elem x Empty = False
elem x (Branch _ less y greater)
   = if x == y
     then True
     else if x < y
          then elem x less
          else elem x greater

insert' :: (Ord a) => a -> AVLTree a ->(Bool , AVLTree a)
insert' x (Branch LeftHeavy lesser a greater)
  | x < a = let (grew, newLesser) = insert' x lesser
            in if not grew
               then (False, Branch LeftHeavy newLesser a greater)
               else case newLesser of
                 Empty -> error "This is a bug."
                 (Branch LeftHeavy lesserb b greaterb)
                   -> (False, Branch Balanced
                             lesserb
                             b
                             (Branch Balanced
                                     greaterb a greater))
                 (Branch RightHeavy
                         lesserb
                         b
                         (Branch cbf lesserc c greaterc))
                     -> (False, Branch Balanced
                               (Branch (if cbf == RightHeavy
                                        then LeftHeavy
                                        else Balanced) lesserb b lesserc)
                               c
                               (Branch (if cbf == LeftHeavy
                                        then RightHeavy
                                        else Balanced) greaterc a greater))
                 (Branch Balanced _ _ _) -> error "foo"
                 (Branch RightHeavy _ _ Empty) -> error "RightHeavy, but empty"
  | a < x = let (grew , newGreater) = insert' x greater
            in (False, Branch (if grew
                              then Balanced
                              else LeftHeavy)
                              lesser a newGreater)
  | otherwise = (False,(Branch LeftHeavy lesser a greater))
insert' x (Branch RightHeavy lesser a greater)
  | x < a = let (grew, newLesser) = insert' x lesser
            in (False, Branch (if grew
                               then Balanced
                               else RightHeavy)
                              newLesser a greater)
  | a < x = let (grew, newGreater) = insert' x greater
            in if not grew
               then (False, Branch RightHeavy lesser a newGreater)
               else case newGreater of
                 Empty -> error "This is a bug"
                 (Branch LeftHeavy (Branch cbf lesserc c greaterc)
                                   b
                                   greaterb)
                  -> (False, Branch Balanced
                                    (Branch (if cbf == RightHeavy
                                             then LeftHeavy
                                             else Balanced)
                                            lesser a lesserc)
                                    c
                                    (Branch (if cbf == LeftHeavy
                                             then RightHeavy
                                             else Balanced)
                                            greaterc b greaterb))
                 (Branch RightHeavy between
                                   b
                                   greaterb)
                   -> (False, Branch Balanced
                                     (Branch Balanced lesser a between) b greaterb)
                 (Branch Balanced _ _ _) -> error "bar"
                 (Branch LeftHeavy Empty _ _) -> error "LeftHeavy, but empty"
  | otherwise = (False, (Branch RightHeavy lesser a greater))
insert' x (Branch Balanced lesser a greater)
  | x < a = let (grew, newLesser) = insert' x lesser
            in (grew, Branch (if grew
                              then LeftHeavy
                              else Balanced)
                             newLesser
                             a
                             greater)
  | a < x = let (grew, newGreater) = insert' x greater
            in (grew, Branch (if grew
                              then RightHeavy
                              else Balanced)
                             lesser
                             a
                             newGreater)
   | otherwise = (False, (Branch Balanced lesser a greater))
insert' x Empty = (True, leaf x)



insert x t = snd (insert' x t)
