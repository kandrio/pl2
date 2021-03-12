import Data.Char ()
import System.IO ()
import Text.Read
    ( Read(readPrec), lexP, (<++), Lexeme(Symbol, Punc, Ident) )
import qualified Data.Map as Map  

data Type  =  Tvar Int | Tfun Type Type                        deriving Eq
data Expr  =  Evar String | Eabs String Expr | Eapp Expr Expr  deriving Eq

-- Pretty printing of expressions

always :: Bool
always = True    -- False omits parentheses whenever possible

instance Show Expr where
  showsPrec p (Evar x) = (x ++)
  showsPrec p (Eabs x e) =
    showParen (always || p > 0) ((("\\" ++ x ++ ". ") ++) . showsPrec 0 e)
  showsPrec p (Eapp e1 e2) =
    showParen (always || p > 1) (showsPrec 1 e1 . (" " ++) . showsPrec 2 e2)

-- Parsing of expressions

instance Read Expr where
  readPrec = (do Ident x <- lexP
                 return (Evar x)) <++
             (do Punc "(" <- lexP
                 Punc "\\" <- lexP
                 Ident x <- lexP
                 Symbol "." <- lexP
                 e <- readPrec
                 Punc ")" <- lexP
                 return (Eabs x e)) <++
             (do Punc "(" <- lexP
                 e1 <- readPrec
                 e2 <- readPrec
                 Punc ")" <- lexP
                 return (Eapp e1 e2))

-- Pretty printing of types

instance Show Type where
  showsPrec p (Tvar alpha) = ("@" ++) . showsPrec 0 alpha
  showsPrec p (Tfun sigma tau) =
    showParen (p > 0) (showsPrec 1 sigma . (" -> " ++) . showsPrec 0 tau)

-- Main program

{-

Explanation of the INPUTS and OUTPUTS of the 'expr_to_constr' function:

expr_to_constr Expression Variables_Database Variables_Counter = 
  (Type_of_Expression, Constraints, Updated_Variables_Counter, TypeError_Bool)

INPUTS
- Expression: An expression given as input.
- Variables_Database: A 'dict' type structure (key = name of the variable, value = an ID of the 
type of the variable) that keeps track of the types of the variables that we encounter as we 
go deeper in the expression. Initially, it's an empty Map.
- Variables_Counter: It becomes the type ID of any newly inserted variable. Then it gets incremented.
Initially, it's set to 0.

OUTPUTS:
- Type_of_Expression: The type of the Expression that was given as input.  
- Constraints: A list of type constraints. A constraint will be represented as a tuple of the form: 
(Type, Type). This tuple shows that the left Type is equal to the right Type. 
- Updated_Variables_Counter: The incremented Variables_Counter, after we find the type of the Expression.
- TypeError_Bool: It is set to True if we encounter a variable that was not previously added to the
Variable_Database.

-}
expr_to_constr :: Expr -> (Map.Map String Int) -> Int -> (Type, [(Type,Type)], Int, Bool)
expr_to_constr (Evar var_name) var_db counter_id = case (Map.lookup var_name var_db) of
  Just var_id -> (Tvar var_id, [], counter_id, False)
  Nothing     -> (Tvar (-42), [], counter_id, True)
expr_to_constr (Eabs var_name expr) var_db counter_id = ((Tfun type1 type2), constraints, counter_id', error)
  where
    type1 = Tvar counter_id
    (type2, constraints, counter_id', error) = expr_to_constr expr (Map.insert var_name counter_id var_db) (counter_id+1)
expr_to_constr (Eapp expr1 expr2) var_db counter_id = (final_type, constraints, counter_id''+1, error)
  where 
    (type1, constr1, counter_id' , error1)  = expr_to_constr expr1 var_db counter_id 
    (type2, constr2, counter_id'', error2)  = expr_to_constr expr2 var_db counter_id'
    final_type = Tvar counter_id''
    constraints = (type1, Tfun type2 final_type):(constr1 ++ constr2) 
    error = error1 || error2

{-

This is the main function that calls 'expr_to_constr'.

-}
find_constraints expr = (constraints, type_of_expression, error)
  where   
    (type_of_expression, constraints, _, error) = expr_to_constr expr (Map.empty) 0


{-

This function implements the W algorithm (finding the most general unifier).

-}
unify :: [(Type, Type)] -> ([(Type, Type)], Bool)
unify [] = ([], False)
unify ((type1, type2):tail)
  | type1 == type2 = unify tail
unify ((Tvar a, type2):tail)
  | (is_not_in a type2) = ((Tvar a, type2):new_constraints, error)
  | otherwise = ([], True)
    where
      (new_constraints, error) = unify updated_tail
      updated_tail = replace (a, type2) tail
unify ((type1, Tvar a):tail) 
  | (is_not_in a type1) = ((Tvar a, type1):new_constraints, error)
  | otherwise = ([], True)
    where
      (new_constraints, error) = unify updated_tail
      updated_tail = replace (a, type1) tail
unify ((Tfun type11 type12, Tfun type21 type22):tail) = 
  unify ((type11, type21):(type12, type22):tail)

{-

Checks whether a certain variable of ID: 'a' is contained in a Type.

-}
is_not_in :: Int -> Type -> Bool
is_not_in a (Tvar b)
  | a == b = False
  | otherwise = True
is_not_in a (Tfun type1 type2) = (is_not_in a type1) && (is_not_in a type2)

{-

Replaces a certain type-variable (Tvar a) in a given type with the variable's equivalent 
type (atype).

-}
replace_in_type :: (Int, Type) -> Type -> Type  
replace_in_type (a, atype) (Tvar a')
  | a == a' = atype
  | otherwise = Tvar a'
replace_in_type (a, atype) (Tfun sub_type1 sub_type2) = Tfun sub_type1' sub_type2'
  where
    sub_type1' = replace_in_type (a, atype) sub_type1
    sub_type2' = replace_in_type (a, atype) sub_type2


{-

Uses the 'replace_in_type' function to replace a certain type-variable (Tvar a) in
both parts of a constraint that has a form: (type1, type2).

-}
replace_in_constraint :: (Int, Type) -> (Type, Type) -> (Type, Type)
replace_in_constraint (a, atype) (type1, type2) = (type1', type2')
  where
    type1' = replace_in_type (a, atype) type1
    type2' = replace_in_type (a, atype) type2


{-

Uses the 'replace_in_constraint' function to replace a certain type-variable (Tvar a) in
a list of constraints that has a form: [(type1, type2)...].

-}
replace :: (Int, Type) -> [(Type, Type)] -> [(Type, Type)]
replace (a, atype) constraints = map (replace_in_constraint (a, atype)) constraints

{-

After the most general unifier is computed, we "apply" the constraints to the initial type,
which was computed by the 'expr_to_constr' function.

-}
replace_unified_constr_in_type :: [(Type, Type)] -> Type -> Type  
replace_unified_constr_in_type [] final_type = final_type  
replace_unified_constr_in_type ((type1, type2):constraints) my_type = final_type
  where
    final_type = replace_unified_constr_in_type constraints (replace' (type1, type2) my_type) 

{-

Helper function for 'replace_unified_constr_in_type'.

-}
replace' (type1, type2) (Tvar v)
  | (type1 == Tvar v) = type2
  | otherwise = Tvar v  
replace' (type11, type12) (Tfun type21 type22) = Tfun type21' type22'
  where 
    type21' = replace' (type11, type12) type21  
    type22' = replace' (type11, type12) type22

{-

Does the reordering in type-variable IDs, so that they appear in ascending order. 

-}
reorder :: Type -> Type  
reorder my_type = final_type  
  where  
    (_, _, final_type) = reorder' my_type Map.empty 0  
      where  
        reorder' :: Type -> (Map.Map Int Int) -> Int  -> (Map.Map Int Int, Int, Type)  
        reorder' (Tvar var) var_db counter_ID = case (Map.lookup var var_db) of  
          Just v  -> (var_db, counter_ID, (Tvar v))  
          Nothing -> (Map.insert var counter_ID var_db, counter_ID + 1, (Tvar counter_ID))  
        reorder' (Tfun type1 type2) var_db counter_ID = (var_db'', counter_ID'', Tfun type1' type2')  
          where  
            (var_db', counter_ID', type1') = reorder' type1 var_db counter_ID   
            (var_db'', counter_ID'', type2') = reorder' type2 var_db' counter_ID'   

readOne :: IO ()
readOne  =  do  s <- getLine
                let e = read s :: Expr
                let (constraints, type_of_expr, error1) = find_constraints e
                if error1 then putStrLn "type error"
                else do
                  let (unified_constraints, error2) = unify constraints
                  if error2 then putStrLn "type error" 
                  else putStrLn $ show $ reorder $ replace_unified_constr_in_type unified_constraints type_of_expr

count :: Monad m => Int -> m a -> m [a]
count n m = sequence $ take n $ repeat m

main :: IO [()]
main =  do  n <- readLn
            count n readOne