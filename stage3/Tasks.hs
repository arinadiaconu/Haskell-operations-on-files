
-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array
import Text.Read

import Common

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]
type ColumnName = String

-- Prerequisities
split_by :: Char -> String -> [String]
split_by x = foldr op [""]
  where op char acc
            | char == x = "":acc
            | otherwise = (char:head(acc)):tail(acc)

read_csv :: CSV -> Table
read_csv = (map (split_by ',')) . (split_by '\n')

write_csv :: Table -> CSV
write_csv = (foldr (++) []).
            (intersperse "\n").
            (map (foldr (++) [])).
            (map (intersperse ","))


{-
    TASK SET 1
-}


-- Task 1

compute_average_steps :: Table -> Table
compute_average_steps m = zipWith (:) (map head m) (build_back_the_table m)

-- this functions takes only the numbers from each list and coverts them to Integer
string_to_integer :: Table -> [[Integer]]
string_to_integer = (map (map read)) . (map tail)

-- this function firstly adds all 8 numbers in each list 
-- and then computes the average by dividing by 8
average :: Table -> [Float]
average m = map (\x -> (fromIntegral x) / (fromIntegral 8)) (map (foldr (+) 0) (string_to_integer m))

-- this function turns all floats in a list back to string and replaces 
-- the head with the required string at the top of the final table
float_to_string :: Table -> [String]
float_to_string m = ["Average Number of Steps"] ++ (tail (map (printf "%.2f") (average m)))

-- this function takes a list of strings and turns each string into a list with that string
-- ["a", "b"] -> [["a"], ["b"]]
build_back_the_table :: Table -> [[String]]
build_back_the_table m = map (foldr (\x (a:acc) -> (x:a):acc) [[]]) (float_to_string m)


-- Task 2

-- Number of people who have achieved their goal:
get_passed_people_num :: Table -> Int
get_passed_people_num m = foldr (\x acc -> acc + 1) 0 (achieved_goal m)

-- this function filters the people who walked at least 1000 steps
achieved_goal :: Table -> [Int]
achieved_goal m = filter (>= 1000) (total_daily_steps m)

-- this function calculates the total number of steps for each person
total_daily_steps :: Table -> [Int]
total_daily_steps m = map (foldr (+) 0) (string_to_int m)

-- this function is the same with string_to_integer, but it returns Int
string_to_int :: Table -> [[Int]]
string_to_int = (map (map read)) . (map tail)

-- Percentage of people who have achieved their:
get_passed_people_percentage :: Table -> Float
get_passed_people_percentage m = (fromIntegral (get_passed_people_num m)) / (fromIntegral (total_number_of_people m))

-- this function calculates the total number of people in the table
total_number_of_people :: Table -> Int
total_number_of_people = foldr (\x acc -> acc + 1) (-1)

-- Average number of daily steps
get_steps_avg :: Table -> Float
get_steps_avg m = (fromIntegral (total_steps_for_all (total_daily_steps m))) / (fromIntegral (total_number_of_people m))

-- this function calculates the number of steps for all the people in the table
total_steps_for_all :: [Int] -> Int
total_steps_for_all l = foldr (+) (0) (tail l)


-- Task 3

get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h m = ["H10","H11","H12","H13","H14","H15","H16","H17"] : (back_to_string m)

-- this function calculates the sum on each column in the table
total_steps_per_h :: Table -> [Int]
total_steps_per_h = map sum . transpose . tail . string_to_int

-- this function computes the average for each total previously calculated
average_per_h :: Table -> [Float]
average_per_h m = foldr aux [] (total_steps_per_h m)
                        where aux :: Int -> [Float] -> [Float]
                              aux x acc = ((fromIntegral x) / (fromIntegral (total_number_of_people m))):acc

-- converts the floats back to string
back_to_string :: Table -> [[String]]
back_to_string m = (map (printf "%.2f") (average_per_h m)) : []


-- Task 4

get_activ_summary :: Table -> Table
get_activ_summary m = ["column","range1","range2","range3"] : (insert_heads m)

-- this function turns strings to float
string_to_float :: Table -> [[Float]]
string_to_float = (map (map read)) . (map tail)

-- this function selects the last 3 columns in the table and transposes them
get_relevant_columns :: Table -> [[Float]]
get_relevant_columns = tail . tail . transpose . tail . string_to_float

-- this function calculates how many people are in the third range on all 3 categories
count_range_3 :: Table -> [Int]
count_range_3 m = map (foldr (\x acc -> acc + 1) 0) (map (filter r3) (get_relevant_columns m))
                      where r3 x = ((x < 500) && (x >= 100))

-- this function calculates how many people are in the second range on all 3 categories
count_range_2 :: Table -> [Int]
count_range_2 m = map (foldr (\x acc -> acc + 1) 0) (map (filter r2) (get_relevant_columns m))
                      where r2 x = ((x < 100) && (x >= 50))

-- this function calculates how many people are in the first range on all 3 categories
count_range_1 :: Table -> [Int]
count_range_1 m = map (foldr (\x acc -> acc + 1) 0) (map (filter (< 50)) (get_relevant_columns m))

-- this function builds up the table that is required at this task
build_required_table :: Table -> Table
build_required_table m = map (map show) (transpose ((count_range_1 m) : (count_range_2 m) : (count_range_3 m) : []))

-- this function inserts the head of each line in the table
insert_heads :: Table -> Table
insert_heads m = aux_f (build_required_table m)
                  where aux_f (x:y:z:xs) = ("VeryActiveMinutes":x) : ("FairlyActiveMinutes":y) : ("LightlyActiveMinutes":z) : xs


-- Task 5

get_ranking :: Table -> Table
get_ranking m = ["Name","Total Steps"] : transpose (((head (transpose (sort_table m))) : 
                                          (head (tail(transpose (sort_table m)))) : []))

-- this function sorts the table using sortBy
sort_table :: Table -> Table
sort_table m = sortBy my_sort_f (tail m)

-- this function explicitly tells the function sortBy how to sort the table
-- first by the total number of steps and then in lexicographic order
my_sort_f (x:y:xs) (a:b:as)
  | y < b = LT
  | y > b = GT
  | otherwise = compare x a


-- Task 6

get_steps_diff_table :: Table -> Table
get_steps_diff_table m = ["Name","Average first 4h","Average last 4h","Difference"] : (sort_diff m)

-- this function calculates the average for the first 4 hours in the table
average_first_4h :: Table -> [Float]
average_first_4h m = tail (map (\x -> (x) / (fromIntegral 4)) (map (foldr (+) 0) (map (take 4) (string_to_float m))))

-- this function calculates the average for the last 4 hours in the table
average_last_4h :: Table -> [Float]
average_last_4h m = tail (map (\x -> (x) / (fromIntegral 4)) (map (foldr (+) 0) (map (drop 4) (string_to_float m))))

-- this function calculates the difference between the elements of the 2 lists previously calculated
difference :: Table -> [Float]
difference m = zipWith (\x y -> if x < y then (y-x) else (x-y)) (average_first_4h m) (average_last_4h m)

-- this function builds the table required from the calculated lists
build_table :: Table -> Table
build_table m = transpose ((tail (map head m)) : (map (printf "%.2f") (average_first_4h m)) :
                (map (printf "%.2f") (average_last_4h m)) : (map (printf "%.2f") (difference m)) : [])

-- this function explicitly tells the function sortBy how to sort the table
-- first by the difference and then in lexicographic order
my_sort_dif (x:y:z:w:xs) (a:b:c:d:as)
  | w < d = LT
  | w > d = GT
  | otherwise = compare x a

-- this function sorts the table using sortBy
sort_diff :: Table -> Table
sort_diff m = sortBy my_sort_dif (build_table m)


-- Task 7

-- Applies the given function to all the values
vmap :: (Value -> Value) -> Table -> Table
vmap f m = map (map f) m


-- Task 8

-- Applies the given function to all the entries
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f s m = s : (map f m)


get_sleep_total :: Row -> Row
get_sleep_total r = (head r) : (to_string r) : []

-- this function calculates the sum for the elements in the row
calculate_sum :: Row -> Float
calculate_sum r = foldr (+) 0 (map read (tail r))

-- this function turns a float to string
to_string :: Row -> String
to_string r = printf "%.2f" (calculate_sum r)


{-
    TASK SET 2
-}


-- Task 1

tsort :: ColumnName -> Table -> Table
tsort column table = (head table) : (sort_after_column table column)

-- this function returns the number of the column with the given ColumnName
count_column :: String -> [String] -> Int
count_column s (x:xs) = if x == s
                        then 1
                        else (1 + (count_column s xs))

-- this function tests whether the elements on the given column are numeric
-- values or strings and calls the comparison functions acordingly
my_compare_function :: String -> String -> String -> String -> Ordering
my_compare_function a b c d = test (readMaybe c)
                                where test :: Maybe Int -> Ordering
                                      test (Just x) = (my_compare_numbers a b (read c) (read d))
                                      test Nothing = (my_compare_strings a b c d)

-- this function compares numeric values for the sortBy function
my_compare_numbers :: String -> String -> Int -> Int -> Ordering
my_compare_numbers x y z w
  | z < w = LT
  | z > w = GT
  | otherwise = compare x y

-- this function compares strings for the sortBy function
my_compare_strings :: String -> String -> String -> String -> Ordering
my_compare_strings x y z w
  | z < w = LT
  | z > w = GT
  | otherwise = compare x y

-- this function recursively searches for the column given by its number and
-- calls the comparison function in order to sort its elements in ascending order
search_elements w z (m:ms) (n:ns) ct
    | ct == 1 = (my_compare_function w z m n)
    | otherwise = (search_elements w z ms ns (ct - 1))

-- this function calls the function above and saves the elements on
-- the first column so that they can be compared for the SortBy function
my_sort_function :: Int -> [String] -> [String] -> Ordering
my_sort_function count (x:xs) (y:ys) = (search_elements x y xs ys (count - 1))

-- this function sorts the table using sortBy
sort_after_column :: Table -> ColumnName -> Table
sort_after_column m column = sortBy (my_sort_function (count_column column (head m))) (tail m)


-- Task 2

vunion :: Table -> Table -> Table
vunion t1 t2 = if (check_headers (head t1) (head t2)) == True
               then t1 ++ (tail t2)
               else t1

-- this function tests if two table headers are the same
check_headers :: Row -> Row -> Bool
check_headers (x:xs) (y:ys) = if x == y
                              then check_headers xs ys
                              else False
check_headers [] [] = True
check_headers _ [] = False
check_headers [] _ = False


-- Task 3

hunion :: Table -> Table -> Table
hunion t1 t2 = my_union t1 t2 (length (head t1)) (length (head t2))

-- this function takes each row in the tables and unites them if
-- they are not empty. If they are empty, they will be padded with ""
my_union :: Table -> Table -> Int -> Int -> Table
my_union (x:xs) (y:ys) l1 l2 = (x++y):(my_union xs ys l1 l2)
my_union [] (y:ys) l1 l2 = ((make_empty_list l1)++y):(my_union [] ys l1 l2)
my_union (x:xs) [] l1 l2 = (x++(make_empty_list l2)):(my_union xs [] l1 l2)
my_union [] [] l1 l2 = []

-- this function builds up an empty row with the given dimension
make_empty_list :: Int -> Row
make_empty_list dimension = if dimension == 0
                            then []
                            else "":(make_empty_list (dimension - 1)) 


-- Task 4

tjoin :: ColumnName -> Table -> Table -> Table
tjoin key_column t1 t2 = (rmvdup ((head t1)++(head t2))) :
          iterate_t1 key_column (head t1) (head t2) (tail t1) (tail t2)

-- this function removes duplicates from a list, leaving the
-- first occurence of each duplicate element in the final list
rmvdup = foldl (\seen x -> if x `elem` seen
                           then seen
                           else seen ++ [x]) []

-- this function iterates through all the rows in the first table and
-- calls the function below in order to find the key for each row
iterate_t1 :: ColumnName -> Row -> Row -> Table -> Table -> Table
iterate_t1 k h1 h2 (r:t1) t2 = if (length (find_key_in_h1 k h1 h2 r t2 r h1)) == 0
                               then iterate_t1 k h1 h2 t1 t2
                               else (find_key_in_h1 k h1 h2 r t2 r h1) : 
                                                  (iterate_t1 k h1 h2 t1 t2)
iterate_t1 _ _ _ [] _ = []

-- this function takes each row in the first table and searches for
-- the element in the column given by the key, calling the function
-- below in order to iterate through the second table
find_key_in_h1 :: ColumnName -> Row -> Row -> Row -> Table -> Row -> Row -> Row
find_key_in_h1 k (el:h1) h2 (x:row) t2 r hd1 = if k == el
                                               then iterate_t2 k h2 x t2 r hd1
                                               else find_key_in_h1 k h1 h2 row t2 r hd1

-- this function iterates through the second table and calls the function
-- below in order to find the element on each row that matches the key column          
iterate_t2 :: ColumnName -> Row -> Value -> Table -> Row -> Row -> Row
iterate_t2 k h2 x (r:t2) r_t1 h1 = if (find_key_in_h2 k h2 x r) == True
                                   then build_row r_t1 r h1 h2 k
                                   else iterate_t2 k h2 x t2 r_t1 h1
iterate_t2 _ _ _ [] _ _ = []

-- this function takes each row in the second table and returns whether the
-- element in the first table on the column given is the same as the element
-- in the second table on the given row
find_key_in_h2 :: ColumnName -> Row -> Value -> Row -> Bool
find_key_in_h2 k (el:h2) x (y:row) = if k == el
                                     then x == y
                                     else find_key_in_h2 k h2 x row

-- this function builds up each new row found in the final table
build_row :: Row -> Row -> Row -> Row -> ColumnName -> Row
build_row r1 r2 h1 h2 k = r1 ++ (check_r2 r1 r2 h1 h2 k)

-- this function is looking for the element in the row from the second
-- table matching the column given by the key
check_r2 :: Row -> Row -> Row -> Row -> ColumnName -> Row
check_r2 r1 (el:row) h1 (x:h2) k = if x == k
                                  then select_values_from_t2 r1 row h1 h2
                                  else x : (check_r2 r1 row h1 h2 k)

-- this function builds up the row by calling the function below in order
-- to save the value from the second table if not empty
select_values_from_t2 :: Row -> Row -> Row -> Row -> Row
select_values_from_t2 r1 (x:row) h1 (el:h2) = (find_same_col r1 h1 el x) :
                                      (select_values_from_t2 r1 row h1 h2)
select_values_from_t2 _ [] _ _ = []

-- this function is looking for the common columns in both tables and compares
-- the values, returning the value in the second table if not empty
find_same_col :: Row -> Row -> Value -> Value -> Value
find_same_col (x:row) (el:h1) el_h2 y = if el == el_h2
                                        then if (length y) == 0
                                             then x
                                             else y
                                        else find_same_col row h1 el_h2 y
find_same_col [] _ _ y = y

-- Task 5

cartesian :: (Row -> Row -> Row) -> [ColumnName] -> Table -> Table -> Table
cartesian new_row_function new_column_names t1 t2 = new_column_names : 
                                      (take_each_row_t1 new_row_function (tail t1) (tail t2))

-- this function takes each row from the first table and calls the next function
-- in order for it to apply the function for each row in the second table
take_each_row_t1 :: (Row -> Row -> Row) -> Table -> Table -> Table
take_each_row_t1 f (x:xs) t2 = (take_each_row_t2 f x t2) ++ (take_each_row_t1 f xs t2)
take_each_row_t1 f [] t2 = []

-- this function takes a row from the first table and applies the given function
-- for each row in the second table
take_each_row_t2 :: (Row -> Row -> Row) -> Row -> Table -> Table
take_each_row_t2 f x (y:ys) = (f x y) : (take_each_row_t2 f x ys)
take_each_row_t2 f x [] = []


-- Task 6

projection :: [ColumnName] -> Table -> Table
projection columns_to_extract t = columns_to_extract : (extract_column columns_to_extract (head t) (tail t))

-- this function iterates through all the rows of the table
-- and builds up again each row by calling the function below
extract_column :: [ColumnName] -> Row -> Table -> Table
extract_column col first_row (x:xs) = (new_row first_row col x) : 
                                                (extract_column col first_row xs)
extract_column _ _ [] = []

-- this function builds up a new row with the columns that need to be
-- extracted from the list with the names of the columns
new_row :: Row -> [ColumnName] -> Row -> Row
new_row (r:row) col (x:xs) = if ((column_to_extract (r:row) col) == True)
                             then x : (new_row row col xs)
                             else new_row row col xs
new_row _ _ [] = []

-- this function gets a row and the list with the names of the columns and
-- calls the function below for all the strings in the row in order to verfy 
-- if they are a match with one of the strings in the column name given
column_to_extract :: Row -> [ColumnName] -> Bool
column_to_extract (x:xs) row = (is_column_in_table x row) || (column_to_extract xs row)
column_to_extract [] row = False

-- this function gets a string and compares it with all elements in 
-- a given row and returns True if that string is found in the row
is_column_in_table :: String -> Row -> Bool
is_column_in_table s (x:xs) = if s == x
                              then True
                              else is_column_in_table s xs
is_column_in_table s [] = False


-- Task 7

filterTable :: (Value -> Bool) -> ColumnName -> Table -> Table
filterTable condition key_column t = (head t) : 
                          (filter_line condition key_column (head t) (tail t))

-- this function iterates through all the rows of the table
-- and filters each row by calling the function below
filter_line :: (Value -> Bool) -> ColumnName -> Row -> Table -> Table
filter_line f c_name first_row (x:xs) = if ((row_condition first_row c_name x f) == True)
                                        then x : (filter_line f c_name first_row xs)
                                        else filter_line f c_name first_row xs
filter_line _ _ _ [] = []

-- this function takes the first row of the table, the key, a row from
-- the table and the filtering function and if it finds the key in the
-- first row, it applies the function to the current element of the row
row_condition :: Row -> ColumnName -> Row -> (Value -> Bool) -> Bool
row_condition (el:f_row) c_name (x:xs) f = if el == c_name
                                           then f x
                                           else row_condition f_row c_name xs f
row_condition _ _ [] _ = False

-- Task 8 TO_DO


{-
    TASK SET 3
-}


-- 3.1

data Query =
    FromTable Table
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query -- 3.4
    | Graph EdgeOp Query -- 3.5

instance Show QResult where
    show (List l) = show l
    show (Table t) = show t

class Eval a where
  eval :: a -> QResult

instance Eval Query where
  eval (FromTable table) = Table table

  eval (AsList colname (FromTable t)) = List (iterate_table colname (tail t) (head t))
                                              where iterate_table col (x:xs) first_row = (search_value x col first_row) : 
                                                                                         (iterate_table col xs first_row)
                                                    iterate_table col [] first_row = []  
                                                    search_value (el:l) col (x:xs) = if col == x
                                                                                     then el
                                                                                     else search_value l col xs
                                                    search_value [] _ _ = undefined
  
  eval (Sort colname (FromTable t)) = Table (tsort colname t)

  eval (ValueMap op (FromTable t)) = Table (vmap op t)

  eval (RowMap op colnames (FromTable t)) = Table (rmap op colnames (tail t))

  eval (VUnion (FromTable t1) (FromTable t2)) = Table (vunion t1 t2)

  eval (HUnion (FromTable t1) (FromTable t2)) = Table (hunion t1 t2)

  eval (TableJoin colname (FromTable t1) (FromTable t2)) = Table (tjoin colname t1 t2)

  eval (Cartesian op colnames (FromTable t1) (FromTable t2)) = Table (cartesian op colnames t1 t2)

  eval (Projection colnames (FromTable t)) = Table (projection colnames t)

  eval (Filter filter_cond (FromTable t)) = Table ((head t) : (iterate_t (head t) filter_cond (tail t)))
                                                    where iterate_t h f (x:xs) = if (feval h f x) == True
                                                                                 then x : (iterate_t h f xs)
                                                                                 else iterate_t h f xs
                                                          iterate_t _ _ [] = []

  eval (Graph edge_op (FromTable t)) = Table (nub (["From","To","Value"] : (iterate_lines (tail t) edge_op)))
                                                    where iterate_lines :: Table -> EdgeOp -> Table
                                                          iterate_lines [x] _ = []
                                                          iterate_lines [] _ = []
                                                          iterate_lines (x:xs) op = (iterate_each_subtable x xs op) ++ 
                                                                                    (iterate_lines xs op)
                                                          iterate_each_subtable :: Row -> Table -> EdgeOp -> Table
                                                          iterate_each_subtable _ [] _ = [] 
                                                          iterate_each_subtable row (x:xs) op = case (edge_op row x) of
                                                                                                     (Just value) -> if ((head row) < (head x)) 
                                                                                                                     then ((head row) : (head x) : [value]) : (iterate_each_subtable row xs op)
                                                                                                                     else ((head x) : (head row) : [value]) : (iterate_each_subtable row xs op)
                                                                                                     Nothing -> (iterate_each_subtable row xs op)


-- 3.2 & 3.3

type FilterOp = Row -> Bool

data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp

instance FEval Float where
  feval [] _ _ = False
  feval _ _ [] = False

  feval (h:hd) (Eq colname ref) (el:row) = if h == colname
                                           then if (read el) == ref then True else False
                                           else feval hd (Eq colname ref) row
  
  feval (h:hd) (Lt colname ref) (el:row) = if h == colname
                                           then if (read el) < ref then True else False
                                           else feval hd (Lt colname ref) row
  
  feval (h:hd) (Gt colname ref) (el:row) = if h == colname
                                           then if (read el) > ref then True else False
                                           else feval hd (Gt colname ref) row

  feval (h:hd) (In colname list) (el:row) = if h == colname
                                            then search_interval el list
                                            else feval hd (In colname list) row
                                                        where search_interval el (x:xs) = if (read el) == x
                                                                                          then True
                                                                                          else search_interval el xs
                                                              search_interval el [] = False

  feval (h:hd) (FNot (Eq colname ref)) (el:row) = if h == colname
                                                  then if (read el) /= ref then True else False
                                                  else feval hd (FNot (Eq colname ref)) row

  feval (h:hd) (FNot (Lt colname ref)) (el:row) = if h == colname
                                                  then if (read el) >= ref then True else False
                                                  else feval hd (FNot (Lt colname ref)) row

  feval (h:hd) (FNot (Gt colname ref)) (el:row) = if h == colname
                                                  then if (read el) <= ref then True else False
                                                  else feval hd (FNot (Gt colname ref)) row

  feval (h:hd) (FNot (In colname list)) (el:row) = if h == colname
                                                  then search_interval el list
                                                  else feval hd (FNot (In colname list)) row
                                                        where search_interval el (x:xs) = if (read el) == x
                                                                                          then False
                                                                                          else search_interval el xs
                                                              search_interval el [] = True

  feval (h:hd) (FieldEq cn1 cn2) (el1:row) = if h == cn1
                                             then compare_next (read el1) row hd cn2
                                             else if h == cn2
                                                  then compare_next (read el1) row hd cn1
                                                  else search_for_first_el hd cn1 cn2 row
                                                        where search_for_first_el :: [String] -> String -> String -> Row -> Bool
                                                              search_for_first_el (h:hd) cn1 cn2 (el1:row)
                                                                  | h == cn1 = compare_next (read el1) row hd cn2
                                                                  | h == cn2 = compare_next (read el1) row hd cn1
                                                                  | otherwise = search_for_first_el hd cn1 cn2 row
                                                              compare_next :: Float -> Row -> [String] -> String -> Bool
                                                              compare_next el1 (el2:row) (h:hd) cn
                                                                                 | h == cn = if (el1 == (read el2)) then True else False
                                                                                 | otherwise = compare_next el1 row hd cn



instance FEval String where
  feval [] _ _ = False
  feval _ _ [] = False

  feval (h:hd) (Eq colname ref) (el:row) = if h == colname
                                           then if el == ref then True else False
                                           else feval hd (Eq colname ref) row
  
  feval (h:hd) (Lt colname ref) (el:row) = if h == colname
                                           then if el < ref then True else False
                                           else feval hd (Lt colname ref) row
  
  feval (h:hd) (Gt colname ref) (el:row) = if h == colname
                                           then if el > ref then True else False
                                           else feval hd (Gt colname ref) row

  feval (h:hd) (In colname list) (el:row) = if h == colname
                                            then search_interval el list
                                            else feval hd (In colname list) row
                                                        where search_interval el (x:xs) = if el == x
                                                                                          then True
                                                                                          else search_interval el xs
                                                              search_interval el [] = False

  feval (h:hd) (FNot (Eq colname ref)) (el:row) = if h == colname
                                                  then if el /= ref then True else False
                                                  else feval hd (FNot (Eq colname ref)) row

  feval (h:hd) (FNot (Lt colname ref)) (el:row) = if h == colname
                                                  then if el >= ref then True else False
                                                  else feval hd (FNot (Lt colname ref)) row

  feval (h:hd) (FNot (Gt colname ref)) (el:row) = if h == colname
                                                  then if el <= ref then True else False
                                                  else feval hd (FNot (Gt colname ref)) row

  feval (h:hd) (FNot (In colname list)) (el:row) = if h == colname
                                                  then search_interval el list
                                                  else feval hd (FNot (In colname list)) row
                                                        where search_interval el (x:xs) = if el == x
                                                                                          then False
                                                                                          else search_interval el xs
                                                              search_interval el [] = True

  feval (h:hd) (FieldEq cn1 cn2) (el1:row) = if h == cn1
                                             then compare_next el1 row hd cn2
                                             else if h == cn2
                                                  then compare_next el1 row hd cn1
                                                  else search_for_first_el hd cn1 cn2 row
                                                        where search_for_first_el :: [String] -> String -> String -> Row -> Bool
                                                              search_for_first_el (h:hd) cn1 cn2 (el1:row)
                                                                  | h == cn1 = compare_next el1 row hd cn2
                                                                  | h == cn2 = compare_next el1 row hd cn1
                                                                  | otherwise = search_for_first_el hd cn1 cn2 row
                                                              compare_next el1 (el2:row) (h:hd) cn
                                                                                 | h == cn = if (el1 == el2) then True else False
                                                                                 | otherwise = compare_next el1 row hd cn

-- 3.4

-- where EdgeOp is defined:
type EdgeOp = Row -> Row -> Maybe Value

-- 3.5
similarities_query :: Query
similarities_query = case (eval (Graph my_edge_op (FromTable eight_hours))) of
                          Table t -> (Sort "Value" (FromTable t))
  

-- the edge op function for the Graph
my_edge_op :: Row -> Row -> Maybe String
my_edge_op r1 r2
      | ((equal_values r1 r2) >= 5) = Just (show (equal_values r1 r2))
      | otherwise = Nothing

-- this function goes through all strings in both rows
-- and searches for the equal values on the same column
equal_values :: Row -> Row -> Int
equal_values (x:xs) (y:ys) 
    | x == y = 1 + (equal_values xs ys)
    | otherwise = equal_values xs ys
equal_values [] _ = 0
equal_values _ [] = 0

-- 3.6 (Typos)

-- this function computes the distance between two strings
my_distance :: (Eq a) => [a] -> [a] -> Int
my_distance str1 str2 = dist_aux m n
  where (m, n) = (length str1, length str2)
        dist_aux i 0 = i
        dist_aux 0 j = j
        dist_aux i j
          | str1 !! (i - 1) ==  str2 !! (j - 1) = distance ! (i - 1, j - 1)
          | otherwise = minimum [ distance ! (i - 1, j)     + 1
                                , distance ! (i, j - 1)     + 1
                                , distance ! (i - 1, j - 1) + 1
                                ]

        distance = listArray bounds
               [dist_aux i j | (i, j) <- range bounds]
        bounds = ((0, 0), (m, n))

-- this function finds the best match in a row with the given string
find_correct_name :: String -> Row -> String
find_correct_name name row = foldr (\x acc -> if (my_distance x name) < (my_distance acc name)
                                        then x
                                        else acc) [] row

-- this function replaces a certain value in a row
replace_value :: Row -> String -> String -> Row
replace_value row target val = map (\x -> if x == target
                                             then val
                                             else x) row

-- this function is looking for the value in the first table on the given column and if it is
-- equal to the value on the given column in the second table, that row is added to the table
-- otherwise, it goes and searches for the best match wih that value and that row is added to the table
search_for_the_correct_name :: String -> Table -> Table -> Table
search_for_the_correct_name val [] t2 = []
search_for_the_correct_name val t1 t2 = if (found_element (as_list val t2) ((head t1) !! (column_number (head t1) val 0))) == True 
                                        then ((head t1) : (search_for_the_correct_name val (tail t1) t2)) 
                                        else (replace_value (head t1) ((head t1) !! (column_number (head t1) val 0)) 
                                             (find_correct_name ((head t1) !! (column_number (head t1) val 0))  (as_list val t2))):
                                                                                    (search_for_the_correct_name val (tail t1) t2)
                           
-- this function adds the elements with the given index to the accumulator
as_list :: String -> Table -> Row
as_list name t = map (\row -> row !! column_number (head t) name 0) (tail t)

-- this function returns the number of the column on which a string is in a row
column_number :: Row -> String -> Int -> Int
column_number [] _ _ = 0
column_number col name index = if head col == name 
                               then index 
                               else column_number (tail col) name (index + 1)

-- this function looks for a given string in a row
found_element :: Row -> String -> Bool
found_element [] _ = False
found_element row name = if head row == name
                         then True
                         else found_element (tail row) name

correct_table :: String -> Table -> Table -> Table
correct_table val table1 table2 = (head table1) : (tail (search_for_the_correct_name val table1 table2))
