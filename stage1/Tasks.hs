-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]


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