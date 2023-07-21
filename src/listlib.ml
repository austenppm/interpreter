let rec check_duplicates = function
  | [] -> false
  | x :: xs -> List.mem x xs || check_duplicates xs
