let exchange k =
  let a = k / 10 in
  let b = k mod 10 in
  b * 10 + a

let is_valid_answer (grand_father_age, grand_son_age) =
  grand_son_age * 4 = grand_father_age &&
  (exchange grand_son_age) = (exchange grand_father_age) * 3

let rec find (max_grand_father_age, min_grand_son_age) = 
  if min_grand_son_age * 4 > max_grand_father_age || 
     (max_grand_father_age < 10 && max_grand_father_age >= 100) ||
     (min_grand_son_age < 10 && min_grand_son_age >= 100)
  then (-1, -1)
  else if is_valid_answer (min_grand_son_age * 4, min_grand_son_age)
  then (min_grand_son_age * 4, min_grand_son_age)
  else find (max_grand_father_age, min_grand_son_age + 1)

