type date =
  { year : int; month : int; day : int;
    hour : int; minute : int }

let the_origin_of_time =
  { year = 1; month = 1; day = 1;
    hour = 0; minute = 0 }

let wellformed date =
  let { year ; month ; day ; hour ; minute } = date in 
  year >= 1 && 
  month >= 1 && month <= 5 &&
  day >= 1 && day <= 4 && 
  hour >= 0 && hour <= 2 &&
  minute >= 0 && minute <= 1
  
let next date =
  let { year ; month ; day ; hour ; minute } = date in
  let minute = minute + 1 in 
  if minute > 1 then
    let minute = 0 in
    let hour = hour + 1 in
    if hour > 2 then
      let hour = 0 in
      let day = day + 1 in
      if day > 4 then
        let day = 1 in
        let month = month + 1 in
        if month > 5 then
          let month = 1 in
          let year = year + 1 in
          { year ; month ; day ; hour ; minute }
        else
          { year ; month ; day ; hour ; minute }
      else 
        { year ; month ; day ; hour ; minute }
    else
      { year ; month ; day ; hour ; minute }
  else
    { year ; month ; day ; hour ; minute }
  

let of_int minutes =
  let rec aux i d =
    if i = 0 
    then d
    else
      aux (i - 1) (next d)
  in
  aux minutes the_origin_of_time