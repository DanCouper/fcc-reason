/**
 * Return the lowest index at which a value (second argument)
 * should be inserted into a numeric list (first argument) once
 * the list has been sorted.
 */
let getIndexToInsert list n => List.length (List.filter (fun v => v < n) list);

let recIndexToInsert list n => {
  let rec indexToInsert l counter =>
    switch l {
    | [] => counter
    | [x, ...xs] when x < n => indexToInsert xs (counter + 1)
    | [_, ...xs] => indexToInsert xs counter
    };
  indexToInsert list 0
};
