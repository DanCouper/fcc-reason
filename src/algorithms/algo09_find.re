/**
 * Create a function that looks through an array (first argument)
 * and returns the first element in the array that passes a
 * truth test (second argument). If no element passes the test,
 * return undefined.
 */
let nativeFind list pred => List.find pred list;

let recursiveFind list pred => {
  let rec find l =>
    switch l {
    | [] => false
    | [hd, ...tl] when pred hd => hd
    | [_, ...tl] => find tl
    };
  find list
};
