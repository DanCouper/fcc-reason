/**
 * Create a function that looks through an array (first argument)
 * and returns the first element in the array that passes a
 * truth test (second argument). If no element passes the test,
 * return undefined.
 */
let nativeFind list pred => List.find pred list;


/**
 * FIXME recursion isn't well documented, can't figure
 * out correct syntax, examples are all different.
 * This seems to work, but compiled is still doing a
 * `if(l)` for switch, and as l is a list, this aint
 * great - code works, but that seems useless??
 */
let recursiveFind list pred => {
  let rec find l =>
    switch l {
    | [] => false
    | [hd, ...tl] when pred hd => hd
    | [_, ...tl] => find tl
    };
  find list
};
