/**
 * You will be provided with an initial array (the first argument in the
 * `destroyer` function), followed by a list of one or more target values.
 * Remove all elements from the initial array that are of the same value
 * as these targets.
 *
 * NOTE this differs from the original implementation, which used
 * `n` arguments as the targets. This is impossible to replicate in
 * OCaml, and seems a bad idea anyway, so a list is used (which makes
 * it functionally the same as algo 17 [diff a list pair]).
 */

/**
 * NOTE using functions ported from `Containers` library;
 * (see [https://github.com/c-cube/ocaml-containers]).
 *
 * Then defining an infix function for one-way diffing.
 */
let uniq ::eq=(==) l => {
  let rec uniq eq acc l =>
    switch l {
    | [] => List.rev acc
    | [x, ...xs] when List.exists (eq x) xs => uniq eq acc xs
    | [x, ...xs] => uniq eq [x, ...acc] xs
    };
  uniq eq [] l
};

let (-@) a b => uniq (List.filter (fun v => not (List.mem v b)) a);

let destroyer lst targets => lst -@ targets;
