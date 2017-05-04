/**
 * Illustrating the variety of different
 * approaches available, can factorialise
 * in a purely imperative manner, just as in
 * JS itself, or use recursive approaches.
 */
let imperative_factorialise n => {
  let fact = ref 1;
  for i in 1 to n {
    fact.contents = fact.contents * i
  };
  fact.contents
};

let rec recursive_factorialise =
  fun
  | 0 => 1
  | n => n * recursive_factorialise (n - 1);

let tail_recursive_factorialise n => {
  let rec fac acc n =>
    if (n > 0) {
      fac (acc * n) (n - 1)
    } else {
      acc
    };
  fac 1 n
};
