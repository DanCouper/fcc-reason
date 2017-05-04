/**
 * Given an integer, split it into a list
 * of integers in the same order.
 *
 *    Reason # digits 2025
 *    - : list int = [2, 0, 2, 5]
 */
let digits d => {
  let rec digitLoop acc d =>
    if (d < 10) {
      [d, ...acc]
    } else {
      digitLoop [d mod 10, ...acc] (d / 10)
    };
  digitLoop [] d
};


/**
 * In this case, easier for indexing if digits
 * are processed in reverse.
 */
let revDigits d => List.rev (digits d);


/**
 * Just use a 2D array for quick index-based access
 * of numerals.
 */
let numeralGroups = [|
  [|"", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"|],
  [|"", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"|],
  [|"", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"|],
  [|"", "M", "MM", "MMM"|]
|];

let convertTo number =>
  revDigits number |> List.mapi (fun i n => numeralGroups.(i).(n)) |> String.concat "";

/* ----------------------------------------------- */

/**
 * Given a string, 'explode' to individual characters.
 * Nicely-named PHP function, better than `stringToList` :shrug:
 */
let explode str => {
  let rec exp a b =>
    if (a < 0) {
      b
    } else {
      exp (a - 1) [str.[a], ...b]
    };
  exp (String.length str - 1) []
};

let numeralValue char =>
  switch char {
  | 'M' => 1000
  | 'D' => 500
  | 'C' => 100
  | 'L' => 50
  | 'X' => 10
  | 'V' => 5
  | 'I' => 1
  };


/**
 * Not my own work, adapted from Elixir/Erlang solution on Rosetta Code.
 */
let convertFrom numerals => {
  let rec converter nums =>
    switch nums {
    | [] => 0
    | [x] => numeralValue x
    | [x, y, ...rest] =>
      switch (numeralValue x, numeralValue y) {
      | (a, b) when a < b => b - a + converter rest /* e.g. IV, a:I, b:V, 5 - 1 */
      | (a, b) when a == b => a + b + converter rest
      | (a, _) => a + converter [y, ...rest]
      }
    };
  converter (explode numerals)
};
