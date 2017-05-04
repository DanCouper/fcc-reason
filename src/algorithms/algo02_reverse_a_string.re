/**
 * Like JS, there aint a string reversal function.
 * In addition, the in-place string manipulation
 * function (`String.set`) is deprecated, and only
 * turned on for [out-of-the-box] OCaml compilers
 * for backwards compat reasons.
 *
 * Various ways of doing this - in this case,
 * `String.mapi` maps the characters and returns a
 * new string, so simple enough to map over
 * characters from the end.
 */
let reverseString str => {
  let lastIndex = String.length str - 1;
  String.mapi (fun i _ => str.[lastIndex - i]) str
};
