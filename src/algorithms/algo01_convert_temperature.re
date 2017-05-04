/**
 *  Note that OCaml is _strict_ regarding non-mixing
 *  of ints/floats.
 *  Both of these funcs accept and return floats:
 *  note the `.` used for the mathematical operators.
 *
 *  TODO build this as a test of module - various
 *  functions to take numbers and convert them.
 */
let convertCtoF c => c *. 1.8 +. 32.;

let convertFtoC f => (f -. 32.) /. 1.8;
