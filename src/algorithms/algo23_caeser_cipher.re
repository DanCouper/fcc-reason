/**
 * NOTE The following works fine, but was removed from
 * the original OCaml stdlib for being inefficient -
 * use a different way.
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

/*
 * FIXME ahem...
 *
 *   Reason # encode "GUR DHVPX OEBJA SBK WHZCF BIRE GUR YNML QBT.";
 *   - : string = "T8; QU7=5 >ROWN :OX 6U3PS OV;R T8; 4?ZY <O9."
 */
let uppercaseAsciiRot rotN str =>
  List.fold_right
    (
      fun char acc =>
        switch (Char.code char) {
        | c when c >= 65 && c <= 90 =>
          let ccode = c + rotN;
          if (ccode > 90) {
            [Char.chr (64 + 90 - ccode), ...acc]
          } else {
            [Char.chr ccode, ...acc]
          }
        | _ => [char, ...acc]
        }
    )
    (explode str)
    [] |>
  List.map (fun c => String.make 1 c) |>
  String.concat "";

let decode str => uppercaseAsciiRot 13 str;
