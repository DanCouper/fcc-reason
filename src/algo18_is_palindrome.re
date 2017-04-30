/**
 * Return true if the given string is a palindrome. Otherwise, return false.
 * A palindrome is a word or sentence that's spelled the same way both forward
 * and backward, ignoring punctuation, case, and spacing.
 */
let reverse s => {
  let n = String.length s;
  String.init n (fun i => s.[n - i - 1])
};

let isValidAsciiChar c => {
  let code = Char.code c;
  code >= 48 && code <= 57 || code >= 97 && code <= 122
};

let isInvalidAsciiChar c => not (isValidAsciiChar c);

let isAsciiPalindrome string => {
  let lcString = String.lowercase_ascii string;
  let rec loop (strStart, strEnd) =>
    if (strStart >= strEnd) {
      true
    } else {
      switch lcString {
      | s when isInvalidAsciiChar s.[strStart] => loop (strStart + 1, strEnd)
      | s when isInvalidAsciiChar s.[strEnd] => loop (strStart, strEnd - 1)
      | s when s.[strStart] != s.[strEnd] => false
      | _ => loop (strStart + 1, strEnd - 1)
      }
    };
  loop (0, String.length string - 1)
};
