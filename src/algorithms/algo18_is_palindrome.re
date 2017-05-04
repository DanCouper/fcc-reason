/**
 * Return true if the given string is a palindrome. Otherwise, return false.
 * A palindrome is a word or sentence that's spelled the same way both forward
 * and backward, ignoring punctuation, case, and spacing.
 */

/**
 * Cannot figure out how to leverage JS' regex, so instead just check for
 * chars between `0` and `9`/`a` and `z` inclusive.
 */
let isValidAsciiChar c => {
  let code = Char.code c;
  code >= 48 && code <= 57 || code >= 97 && code <= 122
};

let isIgnorableAsciiChar c => not (isValidAsciiChar c);


/**
 * This all seems very verbose, but it does the job:
 * Just recusively loop over the string comparing start
 * and end characters, moving inwards by one each iteration.
 * Ignore any that are not allowed ascii characters.
 * This limits it to Latin-1, but hey ho.
 * If can get to a point where the index of the start and end
 * are either the same (on a pivot point), or start > end
 * (the two character in the centre are the same), can
 * say that the string is a palindrome.
 */
let isAsciiPalindrome string => {
  let lcString = String.lowercase string;
  let rec loop (startIndex, endIndex) =>
    if (startIndex >= endIndex) {
      true
    } else {
      switch lcString {
      | s when isIgnorableAsciiChar s.[startIndex] => loop (startIndex + 1, endIndex)
      | s when isIgnorableAsciiChar s.[endIndex] => loop (startIndex, endIndex - 1)
      | s when s.[startIndex] != s.[endIndex] => false
      | _ => loop (startIndex + 1, endIndex - 1)
      }
    };
  loop (0, String.length string - 1)
};
