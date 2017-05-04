/**
 * Check if a string (first argument, str) ends
 * with the given target string (second argument, target).
 */
let confirmEnding str target => {
  let targetIndex = String.length str - String.length target;
  target == String.sub str targetIndex (String.length target)
};
