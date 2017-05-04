/**
 * Truncate a string (first argument) if it is longer
 * than the given maximum string length (second argument).
 * Return the truncated string with a ... ending.
 */
let truncateString str n =>
  if (String.length str > n) {
    String.sub str 0 n ^ "..."
  } else {
    str
  };
