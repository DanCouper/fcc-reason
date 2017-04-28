let confirmEnding str target => {
  let targetIndex = String.length str - String.length target;
  target == String.sub str targetIndex (String.length target)
};
