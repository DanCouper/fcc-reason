let recursiveRepeatString str n => {
  let rec rrc n acc =>
    if (n < 1) {
      acc
    } else {
      rrc (n - 1) (acc ^ str)
    };
  rrc n ""
};

let imperativeRepeatString str n => {
  let acc = ref "";
  let i = ref n;
  while (i.contents > 0) {
    acc.contents = acc.contents ^ str;
    i.contents = i.contents - 1
  };
  acc.contents
};
