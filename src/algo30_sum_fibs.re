/**
 * Given a positive integer num, return the sum of all
 * odd Fibonacci numbers that are less than or equal to num.
 *
 *  The first two numbers in the Fibonacci sequence are 1 and 1.
 *  Every additional number in the sequence is the sum of
 *  the two previous numbers. The first six numbers of the
 *  Fibonacci sequence are 1, 1, 2, 3, 5 and 8.
 *
 *  For example, `sumFibs 10` should return 10 because
 *  all odd Fibonacci numbers less than or equal to 10
 *  are 1, 1, 3, and 5.
 */
let sumFibs n => {
  let rec oddFibs ::prev ::curr ::acc =>
    if (curr <= n) {
      oddFibs prev::curr curr::(curr + prev) acc::(curr mod 2 != 0 ? acc + curr : acc)
    } else {
      acc
    };
  oddFibs prev::0 curr::1 acc::0
};
