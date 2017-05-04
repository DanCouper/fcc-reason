/**
 * Sum all the prime numbers up to and including the provided number.
 *
 * A prime number is defined as a number greater than one
 * and having only two divisors, one and itself. For example,
 * 2 is a prime number because it's only divisible by one and two.
 *
 * The provided number may not be a prime.
 */

/**
 * Checks a number is prime by recursively
 * testing against an increasing counter.
 * NOTE unoptimised brute-force.
 * NOTE this assumes a sane initial
 * value for the counter (i.e. 3)
 */
let rec isPrime' n counter =>
  switch n {
  | n when n < counter * counter => true
  | n when n mod counter == 0 => false
  | _ => isPrime' n (counter + 2)
  };


/**
 * Simple check for primality that
 * calls the helper once an initial
 * set of checks have been made.
 */
let isPrime n =>
  switch n {
  | 2
  | 3
  | 5
  | 7 => true
  | n when n < 2 || n mod 2 == 0 => false
  | _ => isPrime' n 3
  };


/**
 * Brute-force prime generator. Just iterates
 * a counter and checks if the number is a prime
 * on each iteration.
 */
let sumPrimes n => {
  let rec primesUpTo ::counter ::acc =>
    if (counter > n) {
      acc
    } else {
      primesUpTo counter::(counter + 1) acc::(isPrime counter ? acc + counter : acc)
    };
  primesUpTo counter::0 acc::0
};
