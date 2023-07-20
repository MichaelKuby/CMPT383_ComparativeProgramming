#[cfg(test)]
use crate::primes;

#[test]
fn iterator_test() {
    let mut fi = primes::factors_iterator(6);
    assert_eq!(fi.next(), Some(2));
    assert_eq!(fi.next(), Some(3));
    assert_eq!(fi.next(), None);
}

#[test]
fn factors_test() {
    assert_eq!(primes::factors(12), Vec::from([2, 3, 4, 6]));
    assert_eq!(primes::factors(7), Vec::from([]));
}

#[test]
fn is_prime_test() {
    assert_eq!(primes::is_prime(12), false);
    assert_eq!(primes::is_prime(7), true);
}
