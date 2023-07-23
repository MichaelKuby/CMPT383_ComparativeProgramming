pub fn factors_iterator(n: u64) -> impl Iterator<Item = u64> {
    (2..=n / 2).filter(move |i| n % i == 0)
}

pub fn factors(n: u64) -> Vec<u64> {
    factors_iterator(n).collect()
}

pub fn is_prime(n: u64) -> bool {
    factors_iterator(n).next().is_none()
}