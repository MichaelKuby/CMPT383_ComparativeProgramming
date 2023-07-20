use std::fmt;

fn gcd(a: i64, b: i64) -> i64 {
    if a == 0 {
        return b;
    }
    if b == 0{
        return a;
    }
    let r : i64 = a % b;

    return gcd(b, r);
}

#[derive(Debug, Clone, PartialEq)]
pub struct Rational {
    pub n: i64,
    pub d: i64,
}


impl Rational {
    pub fn new(n: i64, d: i64) -> Rational {
        Rational {
            n,
            d,
        }
    }
    pub fn reduce(&mut self) {
        let gc_div: i64 = gcd(self.n, self.d);
        self.n = self.n / gc_div;
        self.d = self.d / gc_div;
    }
}

impl fmt::Display for Rational {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}/{}", self.n, self.d)
    }
}
impl From<i64> for Rational {
    fn from(int: i64) -> Self {
        Rational::new(int, 1)
    }
}

impl From<Rational> for f64 {
    fn from(value: Rational) -> f64 {
        value.n as f64 / value.d as f64
    }
}