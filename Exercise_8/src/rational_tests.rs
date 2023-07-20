#[cfg(test)]
use crate::rational::Rational;

#[test]
fn rational_test() {
    // Culled from exer7
    let mut r = Rational::new(6, 8);
    assert_eq!(format!("{:?}", r), "Rational { n: 6, d: 8 }");
    r.reduce();
    assert_eq!(format!("{:?}", r), "Rational { n: 3, d: 4 }");
    let four1 = Rational::from(4);
    let four2 = Rational::new(4, 1);
    assert_eq!(four1, four2);

    // New, based on exer7 examples
    let mut rat = Rational::new(6, 8);
    assert_eq!(format!("{}", rat), "6/8");
    rat.reduce();
    assert_eq!(format!("{}", rat), "3/4");

    let float0 = Rational::new(5,4);
    let float1 : f64 = float0.into();
    let float2 : f64 = 1.25;
    assert_eq!(float1, float2);
}