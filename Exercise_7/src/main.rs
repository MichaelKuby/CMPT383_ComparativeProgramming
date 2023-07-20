use crate::rational::Rational;

    pub mod find;
    pub mod hailstone;
    pub mod rational;

    fn main() {
        // Check to see if hailstone works
        let n: u64 = 15;
        let result = hailstone::hailstone(n);
        println!("{}", result);

        // Check to see if hailstone_sequence_append works
        let m = 2;
        let vec = hailstone::hailstone_sequence_append(m);
        println!("{:?}", vec);

        // Check to see if hailstone_sequence_prealloc works
        let vec2 = hailstone::hailstone_sequence_prealloc(m);
        println!("{:?}", vec2);

        // Test find_elt
        let v1: Vec<i32> = Vec::from([4, 5, 2, 8, 7, 3, 1]);
        println!("{:?}", find::find_elt(&v1, 8)); // Some(3)
        println!("{:?}", find::find_elt(&v1, 6)); // None
        let v2: Vec<char> = "Hello World!".chars().collect();
        println!("{:?}", find::find_elt(&v2, 'o')); // Some(4)
        println!("{:?}", find::find_elt(&v2, 'q')); // None

        /*
        // Test gcd function
        let a : i64 = 270;
        let b : i64 = 192;
        let gcdiv : i64 = rational::gcd(a, b);
        println!("{:?}", gcdiv)
         */

        // Test Rational constructor
        let mut r: Rational = rational::Rational::new(6, 8);
        println!("{:?}", r);

        // Test Reduce method
        r.reduce();
        println!("{:?}", r);

        // Test from constructor method
        let n = Rational::from(4_i64);
        println!("{:?}", n); // prints Rational { n: 4, d: 1 }
        println!("{}", n == Rational::new(4, 1)); // prints true
    }