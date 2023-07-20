pub mod rational;
pub mod sort;

fn main() {
    // Test Rational constructor
    let r = rational::Rational::new(-6, 8);
    // Test Display implementation
    println!("{}", r);
    // Test Rational to Float implementation
    let rational = rational::Rational::new(3, 4);
    let f: f64 = rational.into(); // Convert Rational to f64
    println!("f64 value: {}", f);  // Output: 0.75

    // Test quicksort function
    let mut v : Vec<i64> = Vec::from([5, 2, 8, 1, 9, 3, 7, 4, 6]);
    println!("Before sorting: {:?}", v);
    sort::quicksort(&mut v);
    println!("After sorting: {:?}", v);
}