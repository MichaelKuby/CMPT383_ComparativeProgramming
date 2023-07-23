#[allow(unused_imports)]
#[cfg(test)]
use crate::dynamic::*;

#[test]
fn shape_vec_tests() {
    // each make_*_vec should produce a vector of length 2*n
    let v: Vec<Box<Circle>> = make_circle_vec(7);
    assert_eq!(v.len(), 14);

    let v: Vec<Box<Rectangle>> = make_rectangle_vec(12);
    assert_eq!(v.len(), 24);

    let v: Vec<Box<dyn Shape>> = make_mixed_vec(123);
    assert_eq!(v.len(), 246);

    // make_mixed_vec should have n Circles and n Rectangles.
    let num_rect = v.iter().filter(|s| s.description() == "rectangle").count();
    assert_eq!(num_rect, 123);
    let num_circ = v.iter().filter(|s| s.description() == "circle").count();
    assert_eq!(num_circ, 123);
}

#[test]
fn zero_area_tests() {
    // test any_circle_zero_area
    let mut v: Vec<Box<Circle>> = Vec::new();
    assert!(!any_circle_zero_area(&v));
    v.push(Box::new(Circle::new(5.0)));
    v.push(Box::new(Circle::new(2.0)));
    assert!(!any_circle_zero_area(&v));
    v.push(Box::new(Circle::new(0.0)));
    v.push(Box::new(Circle::new(2.0)));
    assert!(any_circle_zero_area(&v));

    // test any_rectangle_zero_area
    let mut v: Vec<Box<Rectangle>> = Vec::new();
    assert!(!any_rectangle_zero_area(&v));
    v.push(Box::new(Rectangle::new(5.4, 3.2)));
    v.push(Box::new(Rectangle::new(1.2, 3.4)));
    assert!(!any_rectangle_zero_area(&v));
    v.push(Box::new(Rectangle::new(5.4, 0.0)));
    v.push(Box::new(Rectangle::new(1.2, 4.8)));
    assert!(any_rectangle_zero_area(&v));

    // test any_shape_zero_area
    let mut v: Vec<Box<dyn Shape>> = Vec::new();
    assert!(!any_shape_zero_area(&v));
    v.push(Box::new(Rectangle::new(5.4, 3.2)));
    v.push(Box::new(Circle::new(1.0)));
    assert!(!any_shape_zero_area(&v));
    v.push(Box::new(Rectangle::new(0.0, 3.2)));
    v.push(Box::new(Circle::new(3.0)));
    assert!(any_shape_zero_area(&v));
}
