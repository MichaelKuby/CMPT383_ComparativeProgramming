/*
From the results below we can see that, at least for this particular run,
there was around a 3x time cost for the use of dynamic binding during run-time.
It's pretty clear that we should only use dynamic binding if absolutely necessary!

Zero Area Checks/any_shape_zero_area
                        time:   [40.712 µs 40.842 µs 40.988 µs]
Found 7 outliers among 100 measurements (7.00%)
  5 (5.00%) high mild
  2 (2.00%) high severe
Zero Area Checks/any_circle_zero_area
                        time:   [13.093 µs 13.126 µs 13.161 µs]
Found 7 outliers among 100 measurements (7.00%)
  1 (1.00%) low mild
  2 (2.00%) high mild
  4 (4.00%) high severe
Zero Area Checks/any_rectangle_zero_area
                        time:   [16.088 µs 16.134 µs 16.177 µs]
Found 1 outliers among 100 measurements (1.00%)
  1 (1.00%) high mild
*/

pub trait Shape {
    fn area(&self) -> f64;
    fn description(&self) -> &str; // used to inspect types during testing
}

#[derive(Debug, Clone)]
pub struct Circle {
    radius: f64,
}
impl Circle {
    pub fn new(radius: f64) -> Circle {
        Circle { radius }
    }
    pub fn random() -> Circle {
        Circle {
            radius: rand::random::<f64>() + 1.0,
        }
    }
}
impl Shape for Circle {
    fn area(&self) -> f64 {
        std::f64::consts::PI * self.radius.powi(2)
    }
    fn description(&self) -> &str {
        "circle"
    }
}

#[derive(Debug, Clone)]
pub struct Rectangle {
    width: f64,
    height: f64,
}
impl Rectangle {
    pub fn new(width: f64, height: f64) -> Rectangle {
        Rectangle { width, height }
    }
    pub fn random() -> Rectangle {
        Rectangle {
            width: rand::random::<f64>() + 1.0,
            height: rand::random::<f64>() + 1.0,
        }
    }
}
impl Shape for Rectangle {
    fn area(&self) -> f64 {
        self.width * self.height
    }
    fn description(&self) -> &str {
        "rectangle"
    }
}

pub fn any_circle_zero_area(shapes: &Vec<Box<Circle>>) -> bool {
    for circle in shapes {
        if circle.radius == 0.0 {
            return true
        }
    }
    false

    /*
    shapes.iter().fold(true, | mut nonzero : bool, circle : Circle|
        nonzero && (circle.area() != 0))
    */
}

pub fn any_rectangle_zero_area(shapes: &Vec<Box<Rectangle>>) -> bool {
    for rec in shapes {
        if rec.area() == 0.0 {
            return true
        }
    }
    false
}

pub fn any_shape_zero_area(shapes: &Vec<Box<dyn Shape>>) -> bool {
    for shape in shapes {
        if shape.area() == 0.0 {
            return true
        }
    }
    false
}

pub fn make_circle_vec(n: usize) -> Vec<Box<Circle>> {
    let mut circle_vec : Vec<Box<Circle>> = Vec::with_capacity(2*n);
    for _ in 0..2*n {
        circle_vec.push(Box::new(Circle::random()));
    }
    circle_vec
}

pub fn make_rectangle_vec(n: usize) -> Vec<Box<Rectangle>> {
    let mut rec_vec : Vec<Box<Rectangle>> = Vec::with_capacity(2*n);
    for _ in 0..2*n {
        rec_vec.push(Box::new(Rectangle::random()));
    }
    rec_vec
}

pub fn make_mixed_vec(n: usize) -> Vec<Box<dyn Shape>> {
    let mut mixed_vec : Vec<Box<dyn Shape>> = Vec::with_capacity(2*n);
    for i in 0..2*n {
        if i % 2 == 0 {
            mixed_vec.push(Box::new(Circle::random()));
        }
        else {
            mixed_vec.push(Box::new(Rectangle::random()));
        }
    }
    mixed_vec
}

use std::fmt;

impl fmt::Debug for dyn Shape {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.description(), f)
    }
}