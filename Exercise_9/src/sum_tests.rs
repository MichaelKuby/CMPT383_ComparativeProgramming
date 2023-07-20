#[cfg(test)]
use crate::sum;

#[test]
fn sum_test() {
    let v1 = Vec::from([1, 2, 3, 4, 5]);
    assert_eq!(sum::sum_loop_index(&v1), 15);
    assert_eq!(sum::sum_loop_iter(&v1), 15);
    assert_eq!(sum::sum_method(&v1), 15);
    assert_eq!(sum::sum_fold(&v1), 15);
}
