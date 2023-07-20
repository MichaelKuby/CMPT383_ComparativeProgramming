pub fn sum_loop_index(vector: &Vec<i64>) -> i64 {
    let mut result : i64 = 0;
    for i in 0..vector.len() {
        result += vector[i];
    }
    return result;
}

pub fn sum_loop_iter(vector: &Vec<i64>) -> i64 {
    let mut result : i64 = 0;
    for val in vector {
        result += val;
    }
    return result;
}

pub fn sum_fold(vector: &Vec<i64>) -> i64 {
    let result : i64 = vector.iter().fold(0, |acc : i64, val : &i64| acc + val);
    return result;
}

pub fn sum_method(vector: &Vec<i64>) -> i64 {
    let result : i64 = vector.iter().sum();
    return result;
}
