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

/*
Benchmarking results

Summing/sum_loop_index  time:   [369.47 µs 392.23 µs 427.82 µs]
Found 4 outliers among 100 measurements (4.00%)
  2 (2.00%) high mild
  2 (2.00%) high severe

Summing/sum_loop_iter   time:   [342.42 µs 347.96 µs 355.11 µs]
Found 3 outliers among 100 measurements (3.00%)
  1 (1.00%) high mild
  2 (2.00%) high severe

Summing/sum_method      time:   [375.88 µs 412.90 µs 461.65 µs]
Found 8 outliers among 100 measurements (8.00%)
  3 (3.00%) high mild
  5 (5.00%) high severe

Summing/sum_fold        time:   [370.68 µs 377.27 µs 384.23 µs]
Found 4 outliers among 100 measurements (4.00%)
  4 (4.00%) high mild

Comments: We see comparable times across all implementations. Interestingly,
sum_loop_iter outperforms every other implementation, having the fastest run time
overall, the fastest average run time, as well as the fastest of the longest
run times. C-style summing over the index does not perform terribly but is
measurably slower than sum_loop_iter and sum_fold both of which use iterator
approaches.

We can say with a high degree of confidence than using an iterator to perform
such a task is a fast and efficient approach.
*/


