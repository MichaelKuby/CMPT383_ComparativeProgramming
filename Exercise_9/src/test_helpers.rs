use rand::Rng;

const VAL_RANGE: i64 = 10000;

pub fn random_vec_i64(n: usize) -> Vec<i64> {
    let mut rng = rand::thread_rng();
    (0..n)
        .into_iter()
        .map(|_| rng.gen_range(-VAL_RANGE..VAL_RANGE))
        .collect()
}

pub fn random_vec_f64(n: usize) -> Vec<f64> {
    let mut rng = rand::thread_rng();
    (0..n)
        .into_iter()
        .map(|_| rng.gen_range(0.0..1.0))
        .collect()
}
