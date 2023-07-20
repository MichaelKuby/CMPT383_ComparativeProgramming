use criterion::{criterion_group, criterion_main, Criterion};
use exer9::primes;
use exer9::sum;
use exer9::test_helpers;

const SUM_LENGTH: usize = 1000000;

pub fn iteration_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("Summing");
    group.bench_function("sum_loop_index", |b| {
        let v = test_helpers::random_vec_i64(SUM_LENGTH);
        b.iter(|| {
            sum::sum_loop_index(&v);
        })
    });
    group.bench_function("sum_loop_iter", |b| {
        let v = test_helpers::random_vec_i64(SUM_LENGTH);
        b.iter(|| {
            sum::sum_loop_iter(&v);
        })
    });
    group.bench_function("sum_method", |b| {
        let v = test_helpers::random_vec_i64(SUM_LENGTH);
        b.iter(|| {
            sum::sum_method(&v);
        })
    });
    group.bench_function("sum_fold", |b| {
        let v = test_helpers::random_vec_i64(SUM_LENGTH);
        b.iter(|| {
            sum::sum_fold(&v);
        })
    });
    group.finish();

    let mut group = c.benchmark_group("Prime stuff");
    group.bench_function("is_prime(7919)", |b| {
        b.iter(|| {
            primes::is_prime(7919);
        })
    });
    group.bench_function("is_prime(7920)", |b| {
        b.iter(|| {
            primes::is_prime(7920);
        })
    });
    group.bench_function("is_prime(4)", |b| {
        b.iter(|| {
            primes::is_prime(4);
        })
    });

    group.finish();
}

criterion_group!(benches, iteration_benchmark);
criterion_main!(benches);
