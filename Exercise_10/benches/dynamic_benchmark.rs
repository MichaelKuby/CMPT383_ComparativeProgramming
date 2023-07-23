#[allow(unused_imports)]
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use exer10::dynamic::{any_circle_zero_area, any_rectangle_zero_area, any_shape_zero_area};
use exer10::dynamic::{make_circle_vec, make_mixed_vec, make_rectangle_vec};

const N: usize = 10000; // create arrays with 2*N elements for the tests

pub fn dynamic_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("Zero Area Checks");

    group.bench_function("any_shape_zero_area", |b| {
        let v = make_mixed_vec(N);
        b.iter(|| {
            assert_eq!(any_shape_zero_area(&v), false);
        })
    });
    group.bench_function("any_circle_zero_area", |b| {
        let v = make_circle_vec(N);
        b.iter(|| {
            assert_eq!(any_circle_zero_area(&v), false);
        })
    });
    group.bench_function("any_rectangle_zero_area", |b| {
        let v = make_rectangle_vec(N);
        b.iter(|| {
            assert_eq!(any_rectangle_zero_area(&v), false);
        })
    });

    group.finish();
}

criterion_group!(benches, dynamic_benchmark);
criterion_main!(benches);
