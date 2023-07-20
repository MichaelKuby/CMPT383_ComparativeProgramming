use criterion::{black_box, criterion_group, criterion_main, Criterion};
use exer7::hailstone::{hailstone_sequence_append, hailstone_sequence_prealloc};

pub fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("n=7");
    group.bench_function("hailstone_sequence_append_7", |b| {
        b.iter(|| hailstone_sequence_append(black_box(7)))
    });
    group.bench_function("hailstone_sequence_prealloc_7", |b| {
        b.iter(|| hailstone_sequence_prealloc(black_box(7)))
    });
    group.finish();
    
    let mut group = c.benchmark_group("n=162964");
    group.bench_function("hailstone_sequence_append_162964", |b| {
        b.iter(|| hailstone_sequence_append(black_box(162964)))
    });
    group.bench_function("hailstone_sequence_prealloc_162964", |b| {
        b.iter(|| hailstone_sequence_prealloc(black_box(162964)))
    });
    group.finish();

    let mut group = c.benchmark_group("n=686901248");
    group.bench_function("hailstone_sequence_append_686901248", |b| {
        b.iter(|| hailstone_sequence_append(black_box(686901248)))
    });
    group.bench_function("hailstone_sequence_prealloc_686901248", |b| {
        b.iter(|| hailstone_sequence_prealloc(black_box(686901248)))
    });
    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
