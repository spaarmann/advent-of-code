use aoc2021::day6;
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};

pub fn criterion_benchmark(c: &mut Criterion) {
    let input = std::fs::read_to_string("input/day6").expect("failed to read input file");

    let mut group = c.benchmark_group("lanternfish");
    for days in [32, 64, 80, 100, 128, 150, 200, 256, 300, 350, 400, 440] {
        group.bench_with_input(BenchmarkId::new("Iterative", days), &days, |b, days| {
            b.iter(|| day6::simulate_fish_iterative(&input, *days))
        });
        group.bench_with_input(
            BenchmarkId::new("Linear Algebra", days),
            &days,
            |b, days| b.iter(|| day6::simulate_fish_linalg(&input, *days)),
        );
    }
    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
