pub fn hailstone(n: u64) -> u64 {
    // TODO
    if n % 2 == 0 {
        n / 2
    }
    else {
        (3*n) + 1
    }
}

pub fn hailstone_sequence_append(n: u64) -> Vec<u64> {
    let mut vec = Vec::new();
    vec.push(n);
    let mut m = n;
    while m != 1 {
        m = hailstone(m);
        vec.push(m);
    }
    return vec
}

pub fn hailstone_sequence_prealloc(n: u64) -> Vec<u64> {
    let mut i = 1;
    let mut m = n;

    while m != 1 {
        m = hailstone(m);
        i+=1
    }

    let mut vec = Vec::with_capacity(i);
    vec.push(n);
    let mut k = n;
    while k != 1 {
        k = hailstone(k);
        vec.push(k);
    }

    vec
}

/*
The results of the benchmarking are shown below. These numbers tell an interesting story.
Beginning at n = 7, prellocating space saves almost 4x the amount of time. At n = 162964,
there is still a considerable time difference with preallocation being about twice as fast.
With n = 686901248, preallocation is still faster, but the time saved is proportionally
diminished.

These numbers seem to make sense. Since we have to compute the length of the chain before
creating the vector we are doing each computation for the hailstone sequence twice.
When n is small, this computation is extremely fast relative to the cost of allocating
more space for the vector. As n grows, the payoff begins to diminish; however, it is
still meaningful.

Results of the benchmarking:

n=7/hailstone_sequence_append_7
                        time:   [388.92 ns 391.09 ns 393.41 ns]
Found 11 outliers among 100 measurements (11.00%)
  7 (7.00%) high mild
  4 (4.00%) high severe
n=7/hailstone_sequence_prealloc_7
                        time:   [100.63 ns 101.09 ns 101.58 ns]
Found 10 outliers among 100 measurements (10.00%)
  9 (9.00%) high mild
  1 (1.00%) high severe

n=162964/hailstone_sequence_append_162964
                        time:   [592.60 ns 595.81 ns 599.53 ns]
Found 6 outliers among 100 measurements (6.00%)
  4 (4.00%) high mild
  2 (2.00%) high severe
n=162964/hailstone_sequence_prealloc_162964
                        time:   [224.68 ns 225.33 ns 226.09 ns]
Found 4 outliers among 100 measurements (4.00%)
  2 (2.00%) high mild
  2 (2.00%) high severe

n=686901248/hailstone_sequence_append_686901248
                        time:   [1.4304 µs 1.4368 µs 1.4435 µs] (1436.8 ns)
Found 3 outliers among 100 measurements (3.00%)
  2 (2.00%) high mild
  1 (1.00%) high severe
n=686901248/hailstone_sequence_prealloc_686901248
                        time:   [934.77 ns 937.18 ns 939.99 ns]
Found 6 outliers among 100 measurements (6.00%)
  1 (1.00%) low mild
  4 (4.00%) high mild
  1 (1.00%) high severe

 */