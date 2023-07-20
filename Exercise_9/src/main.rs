#[allow(unused_imports)]
#[allow(dead_code)]
//pub mod inventory;
//pub mod primes;
pub mod sum;

fn main() {
    let n = 200;
    let vec : Vec<i64> = (1..=n).collect();
    let sum = sum::sum_loop_index(&vec);
    println!("{:?}", sum);

    let sum2 : i64 = sum::sum_loop_iter(&vec);
    println!("{:?}", sum2);

    let sum3: i64 = sum::sum_fold(&vec);
    println!("{:?}", sum3);

    let sum4: i64 = sum::sum_method(&vec);
    println!("{:?}", sum4);
}
