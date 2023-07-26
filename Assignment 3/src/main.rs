use crate::block::Block;
use std::fmt::Write;
mod queue;
mod block;

mod block_tests;
mod queue_tests;

use std::sync;

fn main() {

    let num_workers = 20;

    // Test mining for the first block
    let mut b0 = Block::initial(20);
    b0.mine(num_workers);
    println!("{}", b0.hash_string());
    println!("{:02x}", b0.hash());
    assert_eq!(b0.hash_string(), "0000000000000000000000000000000000000000000000000000000000000000:0:20::1209938");
    assert_eq!(format!("{:02x}", b0.hash()), "19e2d3b3f0e2ebda3891979d76f957a5d51e1ba0b43f4296d8fb37c470600000");

    // Test mining for the second block
    let mut b1 = Block::next(&b0, String::from("this is an interesting message"));
    b1.mine(num_workers);
    println!("{}", b1.hash_string());
    println!("{:02x}", b1.hash());
    assert_eq!(b1.hash_string(), "19e2d3b3f0e2ebda3891979d76f957a5d51e1ba0b43f4296d8fb37c470600000:1:20:this is an interesting message:989099");
    assert_eq!(format!("{:02x}", b1.hash()), "a42b7e319ee2dee845f1eb842c31dac60a94c04432319638ec1b9f989d000000");

    // Test mining for the third block
    let mut b2 = Block::next(&b1, String::from("this is not interesting"));
    b2.mine(num_workers);
    println!("{}", b2.hash_string());
    println!("{:02x}", b2.hash());
    assert_eq!(b2.hash_string(), "a42b7e319ee2dee845f1eb842c31dac60a94c04432319638ec1b9f989d000000:2:20:this is not interesting:1017262");
    assert_eq!(format!("{:02x}", b2.hash()), "6c589f7a3d2df217fdb39cd969006bc8651a0a3251ffb50470cbc9a0e4d00000");
}
