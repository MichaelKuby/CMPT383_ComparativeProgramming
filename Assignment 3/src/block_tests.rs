use super::*;
use crate::block::Block;
use std::time::Instant;

#[cfg(test)]

#[test]
fn hash_string_for_proof_tests() {
    // From Assign Instructions
    let mut b0 = Block::initial(16);
    b0.set_proof(56231);
    assert_eq!(b0.hash_string_for_proof(56231), "0000000000000000000000000000000000000000000000000000000000000000:0:16::56231");

    // Assert "not equal" when proof is changed
    assert_ne!(b0.hash_string_for_proof(12345), "0000000000000000000000000000000000000000000000000000000000000000:0:16::56231");

    let mut b1 = Block::next(&b0, String::from("message"));
    b1.set_proof(2159);
    assert_eq!(b1.hash_string_for_proof(2159), "6c71ff02a08a22309b7dbbcee45d291d4ce955caa32031c50d941e3e9dbd0000:1:16:message:2159");

    // Assert "not equal" when proof is changed
    assert_ne!(b1.hash_string_for_proof(7890), "6c71ff02a08a22309b7dbbcee45d291d4ce955caa32031c50d941e3e9dbd0000:1:16:message:2159");
}

#[test]
fn is_valid_for_proof_tests() {
        let difficulty = 16;
        let mut b0 = Block::initial(difficulty);
        let proof = 56231;
        b0.set_proof(proof);
        assert!(b0.is_valid_for_proof(proof));

        // Try an invalid proof that doesn't meet the difficulty criteria
        let invalid_proof = 12345;
        assert!(!b0.is_valid_for_proof(invalid_proof));
}

#[test]
fn mine_range_speed_test() {
    println!("Will be testing concurrency speedup. Just a minute.");
    let range = Vec::from([1, 2, 4, 8]);
    for num in range {
        let start_time = Instant::now();
        mine_range_tests(num);
        let end_time = Instant::now();

        // Time diff
        let time_elapsed = end_time.duration_since(start_time);

        // Print the duration. To see output use cargo test -- --nocapture
        println!("With num workers = {}: Time elapsed = {:?}", num, time_elapsed)

        /*
        With num workers = 1: Time elapsed = 57.342124633s
        With num workers = 2: Time elapsed = 28.790248095s
        With num workers = 4: Time elapsed = 15.815109772s
        With num workers = 8: Time elapsed = 16.055606759s

        On my machine, no speedup from concurrent execution after 4 threads. Loss of speed from
        synchronization slowdown at 8 threads. But we can achieve a near 4x speedup.
        */
    }
}

fn mine_range_tests(num: usize) {
    // Code to match the assignment example

    let num_workers = num;
    let difficulty : u8 = 20;

    // Test mining for the first block
    let mut b0 = Block::initial(difficulty);
    b0.mine(num_workers);
    // println!("{}", b0.hash_string());
    // println!("{:02x}", b0.hash());
    assert_eq!(b0.hash_string(), "0000000000000000000000000000000000000000000000000000000000000000:0:20::1209938");
    assert_eq!(format!("{:02x}", b0.hash()), "19e2d3b3f0e2ebda3891979d76f957a5d51e1ba0b43f4296d8fb37c470600000");

    // Test mining for the second block
    let mut b1 = Block::next(&b0, String::from("this is an interesting message"));
    b1.mine(num_workers);
    // println!("{}", b1.hash_string());
    // println!("{:02x}", b1.hash());
    assert_eq!(b1.hash_string(), "19e2d3b3f0e2ebda3891979d76f957a5d51e1ba0b43f4296d8fb37c470600000:1:20:this is an interesting message:989099");
    assert_eq!(format!("{:02x}", b1.hash()), "a42b7e319ee2dee845f1eb842c31dac60a94c04432319638ec1b9f989d000000");

    // Test mining for the third block
    let mut b2 = Block::next(&b1, String::from("this is not interesting"));
    b2.mine(num_workers);
    // println!("{}", b2.hash_string());
    // println!("{:02x}", b2.hash());
    assert_eq!(b2.hash_string(), "a42b7e319ee2dee845f1eb842c31dac60a94c04432319638ec1b9f989d000000:2:20:this is not interesting:1017262");
    assert_eq!(format!("{:02x}", b2.hash()), "6c589f7a3d2df217fdb39cd969006bc8651a0a3251ffb50470cbc9a0e4d00000");
}