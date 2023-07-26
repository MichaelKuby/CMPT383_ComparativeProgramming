use crate::queue::{Task, WorkQueue};
use digest::consts::U32;
use sha2::digest::generic_array::GenericArray;
use sha2::{Digest, Sha256};
use std::fmt::Write;
use std::sync;
use digest::typenum::Min;

type Hash = GenericArray<u8, U32>;

#[derive(Debug, Clone)]
pub struct Block {
    prev_hash: Hash,
    generation: u64,
    difficulty: u8,
    data: String,
    proof: Option<u64>,
}

impl Block {
    pub fn initial(difficulty: u8) -> Block {
        // create and return a new initial block
        Block {
            prev_hash: Hash::default(),
            generation: 0,
            difficulty,
            data: String::new(),
            proof: None
        }
    }

    pub fn next(previous: &Block, data: String) -> Block {
        // create and return a block that could follow `previous` in the chain
        Block {
            prev_hash: Block::hash(previous),
            generation: previous.generation + 1,
            difficulty: previous.difficulty,
            data,
            proof: None,
        }
    }

    pub fn hash_string_for_proof(&self, proof: u64) -> String {
        // return the hash string this block would have if we set the proof to `proof`.
        let mut string = String::new();

        // Previous hash encoded to a string in lower case hexadecimal
        // Help from here: https://stackoverflow.com/questions/19076719/how-do-i-convert-a-vector-of-bytes-u8-to-a-string
        string += &*format!("{:x}", self.prev_hash);
        string += ":";

        // the generation
        string += &self.generation.to_string();
        string += ":";

        // the difficulty
        string += &self.difficulty.to_string();
        string += ":";

        // the data string
        string += &self.data;
        string += ":";

        // the proof of work
        string += &proof.to_string();

        string
    }

    pub fn hash_string(&self) -> String {
        // self.proof.unwrap() panics if block not mined
        let p = self.proof.unwrap();
        self.hash_string_for_proof(p)
    }

    pub fn hash_for_proof(&self, proof: u64) -> Hash {
        // return the block's hash as it would be if we set the proof to `proof`.

        // create a Sha256 object
        let mut hasher = Sha256::new();

        // String if proof set to 'proof'
        let hash_string = Block::hash_string_for_proof(&self, proof);

        // Convert to bytes
        let hash_bytes = hash_string.as_bytes();

        // write input message
        hasher.update(hash_bytes);

        hasher.finalize()
    }

    pub fn hash(&self) -> Hash {
        // self.proof.unwrap() panics if block not mined
        let p = self.proof.unwrap();
        self.hash_for_proof(p)
    }

    pub fn set_proof(self: &mut Block, proof: u64) {
        self.proof = Some(proof);
    }

    pub fn is_valid_for_proof(&self, proof: u64) -> bool {
        // would this block be valid if we set the proof to `proof`?

        // return the block's hash as it would be if we set the proof to `proof`.
        let possibly_valid = self.hash_for_proof(proof);

        let n_bytes = self.difficulty / 8;
        let n_bits = self.difficulty % 8;

        // Check that each of the last n_bytes are zero
        let end_index = possibly_valid.len();
        let start_index = end_index - (n_bytes as usize);

        for i in (start_index..end_index).rev() {
            if possibly_valid[i] != 0u8 {
                return false
            }
        }

        // Check that the next byte from the end is divisible by 1 << n_bits, i.e., 2^n_bits
        let left_shift = 1 << n_bits;
        if possibly_valid[start_index-1] % left_shift != 0 {
            return false
        }

        return true
    }

    pub fn is_valid(&self) -> bool {
        if self.proof.is_none() {
            return false;
        }
        self.is_valid_for_proof(self.proof.unwrap())
    }

    // Mine in a very simple way: check sequentially until a valid hash is found.
    // This doesn't *need* to be used in any way, but could be used to do some mining
    // before your .mine is complete. Results should be the same as .mine (but slower).
    pub fn mine_serial(self: &mut Block) {
        let mut p = 0u64;
        while !self.is_valid_for_proof(p) {
            p += 1;
        }
        self.proof = Some(p);
    }

    pub fn mine_range(self: &Block, workers: usize, start: u64, end: u64, chunks: u64) -> u64 {
        // With `workers` threads, check proof values in the given range, breaking up
	    // into `chunks` tasks in a work queue. Return the first valid proof found.
        // HINTS:
        // - Create and use a queue::WorkQueue.
        // - Use sync::Arc to wrap a clone of self for sharing.

        // - Create and use a queue::WorkQueue. Available: enqueue, recv, iter, try_recv
        let mut work_queue = WorkQueue::new(workers);

        /* println!("mine_range start: {}, end: {}, chunks: {}", start, end, chunks); // For debugging */

        /* Determine the number of tasks based on the chunk size. Uses integer division. */
        let num_tasks = ((end - start) / chunks) + 1;

        /*println!("Number of tasks: {}", num_tasks); // For debugging */

        // Create tasks and enqueue them to the work queue
        for i in 0..num_tasks {
            let current_start = start + (i * chunks);
            let current_end: u64;
            if current_start + chunks > end {
                current_end = end;
            }
            else {
                current_end = current_start + chunks;
            }
            let new_task = MiningTask::new(self, current_start, current_end);
            work_queue.enqueue(new_task).unwrap();
        }

        // Retrieve the first valid proof found from the work queue
        for proof in work_queue.iter() {
            if self.is_valid_for_proof(proof) {
                return proof;
            }
            println!("Found an invalid proof?"); // Should never happen.
        }
        0 // Represents failure.
    }

    pub fn mine_for_proof(self: &Block, workers: usize) -> u64 {
        let range_start: u64 = 0;
        let range_end: u64 = 8 * (1 << self.difficulty); // 8 * 2^(bits that must be zero)
        let chunks: u64 = 2345;
        self.mine_range(workers, range_start, range_end, chunks)
    }

    pub fn mine(self: &mut Block, workers: usize) {
        self.proof = Some(self.mine_for_proof(workers));
    }
}

/*
    A MiningTask struct represents a mining task to be fed to the worker threads. Each task
    needs a range (start and end point) to be searched for the proof of work. Note that Arc must
    take ownership over the block, hence we need to .clone() it.

    Notice that each struct has the same Block data but different range points. This construct
    allows us to share cloned Block data between multiple threads, but have them work on different
    ranges related to it.

    Why not use &Block? Since some thread may find Some(p) while others are still looking, the
    thread owning Block might receive that result, and have Block fall out of scope, before the
    others finish looking. At that point, the threads referencing Block would have outlived Block
    itself. Rust prevents this type of behaviour.
*/
struct MiningTask {
    block: sync::Arc<Block>,
    start: u64,
    end: u64,
}

/*
    Any MiningTask must implement at least a constructor: MiningTask::new(). Note that we must
    clone the block
*/
impl MiningTask {
    pub fn new(block: &Block, start: u64, end: u64) -> MiningTask {
        MiningTask {
            block: sync::Arc::new(block.clone()),
            start,
            end,
        }
    }
}

/*
    To work with WorkQueue, MiningTask must implement Task. Task has output of type u64. Tasks
    also must "run." In this context, to run means to check the given range given by [self.start,
    self.end]. Note that this range must be inclusive!
 */
impl Task for MiningTask {
    type Output = u64;
    fn run(&self) -> Option<u64> {
        // what does it mean to .run? For possible p in range, check if p is the proof
        for p in self.start..self.end+1 {
            if self.block.is_valid_for_proof(p) {
                return Some(p);
            }
        }
        None
    }
}