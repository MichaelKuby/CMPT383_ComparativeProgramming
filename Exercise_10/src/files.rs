use std::fmt;
use std::fs;

#[derive(Debug, Clone)]
pub struct SummationError {
    msg: String,
}

impl std::error::Error for SummationError {}
impl fmt::Display for SummationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}
impl From<std::io::Error> for SummationError {
    fn from(e: std::io::Error) -> SummationError {
        SummationError {
            msg: format!("io::Error: {}", e),
        }
    }
}
impl From<std::num::ParseIntError> for SummationError {
    fn from(e: std::num::ParseIntError) -> SummationError {
        SummationError {
            msg: format!("ParseIntError: {}", e),
        }
    }
}

pub fn sum_file_1(file_path : &std::path::Path) -> Result<i64, SummationError> {
    // Read the contents of the file to a string as a Result<String> type
    let contents = fs::read_to_string(file_path);

    // Check if there was an error opening the file
    let lines = match contents {
        Ok(c) => c,
        Err(e) => return Err(SummationError::from(e)),
    };

    // If there was no error opening the file, parse the lines as i64 and sum
    let mut acc: i64 = 0;
    for line in lines.lines() {
        match line.parse::<i64>() {
            Ok(val) => acc += val,
            Err(e) => return Err(SummationError::from(e)),
        };
    }
    // If no errors occurred, return the sum.
    Ok(acc)
}

pub fn sum_file_2(file_path : &std::path::Path) -> Result<i64, SummationError> {
    // Read the contents of the file to a string as a Result<String> type
    let contents = fs::read_to_string(file_path)?; // If err, propagate. If Ok, unwrap.

    // Parse the lines as i64 and sum
    let mut acc: i64 = 0;
    for line in contents.lines() {
        acc += line.parse::<i64>()? // If line.parse::<i64> is an error, propagate. If Ok, unwrap.
    }
    Ok(acc)
}