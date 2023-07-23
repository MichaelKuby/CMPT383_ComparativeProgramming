mod dynamic;
mod files;

use files::SummationError;
use std::path::Path;

fn test_each_file(sum_file: fn(&Path) -> Result<i64, SummationError>) {
    println!("Success: {:?}", sum_file(Path::new("good.txt")));
    println!(
        "No such file or directory: {:?}",
        sum_file(Path::new("missing_file.txt"))
    );
    println!(
        "Did not contain valid UTF-8: {:?}",
        sum_file(Path::new("bad_utf8.txt"))
    );
    println!(
        "Invalid digit found: {:?}",
        sum_file(Path::new("bad_number.txt"))
    );
}

fn main() {
    test_each_file(files::sum_file_1);
    //test_each_file(files::sum_file_2);
}
