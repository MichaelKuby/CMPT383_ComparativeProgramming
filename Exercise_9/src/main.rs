#[allow(unused_imports)]
#[allow(dead_code)]
pub mod inventory;
pub mod primes;
pub mod sum;

use inventory::*;

fn main() {
    let items = vec![
        InventoryItem::new(3, 1.0, "apple".to_string()),
        InventoryItem::new(0, 2.0, "banana".to_string()),
        InventoryItem::new(2, 3.0, "cherry".to_string()),
    ];

    let total = total_value(&items);
    println!("Total value: {}", total);

    let out_of_stock_items = out_of_stock(&items);
    println!("Out of stock items: {:?}", out_of_stock_items);

    let exploded_items = explode(&items);
    println!("Exploded items: {:?}", exploded_items);
}

