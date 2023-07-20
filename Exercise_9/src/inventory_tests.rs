#[cfg(test)]
use crate::inventory;

#[test]
fn inventory_test() {
    let items_vec = Vec::from([
        inventory::InventoryItem::new(5, 9.99, "Widget".into()),
        inventory::InventoryItem::new(2, 199.99, "Gold-Crusted Widget".into()),
        inventory::InventoryItem::new(0, 19.99, "Unobtanium".into()),
    ]);
    
    // convert to some other iterable collections, just to prove it works.
    let items_ll: std::collections::LinkedList<inventory::InventoryItem> =
        items_vec.iter().cloned().collect();
    let items_bt: std::collections::BTreeSet<inventory::InventoryItem> =
        items_vec.iter().cloned().collect();
    let items_hs: std::collections::HashSet<inventory::InventoryItem> =
        items_vec.iter().cloned().collect();

    // The expected result from .explode() below
    let items_exploded = Vec::from([
        inventory::InventoryItem::new(1, 9.99, "Widget".into()),
        inventory::InventoryItem::new(1, 9.99, "Widget".into()),
        inventory::InventoryItem::new(1, 9.99, "Widget".into()),
        inventory::InventoryItem::new(1, 9.99, "Widget".into()),
        inventory::InventoryItem::new(1, 9.99, "Widget".into()),
        inventory::InventoryItem::new(1, 199.99, "Gold-Crusted Widget".into()),
        inventory::InventoryItem::new(1, 199.99, "Gold-Crusted Widget".into()),
    ]);

    assert_eq!(inventory::total_value(&items_vec), 449.93);
    assert_eq!(inventory::total_value(&items_ll), 449.93);
    assert_eq!(inventory::total_value(&items_bt), 449.93);
    assert_eq!(inventory::total_value(&items_hs), 449.93);

    assert_eq!(
        inventory::out_of_stock(&items_vec),
        Vec::from([items_vec[2].clone()])
    );

    let mut exploded_result = inventory::explode(&items_vec);
    exploded_result.sort();
    assert_eq!(items_exploded, exploded_result);
}
