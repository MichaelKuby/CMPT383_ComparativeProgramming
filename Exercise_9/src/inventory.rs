#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct InventoryItem {
    count: u64,
    cost: f64,
    description: String,
}

impl InventoryItem {
    pub fn new(count: u64, cost: f64, description: String) -> InventoryItem {
        InventoryItem {
            count,
            cost,
            description,
        }
    }
}

impl Eq for InventoryItem {
    // f64 isn't technically fully orderable, but we'll live with it.
}
impl Ord for InventoryItem {
    // implement Ord so they can go in a BTreeSet
    fn cmp(&self, other: &InventoryItem) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}
impl std::hash::Hash for InventoryItem {
    // implement Hash so they can go in a HashSet
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.count.hash(state);
        self.description.hash(state);
    }
}

pub fn total_value<'a>(items: impl IntoIterator<Item = &'a InventoryItem>) -> f64 {
    // The sum of the count times the cost of each item
    let acc : f64 = items
                        .into_iter()
                        .fold(0.0, |acc : f64, item| acc + item.count as f64 * item.cost);
    acc
}

pub fn out_of_stock<'a>(items: impl IntoIterator<Item = &'a InventoryItem>) -> Vec<InventoryItem> {
    // Create and return a vector of the items that have count 0.
    items.into_iter().filter(|item| item.count == 0).cloned().collect()
}

pub fn explode<'a>(items: impl IntoIterator<Item = &'a InventoryItem>) -> Vec<InventoryItem> {
    items.into_iter().flat_map(|item| {
        let mut exploded_items = Vec::new();
        for _ in 0..item.count {
            exploded_items.push(InventoryItem::new(1, item.cost, item.description.clone()));
        }
        exploded_items.into_iter()
    }).collect()
}

/*
pub fn explode<'a>(items: impl IntoIterator<Item = &'a InventoryItem>) -> Vec<InventoryItem> {
    items.into_iter().flat_map(|item| std::iter::repeat_with(|| InventoryItem::new(1, item.cost, item.description.clone())).take(item.count as usize)).collect()
}
*/

/* This is the equivalent and what we are trying to recreate:

pub fn explode<'a>(items: impl IntoIterator<Item = &'a InventoryItem>) -> Vec<InventoryItem> {
    let mut result = Vec::new();

    for item in items {
        for _ in 0..item.count {
            result.push(InventoryItem::new(1, item.cost, item.description));
        }
    }

    result
}
*/
