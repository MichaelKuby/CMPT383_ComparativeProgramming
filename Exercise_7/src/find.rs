pub fn find_elt<T: Eq>(values: &Vec<T>, elt: T) -> Option<usize> {
    let index: Option<usize> = values.iter().position(|x| *x == elt);

    match index {
        Some(i) => Some(i),
        None => None,
    }
}
