// This test looks for an attribute macro `better_impls`
//
//

use better_impl::*;

#[better_impls]
fn foo() -> Option<i32> {
    Some(8)
}

fn main() {}
