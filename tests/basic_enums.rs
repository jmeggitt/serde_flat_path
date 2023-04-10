use serde::{Deserialize, Serialize};
use serde_flat_path::flat_path;

#[flat_path]
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq)]
pub enum Foo {
    Abc {
        #[flat_path(path=["a", "b", "c"])]
        foo: i32,
        x: u64,
    },
}

#[test]
fn named_field_variant() {
    let foo = Foo::Abc { foo: 123, x: 999 };

    let json = serde_json::to_string(&foo).unwrap();
    assert_eq!(json, r#"{"Abc":{"a":{"b":{"c":123}},"x":999}}"#);

    let parsed: Foo = serde_json::from_str(&json).unwrap();
    assert_eq!(parsed, foo);
}
