use serde::{Deserialize, Serialize};
use serde_flat_path::flat_path;

#[flat_path]
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq)]
pub struct Foo {
    #[flat_path(path=["a", "b", "c"])]
    foo: bool,
    x: u64,
    #[flat_path("d.e.f.g")]
    bar: i32,
}

#[test]
fn serialize_deserialize_struct() {
    let foo = Foo {
        foo: false,
        x: 123,
        bar: -456,
    };

    let json = serde_json::to_string(&foo).unwrap();
    assert_eq!(
        json,
        r#"{"a":{"b":{"c":false}},"x":123,"d":{"e":{"f":{"g":-456}}}}"#
    );

    let parsed: Foo = serde_json::from_str(&json).unwrap();
    assert_eq!(parsed, foo);
}
