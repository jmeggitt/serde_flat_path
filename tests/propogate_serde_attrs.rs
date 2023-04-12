use serde::{Deserialize, Serialize};
use serde_flat_path::flat_path;

#[flat_path]
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Default)]
#[serde(default)]
pub struct Foo {
    #[flat_path(path=["a", "b", "c"])]
    #[serde(with = "flip_bool")]
    foo: bool,
    #[serde(skip_serializing_if = "Option::is_some")]
    x: Option<u64>,
}

mod flip_bool {
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

    pub fn deserialize<'de, D>(deserializer: D) -> Result<bool, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(!bool::deserialize(deserializer)?)
    }

    pub fn serialize<S>(this: &bool, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        bool::serialize(&!this, serializer)
    }
}

#[test]
fn serialize_deserialize_struct() {
    let foo_initial = Foo {
        foo: false,
        x: Some(4),
    };

    let json = serde_json::to_string(&foo_initial).unwrap();
    assert_eq!(json, r#"{"a":{"b":{"c":true}}}"#);

    let foo_modified = Foo {
        foo: false,
        x: None,
    };
    let parsed: Foo = serde_json::from_str(&json).unwrap();
    assert_eq!(parsed, foo_modified);
}
