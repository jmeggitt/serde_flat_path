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
    Ghi(i32),
    Def {
        #[flat_path("d.e.f.g")]
        bar: i32,
    },
    Jkl,
    Mno {
        z: bool,
    },
}

macro_rules! serde_test {
    ($name:ident, $foo:expr, $json:expr) => {
        // Use module as alternative to concat_idents!
        mod $name {
            use super::*;

            #[test]
            fn perform_serialize() {
                let json = serde_json::to_string(&$foo).unwrap();
                assert_eq!(json, $json);
            }

            #[test]
            fn perform_deserialize() {
                let parsed: Foo = serde_json::from_str($json).unwrap();
                assert_eq!(parsed, $foo);
            }
        }
    };
}

serde_test! {
    named_field_variant_abc,
    Foo::Abc { foo: 123, x: 999 },
    r#"{"Abc":{"a":{"b":{"c":123}},"x":999}}"#
}

serde_test! {
    named_field_variant_def,
    Foo::Def { bar: 987 },
    r#"{"Def":{"d":{"e":{"f":{"g":987}}}}}"#
}

serde_test! {
    tuple_variant,
    Foo::Ghi(-654),
    r#"{"Ghi":-654}"#
}

serde_test! {
    unit_variant,
    Foo::Jkl,
    r#""Jkl""#
}

serde_test! {
    named_field_variant_no_flat_path,
    Foo::Mno { z: false, },
    r#"{"Mno":{"z":false}}"#
}
