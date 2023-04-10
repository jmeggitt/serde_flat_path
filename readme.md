# `serde_flat_path`
This crate was designed to solve the problem of needing to manually create placeholder structs or untyped values (such
as `serde_json::Value`). For example, take this json file:
```json
{
  "title": "Foo",
  "foo": true,
  "config": {
    "bar": {
      "name": "foo bar"
    }
  }
}
```
Using serde derive, you would likely arrive at something similar to the code shown below. As you can see, you may need
to create separate structures for `config` and `bar` despite only containing a single field of interest each.
```rust
#[derive(Deserialize)]
struct Foo {
    title: String,
    foo: bool,
    config: FooConfig,
}

#[derive(Deserialize)]
struct FooConfig {
    bar: Bar,
}

#[derive(Deserialize)]
struct Bar {
    name: String,
}
```
To solve this problem, this crate provides a macro for shortening a long path to a single field.
```rust
#[flat_path]
#[derive(Deserialize)]
struct Foo {
    title: String,
    foo: bool,
    #[flat_path("config.bar.name")]
    bar_name: String,
}
```

## Usage
`flat_path` can be applied to any named field within a `struct` or `enum`. The `#[flat_path]` attribute must be applied
before the serialize/deserialize `#[derive(...)]` so that it can apply the necessary serde attributes before serde
performs macro expansion for its derive macros. Similar to derive macros, the original type is not altered.

For cases where field names contain `.` or additional verbosity is desired, `#[flat_path("a.b.c")]` may also be written
as `#[flat_path(path = ["a", "b", "c"])]`. These two forms are equivalent and no distinction is made between regarding
macro expansion.

`#[serde(...)]` attributes are also moved from `#[flat_path(...)]` fields to the final field within the path. During
this process, the order of attributes remains the same. 

