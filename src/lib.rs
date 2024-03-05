//! `flat_path` can be applied to any named field within a `struct` or `enum`. The `#[flat_path]`
//! attribute must be applied before the serialize/deserialize `#[derive(...)]` so that it can apply
//! the necessary serde attributes before serde performs macro expansion for its derive macros.
//! Similar to derive macros, the original type is not altered.
//!
//! For cases where field names contain `.` or additional verbosity is desired,
//! `#[flat_path("a.b.c")]` may also be written as `#[flat_path(path = ["a", "b", "c"])]`. These two
//! forms are equivalent and no distinction is made between regarding macro expansion.
//!
//! `#[serde(...)]` attributes are also moved from `#[flat_path(...)]` fields to the final field
//! within the path. During this process, the order of attributes remains the same.
//!
//! ```rust
//! # use serde::{Serialize, Deserialize};
//! # use serde_flat_path::flat_path;
//! #[flat_path]
//! #[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Default)]
//! #[serde(default)]
//! pub struct Foo {
//!     foo: bool,
//!     #[flat_path(path=["a", "b", "c"])]
//!     #[serde(skip_serializing_if="Option::is_none")]
//!     x: Option<u64>,
//!     #[serde(rename="INDEX")]
//!     index_number: u32,
//! }
//! ```
//!
//! For more examples see the repository's tests folder.

use proc_macro::TokenStream;
use proc_macro2::Span;
use proc_macro2::{Ident, TokenStream as TokenStream2};
use quote::{format_ident, quote, ToTokens};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{
    parse_quote, Attribute, Error, Field, Fields, ItemEnum, ItemStruct, LitStr, Path, Token, Type,
};

mod attr;

use attr::*;

/// For documentation go to the [crate] root page.
#[proc_macro_attribute]
pub fn flat_path(attr: TokenStream, input: TokenStream) -> TokenStream {
    match flat_path_impl(attr, input) {
        Ok(v) => v.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn flat_path_impl(attr: TokenStream, input: TokenStream) -> syn::Result<TokenStream2> {
    if !attr.is_empty() {
        panic!("This macro does not accept any inputs when in this position")
    }

    if let Ok(input) = syn::parse::<ItemStruct>(input.clone()) {
        flat_path_struct_impl(input)
    } else if let Ok(input) = syn::parse::<ItemEnum>(input) {
        flat_path_enum_impl(input)
    } else {
        Err(Error::new(
            Span::call_site(),
            "Can not be applied to this type of member",
        ))
    }
}

fn flat_path_struct_impl(mut item: ItemStruct) -> syn::Result<TokenStream2> {
    let module_name = format_ident!("__serde_flat_path_{}", item.ident);

    let named_fields = match named_fields(&mut item.fields)? {
        Some(v) => v,
        None => return Ok(quote!(#item)),
    };

    let module_path = parse_quote!(#module_name);
    let flat_path_conversions = perform_simple_flat_path_addition(&module_path, named_fields)?;

    Ok(quote! {
        #item

        #[doc(hidden)]
        #[allow(non_snake_case)]
        #[automatically_derived]
        mod #module_path {
            use super::*;
            #flat_path_conversions
        }
    })
}

fn flat_path_enum_impl(mut item: ItemEnum) -> syn::Result<TokenStream2> {
    let module_name = format_ident!("__serde_flat_path_{}", item.ident);

    let mut variant_impls = Vec::new();
    for variant in item.variants.iter_mut() {
        let named_fields = match named_fields(&mut variant.fields)? {
            Some(v) => v,
            None => continue,
        };

        let variant_name = &variant.ident;
        let module_path = parse_quote!(#module_name::#variant_name);
        let flat_path_conversions = perform_simple_flat_path_addition(&module_path, named_fields)?;

        variant_impls.push(quote! {
            pub mod #variant_name {
                use super::*;
                #flat_path_conversions
            }
        });
    }

    Ok(quote! {
        #item

        #[doc(hidden)]
        #[allow(non_snake_case)]
        #[automatically_derived]
        mod #module_name {
            use super::*;
            #(#variant_impls)*
        }
    })
}

fn perform_simple_flat_path_addition(
    module_name: &Path,
    named_fields: &mut Punctuated<Field, Token![,]>,
) -> syn::Result<TokenStream2> {
    let mut paths = Vec::new();

    #[cfg(not(feature = "allow_overlap"))]
    if has_overlapping_paths(named_fields)? {
        return Err(Error::new(
            Span::call_site(),
            "Appling flat_path would result in \
        overlapping paths. At the current time, this behavior is not yet supported. Please create \
        an intermediate type to prevent these overlaps from occuring. Alternatively, the \
        `allow_overlap` feature can be enabled to bypass this error, but may result in malformed \
        results.",
        ));
    }

    for field in named_fields.iter_mut() {
        let flat_path_attributes = extract_attributes_by_path(&mut field.attrs, "flat_path");
        if flat_path_attributes.is_empty() {
            continue;
        }

        if flat_path_attributes.len() > 1 {
            return Err(Error::new(
                flat_path_attributes[1].span(),
                "flat_path can only be applied once",
            ));
        }

        let mut flat_path = parse_attr(&flat_path_attributes[0])?;

        let field_name = field
            .ident
            .clone()
            .ok_or_else(|| Error::new(field.span(), "Unable to apply flat_path to tuple fields"))?;

        let module_ref = LitStr::new(
            &format!(
                "{}::{}",
                module_name.clone().into_token_stream(),
                field_name
            ),
            field_name.span(),
        );

        let first_name = flat_path.remove(0);
        let serde_attributes = extract_attributes_by_path(&mut field.attrs, "serde");
        field
            .attrs
            .push(parse_quote!(#[serde(rename=#first_name, with=#module_ref)]));

        paths.push(FlatField {
            ident: field_name,
            ty: field.ty.clone(),
            flat_path,
            serde_attributes,
        });
    }

    Ok(generate_flat_path_module(paths))
}

fn named_fields(fields: &mut Fields) -> syn::Result<Option<&mut Punctuated<Field, Token![,]>>> {
    match fields {
        Fields::Unit => Ok(None),
        Fields::Named(named) => Ok(Some(&mut named.named)),
        Fields::Unnamed(unnamed) => {
            for field in &mut unnamed.unnamed {
                if !extract_attributes_by_path(&mut field.attrs, "flat_path").is_empty() {
                    return Err(Error::new(
                        field.span(),
                        "Unable to apply flat_path to unnamed tuple fields",
                    ));
                }
            }

            Ok(None)
        }
    }
}

fn generate_flat_path_module(flat_fields: Vec<FlatField>) -> TokenStream2 {
    let mut tokens = TokenStream2::new();

    for flat_field in flat_fields {
        let contents = flat_field.generate_serialize_with();
        let field = flat_field.ident;

        tokens.extend(quote! {
            pub mod #field {
                use super::*;
                #contents
            }
        });
    }

    tokens
}

struct FlatField {
    ident: Ident,
    ty: Type,
    flat_path: Vec<LitStr>,
    serde_attributes: Vec<Attribute>,
}

impl FlatField {
    fn generate_serialize_with(&self) -> TokenStream2 {
        if self.serde_attributes.is_empty() {
            return self.with_structural_derive();
        }

        // This is more prone to errors due to generics than with_structural_derive, but it is able
        // to handle serde bounds properly so it is preferred for those cases.
        self.with_concrete_type_derive()
    }

    fn with_concrete_type_derive(&self) -> TokenStream2 {
        let mut tokens = TokenStream2::new();

        let ty_tokens = self.ty.clone().into_token_stream();
        let serialize_bound = LitStr::new(
            &format!("{}: ::serde::Serialize", &ty_tokens),
            Span::call_site(),
        );
        let deserialize_bound = LitStr::new(
            &format!("{}: ::serde::de::Deserialize<'de>", &ty_tokens),
            Span::call_site(),
        );

        let path_length = self.flat_path.len();
        let placeholders = (0..path_length)
            .map(|x| format_ident!("_{}", x))
            .collect::<Vec<_>>();
        for (index, field_name) in self.flat_path[..path_length - 1].iter().enumerate() {
            let ident = &placeholders[index];
            let next = &placeholders[index + 1];

            tokens.extend(quote! {
                #[repr(transparent)]
                #[derive(::serde::Serialize, ::serde::Deserialize, Default)]
                #[serde(bound(serialize = #serialize_bound, deserialize = #deserialize_bound))]
                struct #ident {
                    #[serde(rename=#field_name)]
                    _0: #next
                }
            });
        }

        let last_ident = &placeholders[path_length - 1];
        let last_field_name = &self.flat_path[path_length - 1];
        let serde_attributes = &self.serde_attributes;
        let field_type = &self.ty;

        let chain = std::iter::repeat(format_ident!("_0")).take(path_length);
        tokens.extend(quote! {
            #[repr(transparent)]
            #[derive(::serde::Serialize, ::serde::Deserialize, Default)]
            #[serde(bound(serialize = #serialize_bound, deserialize = #deserialize_bound))]
            struct #last_ident {
                #[serde(rename=#last_field_name)]
                #(#serde_attributes)*
                _0: #field_type
            }

            #[inline(always)]
            pub fn deserialize<'de, D>(deserializer: D) -> Result<#field_type, D::Error>
                where #field_type: ::serde::Deserialize<'de>,
                      D: ::serde::Deserializer<'de>,
            {
                match <_0 as ::serde::Deserialize>::deserialize(deserializer) {
                    Ok(value) => Ok(value #(.#chain)*),
                    Err(e) => Err(e)
                }
            }

            #[inline(always)]
            pub fn serialize<S>(this: &#field_type, serializer: S) -> Result<S::Ok, S::Error>
                where #field_type: ::serde::Serialize,
                      S: ::serde::Serializer
            {
                // # Safety
                // This is safe as all members within the chain use repr(transparent) to a value of
                // T. Furthermore, data is not accessed via this reference until it is converted
                // back to &T at the end of the chain.
                let chain_ref = unsafe { ::std::mem::transmute::<&#field_type, &_0>(this) };
                ::serde::Serialize::serialize(chain_ref, serializer)
            }
        });

        tokens
    }

    fn with_structural_derive(&self) -> TokenStream2 {
        let mut tokens = TokenStream2::new();

        let path_length = self.flat_path.len();
        let placeholders = (0..path_length)
            .map(|x| format_ident!("_{}", x))
            .collect::<Vec<_>>();
        for (index, field_name) in self.flat_path[..path_length - 1].iter().enumerate() {
            let ident = &placeholders[index];
            let next = &placeholders[index + 1];

            tokens.extend(quote! {
                #[repr(transparent)]
                #[derive(::serde::Serialize, ::serde::Deserialize, Default)]
                struct #ident<T: ?Sized> {
                    #[serde(rename=#field_name)]
                    _0: #next<T>
                }
            });
        }

        let last_ident = &placeholders[path_length - 1];
        let last_field_name = &self.flat_path[path_length - 1];
        let serde_attributes = &self.serde_attributes;

        let chain = std::iter::repeat(format_ident!("_0")).take(path_length);
        tokens.extend(quote! {
            #[repr(transparent)]
            #[derive(::serde::Serialize, ::serde::Deserialize, Default)]
            struct #last_ident<T: ?Sized> {
                #[serde(rename=#last_field_name)]
                #(#serde_attributes)*
                _0: T
            }

            #[inline(always)]
            pub fn deserialize<'de, D, T>(deserializer: D) -> Result<T, D::Error>
                where T: ?Sized + ::serde::Deserialize<'de>,
                      D: ::serde::Deserializer<'de>,
            {
                match <_0<T> as ::serde::Deserialize>::deserialize(deserializer) {
                    Ok(value) => Ok(value #(.#chain)*),
                    Err(e) => Err(e)
                }
            }

            #[inline(always)]
            pub fn serialize<S, T>(this: &T, serializer: S) -> Result<S::Ok, S::Error>
                where T: ?Sized + ::serde::Serialize,
                      S: ::serde::Serializer
            {
                // # Safety
                // This is safe as all members within the chain use repr(transparent) to a value of
                // T. Furthermore, data is not accessed via this reference until it is converted
                // back to &T at the end of the chain.
                let chain_ref = unsafe { ::std::mem::transmute::<&T, &_0<T>>(this) };
                ::serde::Serialize::serialize(chain_ref, serializer)
            }
        });

        tokens
    }
}
