use proc_macro2::Delimiter;
use proc_macro2::TokenTree as TokenTree2;
use quote::ToTokens;
use std::collections::HashSet;
use syn::punctuated::Punctuated;
use syn::{Attribute, Error, Field, LitStr, Token};

/// There is probably a better approach to this. I attempted to use darling to parse the attributes,
/// but it had trouble with the string literals. Since it is fairly simple, I decided to just
/// implement it myself.
pub fn parse_attr(attr: &Attribute) -> syn::Result<Vec<LitStr>> {
    let attr_name = attr.path.clone().into_token_stream().to_string();
    assert_eq!(attr_name, "flat_path");

    let tree = syn::parse::<TokenTree2>(attr.tokens.clone().into())?;
    let group = match tree {
        TokenTree2::Group(group) => group,
        x => return Err(Error::new(x.span(), "Unexpected token in flat_path")),
    };

    let mut token_iter = group.stream().into_iter().map(TokenTree2::from);
    let first = token_iter
        .next()
        .ok_or_else(|| Error::new(group.span(), "Missing arguments"))?;

    match first {
        TokenTree2::Ident(x) => {
            if x != "path" {
                return Err(Error::new(x.span(), format!("Unknown argument `{}`", x)));
            }

            let eq = token_iter
                .next()
                .ok_or_else(|| Error::new(group.span(), "Incomplete macro arguments"))?;
            let list = token_iter
                .next()
                .ok_or_else(|| Error::new(group.span(), "Incomplete macro arguments"))?;

            if let Some(x) = token_iter.next() {
                return Err(Error::new(x.span(), "Too many arguments"));
            }

            expect_punctuation('=', eq)?;
            attr_from_path_array(list)
        }
        TokenTree2::Literal(x) => {
            let literal_str = syn::parse::<LitStr>(x.into_token_stream().into())?;
            Ok(attr_from_string_literal(literal_str))
        }
        _ => Err(Error::new(first.span(), "Invalid token")),
    }
}

fn attr_from_string_literal(literal: LitStr) -> Vec<LitStr> {
    literal
        .value()
        .split('.')
        .map(|x| LitStr::new(x, literal.span()))
        .collect()
}

fn attr_from_path_array(tree: TokenTree2) -> syn::Result<Vec<LitStr>> {
    let group = match tree {
        TokenTree2::Group(group) => group,
        x => return Err(Error::new(x.span(), "Expected bracketed group")),
    };

    if group.delimiter() != Delimiter::Bracket {
        return Err(Error::new(group.span(), "Expected bracketed group"));
    }

    let mut items = Vec::new();
    let mut iter = group.stream().into_iter();
    while let Some(tree) = iter.next() {
        items.push(syn::parse::<LitStr>(tree.into_token_stream().into())?);

        if let Some(comma) = iter.next() {
            expect_punctuation(',', comma)?;
        }
    }

    Ok(items)
}

fn expect_punctuation(c: char, tree: TokenTree2) -> syn::Result<()> {
    if let TokenTree2::Punct(punct) = &tree {
        if punct.as_char() == c {
            return Ok(());
        }
    }

    Err(Error::new(tree.span(), format!("Expected token `{}`", c)))
}

// When are they going to stabalize drain_filter?
pub fn extract_attributes_by_path(attrs: &mut Vec<Attribute>, path: &str) -> Vec<Attribute> {
    let mut output = Vec::new();
    let mut index = 0;
    while let Some(attr) = attrs.get(index) {
        if attr.path.clone().into_token_stream().to_string() == path {
            output.push(attrs.remove(index));
        } else {
            index += 1;
        }
    }

    output
}

pub fn has_overlapping_paths(fields: &Punctuated<Field, Token![,]>) -> syn::Result<bool> {
    let mut root_fields = HashSet::new();
    let mut has_overlaps = false;

    for attribute in fields.iter().flat_map(|field| field.attrs.iter()) {
        if attribute.path.clone().into_token_stream().to_string() != "flat_path" {
            continue;
        }

        if let Some(item) = parse_attr(attribute)?.get(0) {
            let ident = item.value();
            if root_fields.contains(&ident) {
                has_overlaps = true;
            }
            root_fields.insert(ident);
        }
    }

    Ok(has_overlaps)
}
