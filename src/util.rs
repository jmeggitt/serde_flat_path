use proc_macro::TokenStream;
use proc_macro2::{Ident, Span, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::{Error, Field, Fields, ItemEnum, ItemStruct, ItemUnion};

pub fn visit_all_fields_mut<F>(input: TokenStream, mut func: F) -> syn::Result<TokenStream2>
where
    F: FnMut(&mut Field) -> syn::Result<()>,
{
    fn visit_fields<F>(fields: &mut Fields, func: F) -> syn::Result<()>
    where
        F: FnMut(&mut Field) -> syn::Result<()>,
    {
        match fields {
            Fields::Named(named) => named.named.iter_mut().try_for_each(func),
            Fields::Unnamed(unnamed) => unnamed.unnamed.iter_mut().try_for_each(func),
            Fields::Unit => Ok(()),
        }
    }

    if let Ok(mut input) = syn::parse::<ItemStruct>(input.clone()) {
        visit_fields(&mut input.fields, func)?;
        Ok(quote!(#input))
    } else if let Ok(mut input) = syn::parse::<ItemEnum>(input) {
        for variant in &mut input.variants {
            visit_fields(&mut variant.fields, &mut func)?;
        }
        Ok(quote!(#input))
    } else {
        Err(Error::new(
            Span::call_site(),
            "Can not be applied to this type of member",
        ))
    }
}

/// Takes a token stream representing a type and extracts its name as a string.
pub fn type_name(input: TokenStream) -> syn::Result<String> {
    if let Ok(input) = syn::parse::<ItemStruct>(input.clone()) {
        Ok(input.ident.to_string())
    } else if let Ok(input) = syn::parse::<ItemEnum>(input.clone()) {
        Ok(input.ident.to_string())
    } else if let Ok(input) = syn::parse::<ItemUnion>(input) {
        Ok(input.ident.to_string())
    } else {
        Err(Error::new(Span::call_site(), "Unable to get name of type"))
    }
}

pub fn placeholder_idents(n: usize) -> Vec<Ident> {
    (0..n).map(|x| format_ident!("_{}", x)).collect()
}
