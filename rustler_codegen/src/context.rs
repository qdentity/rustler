use heck::ToSnakeCase;
use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{Attribute, Data, Field, Fields, Ident, Lifetime, Lit, Meta, TypeParam, Variant};

use super::RustlerAttr;

///
/// A parsing context struct.
///
/// `Context` holds information usable for different codegen modules.
///
pub(crate) struct Context<'a> {
    pub attrs: Vec<RustlerAttr>,
    pub ident: &'a proc_macro2::Ident,
    pub generics: &'a syn::Generics,
    pub lifetimes: Vec<Lifetime>,
    pub type_parameters: Vec<TypeParam>,
    pub variants: Option<Vec<&'a Variant>>,
    pub struct_fields: Option<Vec<&'a Field>>,
    pub is_tuple_struct: bool,
}

impl<'a> Context<'a> {
    pub fn from_ast(ast: &'a syn::DeriveInput) -> Self {
        let mut attrs: Vec<_> = ast
            .attrs
            .iter()
            .flat_map(Context::get_rustler_attrs)
            .collect();

        //
        // Default: generate encoder and decoder
        //
        if !Context::encode_decode_attr_set(&attrs) {
            attrs.push(RustlerAttr::Encode);
            attrs.push(RustlerAttr::Decode);
        }

        let variants = match ast.data {
            Data::Enum(ref data_enum) => Some(data_enum.variants.iter().collect()),
            _ => None,
        };

        let struct_fields = match ast.data {
            Data::Struct(ref data_struct) => Some(data_struct.fields.iter().collect()),
            _ => None,
        };

        let is_tuple_struct = match ast.data {
            Data::Struct(ref data_struct) => matches!(data_struct.fields, Fields::Unnamed(_)),
            _ => false,
        };

        let lifetimes: Vec<_> = ast
            .generics
            .params
            .iter()
            .filter_map(|g| match g {
                syn::GenericParam::Lifetime(l) => Some(l.lifetime.clone()),
                _ => None,
            })
            .collect();

        let type_parameters: Vec<_> = ast
            .generics
            .params
            .iter()
            .filter_map(|g| match g {
                syn::GenericParam::Type(t) => Some(t.clone()),
                // Don't keep lifetimes or generic constants
                _ => None,
            })
            .collect();

        Self {
            attrs,
            ident: &ast.ident,
            generics: &ast.generics,
            lifetimes,
            type_parameters,
            variants,
            struct_fields,
            is_tuple_struct,
        }
    }

    pub fn atoms_module_name(&self, span: Span) -> Ident {
        Ident::new(
            &format!("rustler_atoms_{}", self.ident).to_snake_case(),
            span,
        )
    }

    pub fn encode(&self) -> bool {
        self.attrs
            .iter()
            .any(|attr| matches!(attr, RustlerAttr::Encode))
    }

    pub fn decode(&self) -> bool {
        self.attrs
            .iter()
            .any(|attr| matches!(attr, RustlerAttr::Decode))
    }

    pub fn field_atoms(&self) -> Option<Vec<TokenStream>> {
        self.struct_fields.as_ref().map(|struct_fields| {
            struct_fields
                .iter()
                .map(|field| {                    
                    let atom_fun = Self::field_to_atom_fun(field);
                    let ident_str = Self::maybe_parse_rename(&field.attrs)
                        .unwrap_or_else(|| {
                            let ident = field.ident.as_ref().unwrap();
                            ident.to_string()
                        });
                    let ident_str = Self::remove_raw(&ident_str);

                    quote! {
                        #atom_fun = #ident_str,
                    }
                })
                .collect()
        })
    }

    pub fn field_to_atom_fun(field: &Field) -> Ident {
        let ident = field.ident.as_ref().unwrap();
        Self::string_to_atom_fun(&ident.to_string())
    }

    pub fn string_to_atom_fun(str: &str) -> Ident {
        let ident_str = str.to_snake_case();
        let ident_str = Self::remove_raw(&ident_str);

        Ident::new(&format!("atom_{}", ident_str), Span::call_site())
    }

    pub fn escape_ident_with_index(ident_str: &str, index: usize, infix: &str) -> Ident {
        Ident::new(
            &format!(
                "rustler_{}_field_{}_{}",
                infix,
                index,
                Self::remove_raw(ident_str)
            ),
            Span::call_site(),
        )
    }

    pub fn escape_ident(ident_str: &str, infix: &str) -> Ident {
        Ident::new(
            &format!("rustler_{}_field_{}", infix, Self::remove_raw(ident_str)),
            Span::call_site(),
        )
    }

    fn remove_raw(ident_str: &str) -> &str {
        ident_str
            .split("r#")
            .last()
            .expect("split has always at least one element")
    }

    fn encode_decode_attr_set(attrs: &[RustlerAttr]) -> bool {
        attrs
            .iter()
            .any(|attr| matches!(attr, RustlerAttr::Encode | RustlerAttr::Decode))
    }

    fn get_rustler_attrs(attr: &syn::Attribute) -> Vec<RustlerAttr> {
        attr.path()
            .segments
            .iter()
            .filter_map(|segment| {
                let meta = &attr.meta;
                match segment.ident.to_string().as_ref() {
                    "rustler" => Some(Context::parse_rustler(meta)),
                    "tag" => Context::try_parse_tag(meta),
                    "module" => Context::try_parse_module(meta),
                    _ => None,
                }
            })
            .flatten()
            .collect()
    }

    fn parse_rustler(meta: &Meta) -> Vec<RustlerAttr> {
        if let Meta::List(ref list) = meta {
            let mut attrs: Vec<RustlerAttr> = vec![];
            let _ = list.parse_nested_meta(|nested_meta| {
                if nested_meta.path.is_ident("encode") {
                    attrs.push(RustlerAttr::Encode);
                    Ok(())
                } else if nested_meta.path.is_ident("decode") {
                    attrs.push(RustlerAttr::Decode);
                    Ok(())
                } else {
                    Err(nested_meta.error("Expected encode and/or decode in rustler attribute"))
                }
            });

            return attrs;
        }

        panic!("Expected encode and/or decode in rustler attribute");
    }

    fn try_parse_tag(meta: &Meta) -> Option<Vec<RustlerAttr>> {
        if let Meta::NameValue(ref name_value) = meta {
            let expr = &name_value.value;

            if let syn::Expr::Lit(lit_expr) = expr {
                if let Lit::Str(ref tag) = lit_expr.lit {
                    return Some(vec![RustlerAttr::Tag(tag.value())]);
                }
            }
        }
        panic!("Cannot parse tag")
    }

    fn try_parse_module(meta: &Meta) -> Option<Vec<RustlerAttr>> {
        if let Meta::NameValue(name_value) = meta {
            let expr = &name_value.value;

            if let syn::Expr::Lit(lit_expr) = expr {
                if let Lit::Str(ref module) = lit_expr.lit {
                    let ident = format!("Elixir.{}", module.value());
                    return Some(vec![RustlerAttr::Module(ident)]);
                }
            }
        }
        panic!("Cannot parse module")
    }

    pub(crate) fn name_from_attrs_or_ident(ident: &Ident, attrs: &[Attribute]) -> String {
        if let Some(name) = Self::maybe_parse_rename(attrs) {
            return name;
        }

        return ident.to_string();
    }

    fn maybe_parse_rename(attrs: &[Attribute]) -> Option<String> {
        fn parse_rename(attr: &Attribute) -> Result<String, ()> {
            let expr: syn::Expr = attr.parse_args().map_err(|_| ())?;
            let expr = match expr {
                syn::Expr::Assign(expr) => expr,
                _ => return Err(())
            };

            // check if left hand side is 'rename'
            let mut found_rename_ident = false;
            if let syn::Expr::Path(key) = &*expr.left {
                found_rename_ident = key.path.is_ident("rename");
            }

            if !found_rename_ident {
                return Err(())
            }

            // check if right hand side is the new field name
            let mut rename_value = None;

            if let syn::Expr::Lit(lit) = &*expr.right {
                if let syn::Lit::Str(str) = &lit.lit {
                    rename_value = Some(str.value());
                }
            }

            return rename_value.ok_or(());
        }

        for attr in attrs {
            if !attr.path().is_ident("rustler") {
                // ignore
                continue
            }

            // We now expect to support whatever is inside the args
            let name = parse_rename(&attr).expect("expected 'rustler(rename = \"field_name\")'");
            return Some(name)
        }

        return None;
    }
}
