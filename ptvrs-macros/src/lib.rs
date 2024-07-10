use proc_macro::TokenStream;
use quote::{quote, ToTokens};

use syn::{
    bracketed,
    parse::{self, discouraged::Speculative, Parse},
    parse_macro_input, parse_quote, parse_quote_spanned,
    punctuated::Punctuated,
    spanned::Spanned,
    token::{self},
    Expr, ExprStruct, Ident, Path, Stmt, Token,
};

struct ExtraParams {
    _comma: Token![,],
    expr: Punctuated<Expr, Token![,]>,
}

impl Parse for ExtraParams {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let _comma = input.parse()?;
        let expr = Punctuated::<Expr, Token![,]>::parse_separated_nonempty(input)?;
        Ok(Self { _comma, expr })
    }
}

struct Field {
    field_name: Ident,
    value: Option<Expr>,
}

impl Parse for Field {
    fn parse(input: parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            field_name: input.parse()?,
            value: if input.peek(Token![:]) {
                input.parse::<Token![:]>()?;
                Some(input.parse()?)
            } else {
                None
            },
        })
    }
}

struct Struct {
    _comma2: Token![,],
    struc: Path,
    _arrow: Token![=>],
    _bracket: token::Bracket,
    idents: Punctuated<Field, Token![,]>,
}
impl Parse for Struct {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content;
        Ok(Self {
            _comma2: input.parse()?,
            struc: input.parse()?,
            _arrow: input.parse()?,
            _bracket: bracketed!(content in input),
            idents: if content.is_empty() {
                Punctuated::new()
            } else {
                Punctuated::<Field, Token![,]>::parse_separated_nonempty(&content)?
            },
        })
    }
}

struct MacroInput {
    map: Expr,
    _comma: Token![,],
    name: Ident,
    struc: Option<Struct>,
    extraparams: Option<ExtraParams>,
}

impl Parse for MacroInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            map: input.parse()?,
            _comma: input.parse()?,
            name: input.parse()?,
            struc: {
                let fork = input.fork();
                if let Ok(struc) = fork.parse::<Struct>() {
                    input.advance_to(&fork);
                    Some(struc)
                } else {
                    None
                }
            },
            extraparams: {
                let fork = input.fork();
                if let Ok(extraparams) = fork.parse() {
                    input.advance_to(&fork);
                    Some(extraparams)
                } else {
                    None
                }
            },
        })
    }
}
#[proc_macro]
pub fn make_test(_input: TokenStream) -> TokenStream {
    // parse the input of the macro, which is (Expr, Ident, Ident => [Ident+], Expr+)
    let MacroInput {
        map,
        name,
        struc,
        extraparams,
        ..
    } = parse_macro_input!(_input as MacroInput);

    let mut stmts: Vec<Stmt> = if let Some(Struct { idents, struc, .. }) = &struc {
        idents
            .iter()
            .map(|Field { field_name, value }| {
                // create the name of the test
                let mut test_name = format!("{}::{}", name, field_name);
                let value: Expr = if let Some(value) = value {
                    // if the field has a value, add it to the test name
                    test_name.push_str(&format!("::({})", value.to_token_stream()));
                    parse_quote_spanned! { value.span() => Some(#value)}
                } else {
                    parse_quote_spanned! { field_name.span() => Some(true)}
                };
                let parsed_struc: ExprStruct = parse_quote_spanned! { field_name.span() =>
                    #struc {
                        #field_name: #value,
                        ..Default::default()
                    }
                };
                let params: Punctuated<Expr, Token![,]> =
                    if let Some(ExtraParams { expr, .. }) = &extraparams {
                        parse_quote!( #expr, #parsed_struc)
                    } else {
                        parse_quote!(#parsed_struc )
                    };
                parse_quote_spanned! { field_name.span() =>
                    #map.insert(#test_name, Arc::new(|| {
                        Box::pin(async {
                            let res = (CLIENT.#name(#params)).await?;
                            Ok(format!("{:?}", res))
                        })
                    }));
                }
            })
            .collect()
    } else {
        vec![]
    };
    let default_params: Punctuated<Expr, Token![,]> = match (extraparams, struc) {
        (Some(ExtraParams { expr, .. }), Some(Struct { struc, .. })) => {
            parse_quote!(#expr, #struc::default())
        }
        (Some(ExtraParams { expr, .. }), None) => {
            parse_quote!(#expr)
        }
        (None, Some(Struct { struc, .. })) => {
            parse_quote!(#struc::default())
        }
        (None, None) => {
            parse_quote!()
        }
    };

    stmts.push(parse_quote! {
        #map.insert(stringify!(#name), Arc::new(|| {
            Box::pin(async {
                let res = (CLIENT.#name(#default_params)).await?;
                Ok(format!("{:?}", res))
            })
        }));
    });
    quote! {
        #(#stmts)*
    }
    .into()
}
