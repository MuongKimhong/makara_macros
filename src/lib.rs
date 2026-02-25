use proc_macro::TokenStream;
use proc_macro2::TokenTree;
use quote::quote;
use syn::parse::{Parse, ParseStream, Result};
use syn::{bracketed, parse_macro_input, Expr, Ident, Token};

mod kw {
    syn::custom_keyword!(insert);
    syn::custom_keyword!(on);
    syn::custom_keyword!(iter);
    syn::custom_keyword!(choices);
}

enum ChildrenMode {
    None,
    Static(Vec<Expr>),
    Iter(Expr),
}

struct WidgetTail {
    properties: Vec<(Ident, Expr)>,
    inserts: Vec<Expr>,
    observers: Vec<Expr>,
    children: ChildrenMode,
}

impl Parse for WidgetTail {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut properties = Vec::new();
        let mut inserts = Vec::new();
        let mut observers = Vec::new();
        let mut children = ChildrenMode::None;

        while !input.is_empty() {
            // Skip commas or semicolons
            if input.peek(Token![;]) || input.peek(Token![,]) {
                let _ = input.parse::<TokenTree>()?;
                continue;
            }

            if input.peek(kw::insert) && input.peek2(Token![:]) {
                input.parse::<kw::insert>()?; input.parse::<Token![:]>()?;
                inserts.push(input.parse()?);
            }
            else if input.peek(kw::on) && input.peek2(Token![:]) {
                input.parse::<kw::on>()?; input.parse::<Token![:]>()?;
                observers.push(input.parse()?);
            }
            else if input.peek(kw::iter) && input.peek2(syn::token::Bracket) {
                input.parse::<kw::iter>()?;
                let content;
                bracketed!(content in input);
                children = ChildrenMode::Iter(content.parse()?);
            }
            else if input.peek(syn::token::Bracket) {
                let content;
                bracketed!(content in input);
                let exprs = content.parse_terminated(Expr::parse, Token![,])?;
                children = ChildrenMode::Static(exprs.into_iter().collect());
            }
            else if input.peek(Ident) && input.peek2(Token![:]) {
                let ident: Ident = input.parse()?;
                input.parse::<Token![:]>()?;
                let expr: Expr = input.parse()?;
                properties.push((ident, expr));
            }
            else {
                return Err(input.error("Unexpected token in widget declaration"));
            }
        }

        Ok(WidgetTail { properties, inserts, observers, children })
    }
}

fn expand_widget(
    init_expr: proc_macro2::TokenStream,
    build_expr: proc_macro2::TokenStream,
    tail: WidgetTail,
) -> proc_macro2::TokenStream {
    let props = tail.properties.iter().map(|(k, v)| quote!( s = s.#k(#v); ));
    let inserts = &tail.inserts;
    let observers = &tail.observers;

    let children_tokens = match &tail.children {
        ChildrenMode::None => quote!(),
        ChildrenMode::Static(exprs) => quote!( , ::bevy::prelude::children![ #(#exprs),* ] ),
        ChildrenMode::Iter(expr) => quote!( , ::bevy::prelude::Children::spawn(::bevy::prelude::SpawnIter(#expr)) ),
    };

    quote! {
        {
            let mut s = #init_expr;
            #(#props)*
            (
                #build_expr
                #( , #inserts )*
                #( , makara::prelude::observe(#observers) )*
                #children_tokens
            )
        }
    }
}

macro_rules! no_argument_widget {
    ($name:ident, $path:expr) => {
        #[proc_macro]
        pub fn $name(input: TokenStream) -> TokenStream {
            let tail = parse_macro_input!(input as WidgetTail);
            let init: Expr = syn::parse_str($path).unwrap();
            expand_widget(quote!(#init()), quote!(s.build()), tail).into()
        }
    };
}

no_argument_widget!(row_, "makara::widgets::row::row");
no_argument_widget!(column_, "makara::widgets::column::column");
no_argument_widget!(scroll_, "makara::widgets::scroll::scroll");
no_argument_widget!(root_, "makara::widgets::root::root");
no_argument_widget!(modal_, "makara::widgets::modal::modal");
no_argument_widget!(radio_group_, "makara::widgets::radio::radio_group");
no_argument_widget!(progress_bar_, "makara::widgets::progress_bar::progress_bar");
no_argument_widget!(circular_, "makara::widgets::circular::circular");

struct WidgetWithText { text: Expr, tail: WidgetTail }
impl Parse for WidgetWithText {
    fn parse(input: ParseStream) -> Result<Self> {
        let text: Expr = input.parse()?;
        if input.peek(Token![,]) || input.peek(Token![;]) {
            let _ = input.parse::<TokenTree>();
        }
        let tail: WidgetTail = input.parse()?;
        Ok(WidgetWithText { text, tail })
    }
}

macro_rules! single_arg_widget {
    ($name:ident, $path:expr) => {
        #[proc_macro]
        pub fn $name(input: TokenStream) -> TokenStream {
            let parsed = parse_macro_input!(input as WidgetWithText);
            let text = parsed.text;
            let init: Expr = syn::parse_str($path).unwrap();
            expand_widget(quote!(#init(#text)), quote!(s.build()), parsed.tail).into()
        }
    };
}

single_arg_widget!(button_, "makara::widgets::button::button");
single_arg_widget!(dropdown_, "makara::widgets::dropdown::dropdown");
single_arg_widget!(image_, "makara::widgets::image::image");
single_arg_widget!(link_, "makara::widgets::link::link");
single_arg_widget!(radio_, "makara::widgets::radio::radio");
single_arg_widget!(text_input_, "makara::widgets::text_input::text_input");
single_arg_widget!(text_, "makara::widgets::text::text");
single_arg_widget!(checkbox_, "makara::widgets::checkbox::checkbox");

// Slider (min: expr, max: expr)
struct SliderWidget { min: Expr, max: Expr, tail: WidgetTail }
impl Parse for SliderWidget {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut min_val = None;
        let mut max_val = None;
        for _ in 0..2 {
            let ident: Ident = input.parse()?; input.parse::<Token![:]>()?;

            if ident == "min" {
                min_val = Some(input.parse()?);
            }
            else if ident == "max" {
                max_val = Some(input.parse()?);
            }

            if input.peek(Token![,]) || input.peek(Token![;]) {
                let _ = input.parse::<TokenTree>();
            }
        }
        Ok(SliderWidget { min: min_val.unwrap(), max: max_val.unwrap(), tail: input.parse()? })
    }
}

#[proc_macro]
pub fn slider_(input: TokenStream) -> TokenStream {
    let p = parse_macro_input!(input as SliderWidget);
    let (min, max) = (p.min, p.max);
    expand_widget(
        quote!(makara::widgets::slider::slider(#min, #max)),
        quote!(s.build()),
        p.tail
    )
    .into()
}

// Select (placeholder, choices: list)
struct SelectWidget { placeholder: Expr, choices: Expr, tail: WidgetTail }
impl Parse for SelectWidget {
    fn parse(input: ParseStream) -> Result<Self> {
        let placeholder: Expr = input.parse()?;

        if input.peek(Token![,]) || input.peek(Token![;]) {
            let _ = input.parse::<TokenTree>();
        }

        input.parse::<kw::choices>()?; input.parse::<Token![:]>()?;

        let choices: Expr = input.parse()?;
        if input.peek(Token![,]) || input.peek(Token![;]) {
            let _ = input.parse::<TokenTree>();
        }
        Ok(SelectWidget { placeholder, choices, tail: input.parse()? })
    }
}

#[proc_macro]
pub fn select_(input: TokenStream) -> TokenStream {
    let p = parse_macro_input!(input as SelectWidget);
    let (pl, ch) = (p.placeholder, p.choices);
    expand_widget(
        quote!(makara::widgets::select::select(#pl, #ch)),
        quote!(s.build()),
        p.tail
    )
    .into()
}

struct NavigateArgs {
    router: Expr,
    route: Expr,
    param: Expr,
}

impl Parse for NavigateArgs {
    fn parse(input: ParseStream) -> Result<Self> {
        let router: Expr = input.parse()?;
        input.parse::<Token![,]>()?; // Expect a comma

        let route: Expr = input.parse()?;
        input.parse::<Token![,]>()?; // Expect a comma

        let param: Expr = input.parse()?;

        if input.peek(Token![,]) {
            let _ = input.parse::<TokenTree>();
        }

        Ok(NavigateArgs { router, route, param })
    }
}

#[proc_macro]
pub fn navigate_(input: TokenStream) -> TokenStream {
    let args = parse_macro_input!(input as NavigateArgs);
    let router = args.router;
    let route = args.route;
    let param = args.param;

    let expanded = quote! {
        {
            #router.navigate(#route, #param);
        }
    };

    TokenStream::from(expanded)
}
