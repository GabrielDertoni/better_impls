#![allow(unused_macros)]
#![allow(dead_code)]
#![feature(box_patterns)]

extern crate proc_macro;

use proc_macro::TokenStream;
use syn::{ parse_macro_input, parse_quote, ItemFn };
use quote::{ quote, format_ident };


#[proc_macro_attribute]
pub fn impl_iterator(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut ast = parse_macro_input!(item as ItemFn);

    use syn::{ ReturnType
             , Type
             , TypeImplTrait
             , TypeParamBound
             , TraitBound
             , Path
             , PathSegment
             , PathArguments
             , AngleBracketedGenericArguments
             , GenericArgument
             , Binding
             };

    let item_type = match ast.sig.output {
        ReturnType::Type(_, box Type::ImplTrait(TypeImplTrait { ref mut bounds, .. })) => {
            if bounds.len() != 1 {
                panic!("Expected an impl Iterator as return type");
            }

            // Assert the function has `impl Iterator` as return type.
            if let TypeParamBound::Trait(
                TraitBound {
                    path: Path {
                        segments,
                        ..
                    },
                    ..
                }
            ) = bounds.first().unwrap() {
                let PathSegment { ident, .. } = segments.last().unwrap();
                if ident != "Iterator" {
                    panic!("Expected an impl Iterator as return type.");
                }

                if let PathSegment {
                    arguments: PathArguments::AngleBracketed(
                        AngleBracketedGenericArguments {
                            args,
                            ..
                        }
                    ),
                    ..
                } = segments.last().unwrap() {
                    if let GenericArgument::Binding(Binding { ty, .. }) = args.first().unwrap() {
                        ty.clone()
                    } else {
                        panic!("Expected binding in Iterator Item type")
                    }
                } else {
                    panic!("Expected angle bracketed generic arguments")
                }
            } else {
                unreachable!();
            }
        },
        _ => panic!("Expected an impl Iterator as return type."),
    };

    let type_name = format_ident!("BetterImpl{}", ast.sig.ident.to_string());
    let returns_count = overwrite_all_return_exprs(&mut ast, type_name.clone());

    let mut i = 0;
    let enum_variants: Vec<_> = std::iter::from_fn(move || -> Option<syn::Ident> {
        i += 1;
        if i <= returns_count {
            Some(format_ident!("Return{}", i))
        } else {
            None
        }
    }).collect();

    let mut i = 0;
    let enum_generics: Vec<_> = std::iter::from_fn(move || -> Option<syn::GenericParam> {
        i += 1;
        if i <= returns_count {
            let generic = format_ident!("T{}", i);
            Some(parse_quote!{ #generic })
        } else {
            None
        }
    }).collect();

    let stream = quote! {
        enum #type_name<#(#enum_generics),*> {
            #(#enum_variants(#enum_generics)),*
        }

        impl<#(#enum_generics),*> Iterator for #type_name<#(#enum_generics),*>
        where
            #(#enum_generics: Iterator<Item = #item_type>),*
        {
            type Item = #item_type;
            fn next(&mut self) -> Option<#item_type> {
                match self {
                    #(#type_name::#enum_variants(iter) => iter.next()),*
                }
            }
        }
        #ast
    };
    TokenStream::from(stream)
}

fn overwrite_all_return_exprs<'a>(item: &'a mut syn::ItemFn, type_name: syn::Ident) -> usize {
    let mut count = 0;
    overwrite_all_return_exprs_in_block(item.block.as_mut(), &mut count, false, type_name);
    count
    // overwrite_all_return_exprs_acc(expr, &mut count, 0, only_return);
}

fn overwrite_all_return_exprs_acc<'a>(
    expr: &'a mut syn::Expr,
    count: &mut usize,
    only_return: bool,
    type_name: syn::Ident,
) {
    use syn::Expr::*;

    match *expr {
        Array(ref mut expr) => {
            expr.elems.iter_mut()
                .for_each(|el| {
                    overwrite_all_return_exprs_acc(el, count, true, type_name.clone());
                });
        },
        Assign(_) => if !only_return {
            /* Overwrite */
            overwrite(expr, *count, type_name.clone());
            *count += 1;
        },
        AssignOp(_) => if !only_return {
            overwrite(expr, *count, type_name.clone());
            *count += 1;
        },
        Binary(ref mut bin_expr) => {
            overwrite_all_return_exprs_acc(bin_expr.left.as_mut(), count, true, type_name.clone());
            overwrite_all_return_exprs_acc(bin_expr.right.as_mut(), count, true, type_name.clone());

            if !only_return {
                overwrite(expr, *count, type_name.clone());
                *count += 1;
            }
        },
        Block(ref mut expr) =>
            overwrite_all_return_exprs_in_block(&mut expr.block, count, only_return, type_name.clone()),

        Break(_) => panic!("Breaks are not supported"),
        Call(ref mut call) => {
            overwrite_all_return_exprs_acc(call.func.as_mut(), count, true, type_name.clone());
            call.args.iter_mut()
                .for_each(|el| {
                    overwrite_all_return_exprs_acc(el, count, true, type_name.clone());
                });

            if !only_return {
                overwrite(expr, *count, type_name.clone());
                *count += 1;
            }
        },
        Cast(_) => if !only_return {
            overwrite(expr, *count, type_name.clone());
            *count += 1;
        },
        Closure(_) => if !only_return {
            overwrite(expr, *count, type_name.clone());
            *count += 1;
        },
        Field(ref mut field) => {
            overwrite_all_return_exprs_acc(field.base.as_mut(), count, true, type_name.clone());

            if !only_return {
                overwrite(expr, *count, type_name.clone());
                *count += 1;
            }
        },
        ForLoop(ref mut for_loop) => {
            overwrite_all_return_exprs_in_block(&mut for_loop.body, count, true, type_name.clone());
            if !only_return {
                overwrite(expr, *count, type_name.clone());
                *count += 1;
            }
        },
        Group(ref mut expr) =>
            overwrite_all_return_exprs_acc(&mut expr.expr, count, only_return, type_name.clone()),

        If(ref mut if_expr) => {
            overwrite_all_return_exprs_acc(if_expr.cond.as_mut(), count, true, type_name.clone());
            overwrite_all_return_exprs_in_block(&mut if_expr.then_branch, count, only_return, type_name.clone());
            if_expr.else_branch.as_mut().map(|(_, box ref mut expr)| {
                overwrite_all_return_exprs_acc(expr, count, only_return, type_name.clone());
            });
        },
        Index(ref mut index_expr) => {
            overwrite_all_return_exprs_acc(index_expr.expr.as_mut(), count, true, type_name.clone());
            overwrite_all_return_exprs_acc(index_expr.index.as_mut(), count, true, type_name.clone());
            if !only_return {
                overwrite(expr, *count, type_name.clone());
                *count += 1;
            }
        },
        Let(ref mut expr) =>
            overwrite_all_return_exprs_acc(expr.expr.as_mut(), count, true, type_name.clone()),

        Lit(_) => if !only_return {
            overwrite(expr, *count, type_name.clone());
            *count += 1;
        },
        Loop(ref mut loop_expr) => {
            overwrite_all_return_exprs_in_block(&mut loop_expr.body, count, true, type_name.clone());
        },
        Macro(_) => panic!("Macro as return is not supported"),
        Match(ref mut match_expr) => {
            overwrite_all_return_exprs_acc(match_expr.expr.as_mut(), count, true, type_name.clone());
            match_expr.arms.iter_mut()
                .for_each(|arm| {
                    arm.guard.as_mut()
                        .map(|(_, guard)| overwrite_all_return_exprs_acc(guard, count, true, type_name.clone()));

                    overwrite_all_return_exprs_acc(arm.body.as_mut(), count, only_return, type_name.clone());
                });
        },
        MethodCall(ref mut method_expr) => {
            overwrite_all_return_exprs_acc(method_expr.receiver.as_mut(), count, true, type_name.clone());
            method_expr.args.iter_mut()
                .for_each(|el| {
                    overwrite_all_return_exprs_acc(el, count, true, type_name.clone());
                });

            if !only_return {
                overwrite(expr, *count, type_name.clone());
                *count += 1;
            }
        },
        Paren(ref mut paren_expr) => {
            overwrite_all_return_exprs_acc(paren_expr.expr.as_mut(), count, true, type_name.clone());

            if !only_return {
                overwrite(expr, *count, type_name.clone());
                *count += 1;
            }
        },
        Path(_) => if !only_return {
            overwrite(expr, *count, type_name.clone());
            *count += 1;
        },
        Range(ref mut range_expr) => {
            range_expr.from.as_mut().map(|from| overwrite_all_return_exprs_acc(from, count, true, type_name.clone()));
            range_expr.to.as_mut().map(|from| overwrite_all_return_exprs_acc(from, count, true, type_name.clone()));

            if !only_return {
                overwrite(expr, *count, type_name.clone());
                *count += 1;
            }
        },
        Reference(ref mut ref_expr) => {
            overwrite_all_return_exprs_acc(ref_expr.expr.as_mut(), count, true, type_name.clone());

            if !only_return {
                overwrite(expr, *count, type_name.clone());
                *count += 1;
            }
        },
        Repeat(ref mut repeat) => {
            overwrite_all_return_exprs_acc(repeat.expr.as_mut(), count, true, type_name.clone());
            overwrite_all_return_exprs_acc(repeat.len.as_mut(), count, true, type_name.clone());

            if !only_return {
                overwrite(expr, *count, type_name.clone());
                *count += 1;
            }
        },
        Return(ref mut ret_expr) => {
            ret_expr.expr.as_mut().map(|expr| {
                overwrite_all_return_exprs_acc(expr.as_mut(), count, true, type_name.clone());
                overwrite(expr, *count, type_name.clone());
                *count += 1;
            });
        },
        Struct(_) => if !only_return {
            // TODO: Finish with all of the fields.
            overwrite(expr, *count, type_name.clone());
            *count += 1;
        },
        Try(_) => if !only_return {
            // TODO: Finish with the body.
            overwrite(expr, *count, type_name.clone());
            *count += 1;
        },
        TryBlock(_) => panic!("Try blocks are not supported"),
        Tuple(ref mut tuple_expr) => {
            tuple_expr.elems.iter_mut()
                .for_each(|el| {
                    overwrite_all_return_exprs_acc(el, count, true, type_name.clone());
                });

            if !only_return {
                overwrite(expr, *count, type_name.clone());
                *count += 1;
            }
        },
        Type(_) => panic!("Types are not supported as return"),
        Unary(ref mut unary) => {
            overwrite_all_return_exprs_acc(unary.expr.as_mut(), count, true, type_name.clone());

            if !only_return {
                overwrite(expr, *count, type_name.clone());
                *count += 1;
            }
        },
        Unsafe(ref mut unsafe_expr) => {
            overwrite_all_return_exprs_in_block(&mut unsafe_expr.block, count, only_return, type_name.clone());

            if !only_return {
                overwrite(expr, *count, type_name.clone());
                *count += 1;
            }
        },
        While(ref mut while_expr) => {
            overwrite_all_return_exprs_in_block(&mut while_expr.body, count, true, type_name.clone());

            if !only_return {
                overwrite(expr, *count, type_name.clone());
                *count += 1;
            }
        },
        _ => panic!("Unsupported token"),
    }
}

// In a block, only one expression can implicitly be "returned": the last one.
// Other expressions can have return statements in them, though.
fn overwrite_all_return_exprs_in_block(
    block: &mut syn::Block,
    count: &mut usize,
    only_return: bool,
    type_name: syn::Ident
) {
    use syn::Stmt::*;

    let (last_stmt, others) = block.stmts.split_last_mut()
        .expect("There is a satement in the function");

    for stmt in others {
        match stmt {
            Expr(expr) |
            Semi(expr, _) =>
                overwrite_all_return_exprs_acc(expr, count, true, type_name.clone()),

            Local(local) => {
                local.init.as_mut().map(|(_, box ref mut expr)| {
                    overwrite_all_return_exprs_acc(expr, count, true, type_name.clone())
                });
            },

            Item(_)  => (),
        }
    }

    match last_stmt {
        Expr(expr)    =>
            overwrite_all_return_exprs_acc(expr, count, only_return, type_name),

        Semi(expr, _) =>
            overwrite_all_return_exprs_acc(expr, count, true, type_name),

        Local(local) => {
            local.init.as_mut().map(|(_, box ref mut expr)| {
                overwrite_all_return_exprs_acc(expr, count, true, type_name)
            });
        },

        Item(_)  => (),
    }
}

fn overwrite(expr: &mut syn::Expr, count: usize, type_name: syn::Ident) {
    let return_num = format_ident!("Return{}", count + 1);
    memapply(expr, |owned| {
        parse_quote!{ #type_name::#return_num(#owned) }
    });
}

fn memapply<T, F>(val: &mut T, f: F)
where
    F: FnOnce(T) -> T,
{
    unsafe {
        let zeroed = std::mem::MaybeUninit::zeroed();
        let owned = std::mem::replace(val, zeroed.assume_init());
        std::mem::forget(std::mem::replace(val, f(owned)));
    }
}
