// use crate::lexer::Token;

// `Parser`
// pub struct Parser<'de> {
//     whole: &'de str,
//     rest: &'de str,
//     byte: usize,
//     line_num: usize,
// }

/*
expression     → literal
               | unary
               | binary
               | grouping ;

literal        → NUMBER | STRING | "true" | "false" | "nil" ;
grouping       → "(" expression ")" ;
unary          → ( "-" | "!" ) expression ;
binary         → expression operator expression ;
operator       → "==" | "!=" | "<" | "<=" | ">" | ">="
               | "+"  | "-"  | "*" | "/" ;
 */

// struct Expr {}

// struct BinaryExpr {
//     left: Expr,
//     operator: Token,
//     right: Expr,
// }

// // The data we will visit
// mod ast {
//     pub enum Stmt {
//         Expr(Expr),
//         Let(Name, Expr),
//     }

//     pub struct Name {
//         value: String,
//     }

//     pub enum Expr {
//         IntLit(i64),
//         Add(Box<Expr>, Box<Expr>),
//         Sub(Box<Expr>, Box<Expr>),
//     }
// }

// // The abstract visitor
// mod visit {
//     use ast::*;

//     pub trait Visitor<T> {
//         fn visit_name(&mut self, n: &Name) -> T;
//         fn visit_stmt(&mut self, s: &Stmt) -> T;
//         fn visit_expr(&mut self, e: &Expr) -> T;
//     }
// }

// use ast::*;
// use visit::*;

// // An example concrete implementation - walks the AST interpreting it as code.
// struct Interpreter;
// impl Visitor<i64> for Interpreter {
//     fn visit_name(&mut self, n: &Name) -> i64 {
//         panic!()
//     }
//     fn visit_stmt(&mut self, s: &Stmt) -> i64 {
//         match *s {
//             Stmt::Expr(ref e) => self.visit_expr(e),
//             Stmt::Let(..) => unimplemented!(),
//         }
//     }

//     fn visit_expr(&mut self, e: &Expr) -> i64 {
//         match *e {
//             Expr::IntLit(n) => n,
//             Expr::Add(ref lhs, ref rhs) => self.visit_expr(lhs) + self.visit_expr(rhs),
//             Expr::Sub(ref lhs, ref rhs) => self.visit_expr(lhs) - self.visit_expr(rhs),
//         }
//     }
// }
