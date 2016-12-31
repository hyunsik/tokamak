use std::io;

use hir;
use parser::ast;
use parser::ast_printer as ast_pp;
use parser::ast_printer as pp;
use parser::ast_printer::eof;
use parser::comments;
use parser::codemap::CodeMap;

pub enum AnnNode<'a> {
  NodeName(&'a ast::Name),
}

pub enum Nested {}

pub trait PpAnn {
    fn nested(&self, _state: &mut State, _nested: Nested) -> io::Result<()> {
        Ok(())
    }
    fn pre(&self, _state: &mut State, _node: AnnNode) -> io::Result<()> {
        Ok(())
    }
    fn post(&self, _state: &mut State, _node: AnnNode) -> io::Result<()> {
        Ok(())
    }
}

pub struct NoAnn;
impl PpAnn for NoAnn {}
pub const NO_ANN: &'static PpAnn = &NoAnn;

pub struct State<'a> {
    pub s: pp::Printer<'a>,
    cm: Option<&'a CodeMap>,
    comments: Option<Vec<comments::Comment>>,
    literals: Option<Vec<comments::Literal>>,
    cur_cmnt_and_lit: ast_pp::CurrentCommentAndLiteral,
    boxes: Vec<pp::Breaks>,
    ann: &'a (PpAnn + 'a),
}

impl<'a> State<'a> {
  pub fn print_path(&mut self,
                      path: &hir::Path,
                      colons_before_params: bool)
                      -> io::Result<()> {
      unimplemented!()
  }

  pub fn print_stmt(&mut self, st: &hir::Stmt) -> io::Result<()> {
      unimplemented!()
  }

  pub fn print_pat(&mut self, pat: &hir::Pat) -> io::Result<()> {
      unimplemented!()
  }

  pub fn print_expr(&mut self, expr: &hir::Expr) -> io::Result<()> {
      unimplemented!()
  }

  pub fn print_type(&mut self, ty: &hir::Ty) -> io::Result<()> {
      unimplemented!()
  }
}


#[allow(non_upper_case_globals)]
pub const indent_unit: usize = 4;

#[allow(non_upper_case_globals)]
pub const default_columns: usize = 78;

pub fn to_string<F>(ann: &PpAnn, f: F) -> String
    where F: FnOnce(&mut State) -> io::Result<()>
{
    let mut wr = Vec::new();
    {
        let mut printer = State {
            s: pp::mk_printer(Box::new(&mut wr), default_columns),
            cm: None,
            comments: None,
            literals: None,
            cur_cmnt_and_lit: ast_pp::CurrentCommentAndLiteral {
                cur_cmnt: 0,
                cur_lit: 0,
            },
            boxes: Vec::new(),
            ann: ann,
        };
        f(&mut printer).unwrap();
        eof(&mut printer.s).unwrap();
    }
    String::from_utf8(wr).unwrap()
}