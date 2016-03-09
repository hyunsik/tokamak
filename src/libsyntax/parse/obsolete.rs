use codemap::Span;
use parse::parser;

/// The specific types of unsupported syntax
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum ObsoleteSyntax {
    ClosureKind,
    ExternCrateString,
}

pub trait ParserObsoleteMethods {
    /// Reports an obsolete syntax non-fatal error.
    fn obsolete(&mut self, sp: Span, kind: ObsoleteSyntax);
    fn report(&mut self,
              sp: Span,
              kind: ObsoleteSyntax,
              kind_str: &str,
              desc: &str,
              error: bool);
}

impl<'a> ParserObsoleteMethods for parser::Parser<'a> {
    /// Reports an obsolete syntax non-fatal error.
    fn obsolete(&mut self, sp: Span, kind: ObsoleteSyntax) {
        let (kind_str, desc, error) = match kind {
            ObsoleteSyntax::ClosureKind => (
                "`:`, `&mut:`, or `&:`",
                "rely on inference instead",
                true,
            ),
            ObsoleteSyntax::ExternCrateString => (
                "\"crate-name\"",
                "use an identifier not in quotes instead",
                false, // warning for now
            ),
        };

        self.report(sp, kind, kind_str, desc, error);
    }

    fn report(&mut self,
              sp: Span,
              kind: ObsoleteSyntax,
              kind_str: &str,
              desc: &str,
              error: bool) {
        let mut err = if error {
            self.diagnostic().struct_span_err(sp, &format!("obsolete syntax: {}", kind_str))
        } else {
            self.diagnostic().struct_span_warn(sp, &format!("obsolete syntax: {}", kind_str))
        };

        if !self.obsolete_set.contains(&kind) &&
            (error || self.sess.span_diagnostic.can_emit_warnings) {
            err.note(&format!("{}", desc));
            self.obsolete_set.insert(kind);
        }
        err.emit();
    }
}