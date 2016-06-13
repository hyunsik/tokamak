use std::usize;

use ast;
use lexer;

pub fn is_doc_comment(s: &str) -> bool {
    (s.starts_with("///") && lexer::is_doc_comment(s)) || s.starts_with("//!") ||
    (s.starts_with("/**") && lexer::is_block_doc_comment(s)) || s.starts_with("/*!")
}

pub fn doc_comment_style(comment: &str) -> ast::AttrStyle {
    assert!(is_doc_comment(comment));
    if comment.starts_with("//!") || comment.starts_with("/*!") {
        ast::AttrStyle::Inner
    } else {
        ast::AttrStyle::Outer
    }
}

pub fn strip_doc_comment_decoration(comment: &str) -> String {
    /// remove whitespace-only lines from the start/end of lines
    fn vertical_trim(lines: Vec<String>) -> Vec<String> {
        let mut i = 0;
        let mut j = lines.len();
        // first line of all-stars should be omitted
        if !lines.is_empty() && lines[0].chars().all(|c| c == '*') {
            i += 1;
        }
        while i < j && lines[i].trim().is_empty() {
            i += 1;
        }
        // like the first, a last line of all stars should be omitted
        if j > i &&
        lines[j - 1]
        .chars()
        .skip(1)
        .all(|c| c == '*') {
            j -= 1;
        }
        while j > i && lines[j - 1].trim().is_empty() {
            j -= 1;
        }
        lines[i..j].iter().cloned().collect()
    }

    /// remove a "[ \t]*\*" block from each line, if possible
    fn horizontal_trim(lines: Vec<String>) -> Vec<String> {
        let mut i = usize::MAX;
        let mut can_trim = true;
        let mut first = true;
        for line in &lines {
            for (j, c) in line.chars().enumerate() {
                if j > i || !"* \t".contains(c) {
                    can_trim = false;
                    break;
                }
                if c == '*' {
                    if first {
                        i = j;
                        first = false;
                    } else if i != j {
                        can_trim = false;
                    }
                    break;
                }
            }
            if i > line.len() {
                can_trim = false;
            }
            if !can_trim {
                break;
            }
        }

        if can_trim {
            lines.iter()
            .map(|line| (&line[i + 1..line.len()]).to_string())
            .collect()
        } else {
            lines
        }
    }

    // one-line comments lose their prefix
    const ONELINERS: &'static [&'static str] = &["///!", "///", "//!", "//"];
    for prefix in ONELINERS {
        if comment.starts_with(*prefix) {
            return (&comment[prefix.len()..]).to_string();
        }
    }

    if comment.starts_with("/*") {
        let lines = comment[3..comment.len() - 2]
        .lines()
        .map(|s| s.to_string())
        .collect::<Vec<String>>();

        let lines = vertical_trim(lines);
        let lines = horizontal_trim(lines);

        return lines.join("\n");
    }

    panic!("not a doc-comment: {}", comment);
}