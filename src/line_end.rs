use std::{borrow::Cow, fmt};

use crate::utils::Text;

#[derive(Debug, Clone, PartialEq)]
pub enum LineEnd {
    /// Line Feed (LF) - Common on Unix, Linux, and macOS (`\n`).
    Lf,
    /// Carriage Return + Line Feed (CRLF) - Used on Windows (`\r\n`).
    CrLf,
}

impl fmt::Display for LineEnd {
    #[allow(clippy::write_with_newline)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LineEnd::Lf => write!(f, "\n"),
            LineEnd::CrLf => write!(f, "\r\n"),
        }
    }
}

impl LineEnd {
    /// Strip only line ending from `line`.
    ///
    /// Assumes that if line has line ending, then it is last chars.
    pub fn strip<'a, T: ?Sized + Text + ToOwned>(line: &'a Cow<'a, T>) -> Cow<'a, T> {
        let line_without_lf = line.strip_suffix("\n");
        let line_without_crlf = line_without_lf.and_then(|line| line.strip_suffix("\r"));
        let stripped_line = line_without_crlf.or(line_without_lf);

        Cow::Borrowed(stripped_line.unwrap_or(line))
    }

    /// Returns most common line ending.
    pub fn most_common<T: ?Sized + Text + ToOwned>(input: &T) -> LineEnd {
        let mut lf_score: usize = 0;
        let mut crlf_score: usize = 0;

        let mut previous_is_cr = false;
        for byte in input.as_bytes() {
            match byte {
                b'\r' => {
                    previous_is_cr = true;
                }
                b'\n' => {
                    if previous_is_cr {
                        crlf_score += 1;
                    } else {
                        lf_score += 1;
                    }
                    previous_is_cr = false;
                }
                _ => {
                    previous_is_cr = false;
                    continue;
                }
            }
        }

        #[allow(clippy::if_same_then_else)]
        if lf_score > crlf_score {
            LineEnd::Lf
        } else if lf_score < crlf_score {
            LineEnd::CrLf
        } else if cfg!(windows) {
            LineEnd::CrLf
        } else {
            LineEnd::Lf
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case("hello")]
    #[case("\r")]
    #[case("")]
    #[case("\rhello")]
    #[case("hello \r")]
    #[case("\r\nhello")]
    #[case("\nhello")]
    #[case("hello\n ")]
    #[case("hello\r\n ")]
    fn strip_no_line_ending(#[case] input: &str) {
        let input = Cow::Borrowed(input);
        let stripped = LineEnd::strip(&input);
        assert_eq!(input, stripped);
    }

    #[rstest]
    #[case("hello\n")]
    #[case("hello\r\n")]
    #[case("hello \n")]
    #[case("hello \r\n")]
    #[case("\r\nhello \n")]
    #[case("hello \r\n")]
    fn strip_line_ending(#[case] input: &str) {
        let input = Cow::Borrowed(input);
        let stripped = LineEnd::strip(&input);
        assert!(
            input.len().saturating_sub(2) <= stripped.len() && stripped.len() < input.len(),
            "Expected no newline at the end, but got: {:#?}\nOriginal line is: {:#?}",
            stripped,
            input
        );
    }

    #[rstest]
    #[case("\n\r\n")]
    #[case("")]
    #[case("\r")]
    #[case("\r\n\n")]
    #[case("\r\n\r\n\n\n")]
    #[case("\r\n \r\n\n\n")]
    fn most_common_if_eq(#[case] input: &str) {
        let most_common = LineEnd::most_common(input);
        assert_eq!(
            most_common,
            if cfg!(windows) {
                LineEnd::CrLf
            } else {
                LineEnd::Lf
            }
        );
    }

    #[rstest]
    #[case("\n\r")]
    #[case("\r\n\n\n")]
    #[case("\n\n\r\n")]
    #[case(" \n\n  \r\n ")]
    #[case("\r \n")]
    #[case("\r\n\n\n\n")]
    fn most_common_if_neq_lf(#[case] input: &str) {
        let most_common = LineEnd::most_common(input);
        assert_eq!(most_common, LineEnd::Lf);
    }

    #[rstest]
    #[case("\r\n")]
    #[case("\r\n\r\n")]
    #[case("\r\n\r\n\n")]
    #[case("\n\r\n\r\n")]
    fn most_common_if_neq_crlf(#[case] input: &str) {
        let most_common = LineEnd::most_common(input);
        assert_eq!(most_common, LineEnd::CrLf);
    }
}
