// Copyright (c) 2016, Sabre developers
//
// Licensed under the Apache License, Version 2.0 (see LICENSE.Apache in the
// root directory) or MIT license (see LICENSE.MIT in the root directory),
// at your option. This file may be copied, distributed, and modified only
// in accordance with the terms specified by the chosen license.

//! Pretty-printing diffs.

/// Pretty-printing sequential diffs as produced by the `diff::sequence` module.
///
/// - [`as_unified()`](fn.as_unified.html) — unified diffs with full context
///
///   ```diff
///    context
///   -old value
///   +new value
///    more context
///   ```
pub mod sequence {
    use std::fmt;

    use diff::sequence::Diff;

    pub struct UnifiedDiffDisplay<'a, T> where T: 'a {
        diff: &'a [Diff<T>],
    }

    pub struct UnifiedDiff<'a, T, F> where T: 'a {
        diff: &'a [Diff<T>],
        f: F,
    }

    pub fn unified<'a, T>(diff: &'a [Diff<T>]) -> UnifiedDiffDisplay<'a, T>
        where T: fmt::Display
    {
        UnifiedDiffDisplay { diff: diff }
    }

    pub fn unified_format<'a, T, F>(diff: &'a [Diff<T>], f: F) -> UnifiedDiff<'a, T, F>
        where F: Fn(&T, &mut fmt::Write) -> fmt::Result
    {
        UnifiedDiff { diff: diff, f: f }
    }

    impl<'a, T> fmt::Display for UnifiedDiffDisplay<'a, T>
        where T: fmt::Display
    {
        fn fmt(&self, buf: &mut fmt::Formatter) -> fmt::Result {
            write_unified(buf, self.diff, &|item, buf| write!(buf, "{}", item))
        }
    }

    impl<'a, T, F> fmt::Display for UnifiedDiff<'a, T, F>
        where F: Fn(&T, &mut fmt::Write) -> fmt::Result
    {
        fn fmt(&self, buf: &mut fmt::Formatter) -> fmt::Result {
            write_unified(buf, self.diff, &self.f)
        }
    }

    fn write_unified<T, F>(buf: &mut fmt::Write, diff: &[Diff<T>], format: &F) -> fmt::Result
        where F: Fn(&T, &mut fmt::Write) -> fmt::Result
    {
        let mut line_buffer = String::new();

        for item in diff {
            let (prefix, value) = match *item {
                Diff::Left(ref lhs)     => ("-", lhs),
                Diff::Right(ref rhs)    => ("+", rhs),
                Diff::Equal(ref eqv, _) => (" ", eqv),
            };

            line_buffer.clear();
            try!(format(value, &mut line_buffer));

            for line in line_buffer.lines() {
                try!(write!(buf, "{}{}\n", prefix, line));
            }
        }

        Ok(())
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use diff::sequence;

        #[test]
        fn unified_empty() {
            assert_eq!(format!("{}", unified::<u32>(&[])), "");
        }

        #[test]
        fn unified_sample() {
            let before = vec![
                "fn scan_bytevector_open(&mut self) -> Token {",
                "    assert!(self.cur_is('#'));",
                "    if self.ahead_is(\"u8(\") {",
                "        for _ in 0..\"#u8(\".len() { self.read(); }",
                "        return Token::OpenBytevector(ParenType::Parenthesis);",
                "    }",
                "}",
            ];

            let after = vec![
                "fn scan_bytevector_open(&mut self) -> Token {",
                "    assert!(self.cur_is('#'));",
                "    if self.try_scan(\"#u8(\") {",
                "        return Token::OpenBytevector(ParenType::Parenthesis);",
                "    }",
                "}",
            ];

            let diff = sequence::diff(&before, &after);

            assert_eq!(format!("{}", unified(&diff)),
" fn scan_bytevector_open(&mut self) -> Token {
     assert!(self.cur_is('#'));
-    if self.ahead_is(\"u8(\") {
-        for _ in 0..\"#u8(\".len() { self.read(); }
+    if self.try_scan(\"#u8(\") {
         return Token::OpenBytevector(ParenType::Parenthesis);
     }
 }
");
        }

        #[test]
        fn unified_multiline() {
            let a = vec!["a\nb\nc", "a\nb\nc\nd", "e\n", "g"];
            let b = vec!["a\nb\nc", "a\nb",       "f\n", "g"];

            let diff = sequence::diff(&a, &b);

            assert_eq!(format!("{}", unified(&diff)),
" a
 b
 c
-a
-b
-c
-d
-e
+a
+b
+f
 g
");
        }

        #[test]
        fn unified_custom_format() {
            let a: Vec<i32> = vec![0, -1,  2, -3    ];
            let b: Vec<i32> = vec![    1, -2,  3, -4];

            let diff = sequence::diff_with(&a, &b, |a, b| a.abs() == b.abs());

            assert_eq!(format!("{}", unified_format(&diff, |item, buf|
                write!(buf, "<{}>", item.abs()))),
"-<0>
 <1>
 <2>
 <3>
+<4>
");
        }
    }
}
