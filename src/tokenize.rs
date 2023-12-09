use std::str::CharIndices;

#[derive(Debug, PartialEq)]
pub enum Token {
    LeftParen(usize),
    RightParen(usize),
    String(usize, usize),
    Word(usize, usize),
}

pub struct TokenStream<'a> {
    source: &'a str,
    iter: CharIndices<'a>,
    pending: Option<(usize, char)>,
}

impl<'a> TokenStream<'a> {
    fn next_char(&mut self) -> Option<(usize, char)> {
        if let Some(x) = self.pending {
            self.pending = None;
            return Some(x);
        }

        self.iter.next()
    }

    pub fn source_range(&self, first: usize, last: usize) -> &'a str {
        &self.source[first..last]
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let mut in_progress = false;
        let mut start_idx: usize = 0;

        let mut string_in_progress = false;

        loop {
            match self.next_char() {
                Some((index, current)) => {
                    if string_in_progress {
                        if current == '"' {
                            return Some(Token::String(start_idx, index + 1));
                        }
                    } else {
                        match current {
                            '(' => {
                                if in_progress {
                                    self.pending = Some((index, current));
                                    return Some(Token::Word(start_idx, index));
                                }
                                return Some(Token::LeftParen(index));
                            }
                            ')' => {
                                if in_progress {
                                    self.pending = Some((index, current));
                                    return Some(Token::Word(start_idx, index));
                                }
                                return Some(Token::RightParen(index));
                            }
                            '"' => {
                                if in_progress {
                                    self.pending = Some((index, current));
                                    return Some(Token::Word(start_idx, index));
                                }
                                string_in_progress = true;
                                start_idx = index;
                            }
                            ' ' | '\t' | '\r' | '\n' => {
                                if in_progress {
                                    return Some(Token::Word(start_idx, index));
                                }
                            }
                            _ => {
                                if !in_progress {
                                    start_idx = index
                                }
                                in_progress = true
                            }
                        }
                    }
                }
                None => {
                    if in_progress {
                        return Some(Token::Word(start_idx, self.source.len()));
                    } else {
                        return None;
                    }
                }
            }
        }
    }
}

pub fn tokenize<'a>(program: &'a str) -> TokenStream<'a> {
    TokenStream {
        source: program,
        pending: None,
        iter: program.char_indices(),
    }
}

#[test]
fn test_tokenization() {
    let tokens: Vec<Token> = tokenize("( hello )").collect();

    assert_eq!(
        tokens,
        vec![
            Token::LeftParen(0),
            Token::Word(2, 2 + 5),
            Token::RightParen(8)
        ]
    )
}

#[test]
fn test_tokenization_no_spaces() {
    let tokens: Vec<Token> = tokenize("(hello)").collect();

    assert_eq!(
        tokens,
        vec![Token::LeftParen(0), Token::Word(1, 6), Token::RightParen(6)]
    )
}
