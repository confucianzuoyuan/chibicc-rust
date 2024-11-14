use crate::{
    error::{num_text_size, Error},
    position::Pos,
    symbol::Symbol,
    token::{Tok, Token},
};

pub type Result<T> = std::result::Result<T, Error>;

pub struct Lexer<'a> {
    bytes_iter: &'a [u8],
    pos: Pos,
    saved_pos: Pos,
}

fn is_ascii_hexdigit(c: char) -> bool {
    match c {
        '0'..='9' | 'a'..='f' | 'A'..='F' => true,
        _ => false,
    }
}

fn is_ascii_binarydigit(c: char) -> bool {
    match c {
        '0' | '1' => true,
        _ => false,
    }
}

fn is_ascii_octaldigit(c: char) -> bool {
    match c {
        '0'..='7' => true,
        _ => false,
    }
}

impl<'a> Lexer<'a> {
    pub fn new(reader: &'a [u8], filename: Symbol) -> Self {
        Lexer {
            bytes_iter: reader,
            pos: Pos::new(1, 1, 0, filename, 0),
            saved_pos: Pos::new(1, 1, 0, filename, 0),
        }
    }

    fn advance(&mut self) -> Result<()> {
        match self.bytes_iter.get(self.pos.byte as usize) {
            Some(b'\n') => {
                self.pos.line += 1;
                self.pos.column = 1;
                self.pos.byte += 1;
            }
            None => return Err(Error::Eof),
            _ => {
                self.pos.column += 1;
                self.pos.byte += 1;
            }
        }
        Ok(())
    }

    fn current_pos(&self) -> Pos {
        self.pos
    }

    fn save_start(&mut self) {
        self.saved_pos = self.current_pos();
    }

    fn current_char(&mut self) -> Result<char> {
        if let Some(&byte) = self.bytes_iter.get(self.pos.byte as usize) {
            return Ok(byte as char);
        }

        self.pos.byte += 1;
        match self.bytes_iter.get(self.pos.byte as usize) {
            Some(_) => unreachable!(),
            None => Err(Error::Eof),
        }
    }

    fn peek(&mut self) -> Option<u8> {
        if let Some(&b) = self.bytes_iter.get(self.pos.byte as usize) {
            Some(b)
        } else {
            None
        }
    }

    fn peek_next_one(&mut self) -> Option<u8> {
        if let Some(&b) = self.bytes_iter.get((self.pos.byte + 1) as usize) {
            Some(b)
        } else {
            None
        }
    }

    fn read_float(&mut self) -> Result<Token> {
        let mut buffer = String::new();
        let start_pos = self.pos.byte;
        loop {
            if let Some(b) = self.peek() {
                match b {
                    b'0'..=b'9' | b'.' => {
                        self.advance()?;
                        buffer.push(b as char)
                    }
                    _ => break,
                }
            } else {
                panic!()
            }
        }

        let num: f32 = buffer.parse().unwrap();

        self.make_token(Tok::ConstFloat(num), (self.pos.byte - start_pos) as usize)
    }

    fn read_number(&mut self) -> Result<Token> {
        // Try to parse as an integer constant.
        // 保存位置，准备回溯
        let old_pos = self.pos.byte;
        let tok = self.read_int_literal()?;
        match self.current_char()? {
            '.' | 'e' | 'E' | 'f' | 'F' => (),
            _ => return Ok(tok),
        }

        if self.current_char()? == '.' {
            self.pos.byte = old_pos;
            return self.read_float();
        }
        panic!()
    }

    fn read_int_literal(&mut self) -> Result<Token> {
        let buffer;
        let num;
        let mut base = 10;
        match self.current_char()? {
            '0' => {
                self.advance()?;
                match self.current_char()? {
                    'x' | 'X' => {
                        base = 16;
                        self.advance()?;
                        buffer = self.take_while(is_ascii_hexdigit)?;
                        num = u64::from_str_radix(&buffer, 16).unwrap();
                    }
                    'b' | 'B' => {
                        base = 2;
                        self.advance()?;
                        buffer = self.take_while(is_ascii_binarydigit)?;
                        num = u64::from_str_radix(&buffer, 2).unwrap();
                    }
                    '0'..='7' => {
                        base = 8;
                        buffer = self.take_while(is_ascii_octaldigit)?;
                        num = u64::from_str_radix(&buffer, 8).unwrap();
                    }
                    _ => {
                        num = 0 as u64;
                    }
                }
            }
            _ => {
                buffer = self.take_while(char::is_numeric)?;
                num = buffer.parse().unwrap();
            }
        }

        // Read U, L or LL suffixes
        let mut is_long = false;
        let mut is_ulong = false;
        match self.current_char()? {
            'L' | 'l' | 'U' | 'u' => {
                let t1 = self.current_char()?;
                self.advance()?;
                match self.current_char()? {
                    'L' | 'l' | 'U' | 'u' => {
                        let t2 = self.current_char()?;
                        self.advance()?;
                        match self.current_char()? {
                            'L' | 'l' | 'U' | 'u' => {
                                let t3 = self.current_char()?;
                                self.advance()?;
                                match (t1, t2, t3) {
                                    ('L', 'L', 'U')
                                    | ('L', 'L', 'u')
                                    | ('l', 'l', 'U')
                                    | ('l', 'l', 'u')
                                    | ('U', 'L', 'L')
                                    | ('U', 'l', 'l')
                                    | ('u', 'L', 'L')
                                    | ('u', 'l', 'l') => {
                                        is_long = true;
                                        is_ulong = true;
                                    }
                                    _ => (),
                                }
                            }
                            _ => match (t1, t2) {
                                ('L' | 'l', 'U' | 'u') | ('U' | 'u', 'L' | 'l') => {
                                    is_long = true;
                                    is_ulong = true;
                                }
                                ('L', 'L') | ('l', 'l') => is_long = true,
                                _ => (),
                            },
                        }
                    }
                    _ => match t1 {
                        'L' | 'l' => is_long = true,
                        'U' | 'u' => is_ulong = true,
                        _ => (),
                    },
                }
            }
            _ => (),
        }

        // Infer a type
        match base {
            10 => match (is_long, is_ulong) {
                (true, true) => return self.make_token(Tok::ConstULong(num), num_text_size(num)),
                (true, false) => {
                    return self.make_token(Tok::ConstLong(num as i64), num_text_size(num))
                }
                (false, true) => {
                    if (num >> 32) > 0 {
                        return self.make_token(Tok::ConstULong(num as u64), num_text_size(num));
                    } else {
                        return self.make_token(Tok::ConstUInt(num as u32), num_text_size(num));
                    }
                }
                _ => {
                    if (num >> 31) > 0 {
                        return self.make_token(Tok::ConstLong(num as i64), num_text_size(num));
                    } else {
                        return self.make_token(Tok::ConstInt(num as i32), num_text_size(num));
                    }
                }
            },
            _ => match (is_long, is_ulong) {
                (true, true) => {
                    return self.make_token(Tok::ConstULong(num as u64), num_text_size(num))
                }
                (true, _) => {
                    if num >> 63 > 0 {
                        return self.make_token(Tok::ConstULong(num as u64), num_text_size(num));
                    } else {
                        return self.make_token(Tok::ConstLong(num as i64), num_text_size(num));
                    }
                }
                (_, true) => {
                    if (num >> 32) > 0 {
                        return self.make_token(Tok::ConstULong(num as u64), num_text_size(num));
                    } else {
                        return self.make_token(Tok::ConstUInt(num as u32), num_text_size(num));
                    }
                }
                _ => {
                    if (num >> 63) > 0 {
                        return self.make_token(Tok::ConstULong(num as u64), num_text_size(num));
                    } else if num >> 32 > 0 {
                        return self.make_token(Tok::ConstLong(num as i64), num_text_size(num));
                    } else if num >> 31 > 0 {
                        return self.make_token(Tok::ConstUInt(num as u32), num_text_size(num));
                    } else {
                        return self.make_token(Tok::ConstInt(num as i32), num_text_size(num));
                    }
                }
            },
        }
    }

    fn simple_token(&mut self, token: Tok) -> Result<Token> {
        let mut pos = self.pos;
        pos.length = 1;
        self.advance()?;
        Ok(Token { pos, token })
    }

    fn make_token(&self, token: Tok, length: usize) -> Result<Token> {
        if length > 10000 {
            panic!();
        }
        let mut pos = self.saved_pos;
        pos.length = length;
        Ok(Token { pos, token })
    }

    fn take_while<F: Fn(char) -> bool>(&mut self, pred: F) -> Result<String> {
        self.save_start();
        let mut buffer = String::new();
        buffer.push(self.current_char()?);
        self.advance()?;
        let mut ch = self.current_char()?;
        while pred(ch) {
            buffer.push(ch);
            self.advance()?;
            ch = self.current_char()?;
        }
        Ok(buffer)
    }

    fn plus_or_plus_equal_or_plus_plus(&mut self) -> Result<Token> {
        self.two_char_token(vec![('=', Tok::PlusEqual), ('+', Tok::PlusPlus)], Tok::Plus)
    }

    fn minus_or_minus_greater_or_minus_equal_or_minus_minus(&mut self) -> Result<Token> {
        self.two_char_token(
            vec![
                ('>', Tok::MinusGreater),
                ('=', Tok::MinusEqual),
                ('-', Tok::MinusMinus),
            ],
            Tok::Minus,
        )
    }

    fn star_or_star_equal(&mut self) -> Result<Token> {
        self.two_char_token(vec![('=', Tok::StarEqual)], Tok::Star)
    }

    /// ">" | ">=" | ">>" | ">>="
    fn greater_or_other(&mut self) -> Result<Token> {
        self.advance()?;
        match self.current_char()? {
            '=' => {
                self.advance()?;
                self.make_token(Tok::GreaterEqual, 2)
            }
            '>' => {
                self.advance()?;
                match self.current_char()? {
                    '=' => {
                        self.advance()?;
                        self.make_token(Tok::GreaterGreaterEqual, 3)
                    }
                    _ => self.make_token(Tok::GreaterGreater, 2),
                }
            }
            _ => self.make_token(Tok::Greater, 1),
        }
    }

    /// "<" | "<=" | "<<" | "<<="
    fn lesser_or_other(&mut self) -> Result<Token> {
        self.advance()?;
        match self.current_char()? {
            '=' => {
                self.advance()?;
                self.make_token(Tok::LesserEqual, 2)
            }
            '<' => {
                self.advance()?;
                match self.current_char()? {
                    '=' => {
                        self.advance()?;
                        self.make_token(Tok::LesserLesserEqual, 3)
                    }
                    _ => self.make_token(Tok::LesserLesser, 2),
                }
            }
            _ => self.make_token(Tok::Lesser, 1),
        }
    }

    /// "." | "..."
    fn dot_or_other(&mut self) -> Result<Token> {
        self.advance()?;
        match self.current_char()? {
            '.' => {
                self.advance()?;
                match self.current_char()? {
                    '.' => {
                        self.advance()?;
                        self.make_token(Tok::DotDotDot, 3)
                    }
                    _ => panic!(),
                }
            }
            _ => self.make_token(Tok::Dot, 1),
        }
    }

    fn bang_or_bang_equal(&mut self) -> Result<Token> {
        self.two_char_token(vec![('=', Tok::BangEqual)], Tok::Bang)
    }

    fn equal_or_equal_equal(&mut self) -> Result<Token> {
        self.two_char_token(vec![('=', Tok::EqualEqual)], Tok::Equal)
    }

    fn percent_or_percent_equal(&mut self) -> Result<Token> {
        self.two_char_token(vec![('=', Tok::PercentEqual)], Tok::Percent)
    }

    fn hat_or_hat_equal(&mut self) -> Result<Token> {
        self.two_char_token(vec![('=', Tok::HatEqual)], Tok::Hat)
    }

    fn amp_or_amp_equal_or_amp_amp(&mut self) -> Result<Token> {
        self.two_char_token(vec![('=', Tok::AmpEqual), ('&', Tok::AmpAmp)], Tok::Amp)
    }

    fn bar_or_bar_equal_or_bar_bar(&mut self) -> Result<Token> {
        self.two_char_token(vec![('=', Tok::BarEqual), ('|', Tok::BarBar)], Tok::Bar)
    }

    fn eat(&mut self, ch: char) -> Result<()> {
        if self.current_char()? != ch {
            panic!(
                "Expected character `{}`, but found `{}`.",
                ch,
                self.current_char()?
            );
        }
        self.advance()
    }

    fn identifier(&mut self) -> Result<Token> {
        let ident = self.take_while(|ch| ch.is_alphanumeric() || ch == '_')?;
        let len = ident.len();
        let token = match ident.as_str() {
            "if" => Tok::KeywordIf,
            "else" => Tok::KeywordElse,
            "return" => Tok::KeywordReturn,
            "for" => Tok::KeywordFor,
            "while" => Tok::KeywordWhile,
            "do" => Tok::KeywordDo,
            "int" => Tok::KeywordInt,
            "char" => Tok::KeywordChar,
            "long" => Tok::KeywordLong,
            "short" => Tok::KeywordShort,
            "sizeof" => Tok::KeywordSizeof,
            "struct" => Tok::KeywordStruct,
            "union" => Tok::KeywordUnion,
            "void" => Tok::KeywordVoid,
            "typedef" => Tok::KeywordTypedef,
            "_Bool" => Tok::KeywordBool,
            "enum" => Tok::KeywordEnum,
            "static" => Tok::KeywordStatic,
            "goto" => Tok::KeywordGoto,
            "break" => Tok::KeywordBreak,
            "continue" => Tok::KeywordContinue,
            "switch" => Tok::KeywordSwitch,
            "default" => Tok::KeywordDefault,
            "case" => Tok::KeywordCase,
            "extern" => Tok::KeywordExtern,
            "_Alignof" => Tok::KeywordAlignof,
            "_Alignas" => Tok::KeywordAlignas,
            "signed" => Tok::KeywordSigned,
            "unsigned" => Tok::KeywordUnsigned,
            "const" => Tok::KeywordConst,
            "volatile" => Tok::KeywordVolatile,
            "auto" => Tok::KeywordAuto,
            "register" => Tok::KeywordRegister,
            "restrict" => Tok::KeywordRestrict,
            "__restrict" => Tok::KeywordRestrict1,
            "__restrict__" => Tok::KeywordRestrict2,
            "_Noreturn" => Tok::KeywordNoreturn,
            _ => Tok::Ident(ident),
        };
        self.make_token(token, len)
    }

    fn skip_until_slash(&mut self) -> Result<()> {
        loop {
            let ch = self.current_char()?;
            if ch == '\\' {
                self.advance()?;
                break;
            } else if !ch.is_whitespace() {
                let mut pos = self.current_pos();
                pos.length = 1;
                return Err(Error::InvalidToken { pos, start: ch });
            }
            self.advance()?;
        }
        Ok(())
    }

    fn escape_ascii_code(&mut self, mut pos: Pos) -> Result<char> {
        let buffer = self.take_while(char::is_numeric)?;
        if buffer.len() == 3 {
            let ascii_code = buffer.parse().unwrap();
            match char::from_u32(ascii_code) {
                Some(ch) => Ok(ch),
                None => Err(Error::Msg(format!("Invalid ascii code {}", ascii_code))),
            }
        } else {
            pos.length = buffer.len() + 1;
            Err(Error::InvalidEscape {
                escape: buffer,
                pos,
            })
        }
    }

    fn from_hex(&mut self, c: char) -> i32 {
        if c >= '0' && c <= '9' {
            return c as i32 - '0' as i32;
        }
        if c >= 'a' && c <= 'f' {
            return c as i32 - 'a' as i32 + 10;
        }
        c as i32 - 'A' as i32 + 10
    }

    fn escape_char(&mut self, pos: Pos) -> Result<char> {
        let escaped_char = match self.current_char()? {
            // Read an octal number.
            '0'..='7' => {
                let mut c = self.current_char()? as i32 - '0' as i32;
                self.advance()?;
                if self.current_char()? <= '7' && self.current_char()? >= '0' {
                    c = (c << 3) + (self.current_char()? as i32 - '0' as i32);
                    self.advance()?;
                    if self.current_char()? <= '7' && self.current_char()? >= '0' {
                        c = (c << 3) + (self.current_char()? as i32 - '0' as i32);
                        self.advance()?;
                    }
                }
                return Ok(c as u8 as char);
            }
            // Read a hexadecimal number.
            'x' => {
                self.advance()?;
                if self.current_char()?.is_ascii_hexdigit() {
                    let mut c = 0;
                    loop {
                        if self.current_char()?.is_ascii_hexdigit() {
                            let _c = self.current_char()?;
                            c = (c << 4) + self.from_hex(_c);
                            self.advance()?;
                        } else {
                            break;
                        }
                    }

                    return Ok(c as u8 as char);
                } else {
                    panic!("invalid hex escape sequence.");
                }
            }
            'n' => '\n',
            't' => '\t',
            'r' => '\r',
            'a' => '\x07',
            'b' => '\x08',
            // [GNU] \e for the ASCII escape character is a GNU C extension.
            'e' => '\x1B',
            'v' => '\x0B',
            'f' => '\x0C',
            '\\' => '\\',
            '"' => '"',
            ch if ch.is_digit(10) => return self.escape_ascii_code(pos),
            escape => return Ok(escape),
        };
        self.advance()?;
        Ok(escaped_char)
    }

    fn string(&mut self) -> Result<Token> {
        let result = (|| {
            self.save_start();
            let mut string = String::new();
            let start = self.current_pos().byte;
            self.eat('"')?;
            let mut ch = self.current_char()?;
            while ch != '"' {
                // look for escaped character.
                if ch == '\\' {
                    let pos = self.current_pos();
                    self.advance()?;
                    if self.current_char()?.is_whitespace() {
                        self.skip_until_slash()?;
                    } else {
                        string.push(self.escape_char(pos)?);
                    }
                } else {
                    string.push(ch);
                    self.advance()?;
                }
                ch = self.current_char()?;
            }
            self.eat('"')?;
            let len = self.current_pos().byte - start;
            self.make_token(Tok::Str(string), len as usize)
        })();
        match result {
            Err(Error::Eof)
            | Ok(Token {
                token: Tok::EndOfFile,
                ..
            }) => {
                let mut pos = self.saved_pos;
                pos.length = 1;
                Err(Error::Unclosed {
                    pos,
                    token: "string",
                })
            }
            _ => result,
        }
    }

    fn comment(&mut self) -> Result<()> {
        loop {
            self.advance()?;
            let ch = self.current_char()?;
            if ch == '*' {
                self.advance()?;
                let ch = self.current_char()?;
                if ch == '/' {
                    self.advance()?;
                    break;
                }
            }
        }
        Ok(())
    }

    fn slash_or_comment_or_slash_equal(&mut self) -> Result<Token> {
        self.save_start();
        self.advance()?;
        if self.current_char()? == '*' {
            match self.comment() {
                Err(Error::Eof) => {
                    let mut pos = self.saved_pos;
                    pos.length = 2;
                    return Err(Error::Unclosed {
                        pos,
                        token: "comment",
                    });
                }
                Err(error) => return Err(error),
                _ => (),
            }
            self.token()
        } else if self.current_char()? == '/' {
            loop {
                let ch = self.current_char()?;
                if ch == '\n' {
                    break;
                } else {
                    self.advance()?;
                }
            }
            self.token()
        } else if self.current_char()? == '=' {
            self.advance()?;
            self.make_token(Tok::SlashEqual, 2)
        } else {
            self.make_token(Tok::Slash, 1)
        }
    }

    fn char_literal(&mut self) -> Result<Token> {
        self.eat('\'')?;
        if self.current_char()? == '\0' {
            return Err(Error::Unclosed {
                pos: self.current_pos(),
                token: "unclosed char literal",
            });
        }

        let c;
        if self.current_char()? == '\\' {
            self.eat('\\')?;
            c = self.escape_char(self.current_pos())?;
        } else {
            c = self.current_char()?;
            self.advance()?;
        }

        self.eat('\'')?;
        self.make_token(Tok::ConstInt(c as i8 as i32), 1)
    }

    pub fn token(&mut self) -> Result<Token> {
        if let Some(ch) = self.peek() {
            return match ch {
                b' ' | b'\n' | b'\t' => {
                    self.advance()?;
                    self.token()
                }
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.identifier(),
                b'0'..=b'9' => self.read_number(),
                b'.' => {
                    if let Some(b) = self.peek_next_one() {
                        match b {
                            b'0'..=b'9' => self.read_number(),
                            _ => self.dot_or_other(),
                        }
                    } else {
                        panic!()
                    }
                }
                b'+' => self.plus_or_plus_equal_or_plus_plus(),
                b'-' => self.minus_or_minus_greater_or_minus_equal_or_minus_minus(),
                b'*' => self.star_or_star_equal(),
                b'/' => self.slash_or_comment_or_slash_equal(),
                b'(' => self.simple_token(Tok::LeftParen),
                b')' => self.simple_token(Tok::RightParen),
                b'{' => self.simple_token(Tok::LeftBrace),
                b'}' => self.simple_token(Tok::RightBrace),
                b'[' => self.simple_token(Tok::LeftBracket),
                b']' => self.simple_token(Tok::RightBracket),
                b'>' => self.greater_or_other(),
                b'<' => self.lesser_or_other(),
                b'!' => self.bang_or_bang_equal(),
                b'=' => self.equal_or_equal_equal(),
                b';' => self.simple_token(Tok::Semicolon),
                b',' => self.simple_token(Tok::Comma),
                b'&' => self.amp_or_amp_equal_or_amp_amp(),
                b'|' => self.bar_or_bar_equal_or_bar_bar(),
                b'^' => self.hat_or_hat_equal(),
                b'~' => self.simple_token(Tok::Tilde),
                b'%' => self.percent_or_percent_equal(),
                b':' => self.simple_token(Tok::Colon),
                b'?' => self.simple_token(Tok::QuestionMark),
                b'"' => self.string(),
                b'\'' => self.char_literal(),
                b'\0' => self.simple_token(Tok::EndOfFile),
                _ => {
                    let mut pos = self.current_pos();
                    pos.length = 1;
                    Err(Error::InvalidToken {
                        pos,
                        start: ch as char,
                    })
                }
            };
        } else {
            panic!()
        }
    }

    fn two_char_token(&mut self, tokens: Vec<(char, Tok)>, default: Tok) -> Result<Token> {
        self.save_start();
        self.advance()?;
        let token = match self.peek() {
            Some(byte) => {
                let mut token = None;
                let next_char = byte as char;
                for (ch, tok) in tokens {
                    if ch == next_char {
                        token = Some(tok);
                    }
                }
                token
            }
            _ => None,
        };
        let (token, len) = if let Some(token) = token {
            self.advance()?;
            (token, 2)
        } else {
            (default, 1)
        };
        self.make_token(token, len)
    }

    pub fn lex(&mut self) -> Result<Vec<Token>> {
        let mut tokens = vec![];
        loop {
            let tok = self.token()?;
            tokens.push(tok.clone());
            if tok.token == Tok::EndOfFile {
                break;
            }
        }
        Ok(tokens)
    }
}
