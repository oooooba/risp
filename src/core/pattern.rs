use core::value::ValuePtr;

#[derive(PartialEq, Eq, Debug, Hash)]
pub enum PatternKind {
    SymbolPattern(ValuePtr),
    VectorPattern(Vec<PatternPtr>, Vec<PatternPtr>, Option<PatternPtr>),
    MapPattern(Vec<(PatternPtr, ValuePtr, Option<ValuePtr>)>, Option<PatternPtr>),
}

impl ToString for PatternKind {
    fn to_string(&self) -> String {
        use self::PatternKind::*;
        match self {
            &SymbolPattern(ref s) => s.to_string(),
            &VectorPattern(ref v, ref r, ref s) => {
                let mut text = String::new();
                text.push('[');
                let mut is_first = true;
                for item in v.iter() {
                    if is_first {
                        is_first = false;
                    } else {
                        text.push(' ');
                    }
                    text.push_str(&item.to_string());
                }
                for p in r.iter() {
                    text.push_str(" & ");
                    text.push_str(&p.to_string());
                }
                if let &Some(ref symbol) = s {
                    text.push_str(" :as ");
                    text.push_str(&symbol.to_string());
                }
                text.push(']');
                text
            }
            &MapPattern(ref _v, ref _s) => unimplemented!(),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Hash)]
pub struct Pattern {
    pub kind: PatternKind,
}

pub type PatternPtr = Box<Pattern>;

impl Pattern {
    pub fn create_symbol(symbol: ValuePtr) -> PatternPtr {
        assert!(symbol.is_symbol());
        Box::new(Pattern {
            kind: PatternKind::SymbolPattern(symbol),
        })
    }

    pub fn create_vector(patterns: Vec<PatternPtr>, rest_patterns: Vec<PatternPtr>,
                         as_symbol: Option<PatternPtr>) -> PatternPtr {
        Box::new(Pattern {
            kind: PatternKind::VectorPattern(patterns, rest_patterns, as_symbol),
        })
    }

    pub fn create_map(patterns: Vec<(PatternPtr, ValuePtr, Option<ValuePtr>)>,
                      as_symbol: Option<PatternPtr>) -> PatternPtr {
        Box::new(Pattern {
            kind: PatternKind::MapPattern(patterns, as_symbol),
        })
    }
}

impl ToString for Pattern {
    fn to_string(&self) -> String {
        self.kind.to_string()
    }
}
