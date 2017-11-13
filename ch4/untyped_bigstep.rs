use std::rc::Rc;

#[derive(Clone, Debug)]
enum Term {
    T,
    F,
    IfThenElse(Rc<Term>, Rc<Term>, Rc<Term>),
    Z,
    Succ(Rc<Term>),
    Pred(Rc<Term>),
    IsZero(Rc<Term>),
}

impl Term {
    fn is_value(&self) -> bool {
        match *self {
            Term::T | Term::F | Term::Z => true,
            Term::Succ(ref x) => x.is_numeric_value(),
            _ => false,
        }
    }

    fn is_numeric_value(&self) -> bool {
        match *self {
            Term::Z => true,
            Term::Succ(ref x) => x.is_numeric_value(),
            _ => false,
        }
    }

    fn eval(&self) -> Term {
        match *self {
            // B-Value
            ref v if v.is_value() => v.clone(),

            // B-IfTrue & B-IfFalse
            Term::IfThenElse(ref c, ref t, ref f) => match c.eval() {
                Term::T => (**t).clone(),
                Term::F => (**f).clone(),
                _ => self.clone(),
            },

            // B-Succ
            Term::Succ(ref x) => {
                let v = x.eval();
                if v.is_numeric_value() {
                    v
                } else {
                    self.clone()
                }
            },

            Term::Pred(ref x) => match x.eval() {
                Term::Z => Term::Z,
                Term::Succ(ref x) if x.is_numeric_value() => (**x).clone(),
                _ => self.clone(),
            },

            Term::IsZero(ref x) => match x.eval() {
                Term::Z => Term::T,
                Term::Succ(ref x) if x.is_numeric_value() => Term::F,
                _ => self.clone(),
            },

            // unreachable
            _ => self.clone(),
        }
    }
}

fn main() {
    let ex = Term::IfThenElse(
        Rc::new(Term::IsZero(Rc::new(Term::Succ(Rc::new(Term::Z))))),
        Rc::new(Term::Pred(Rc::new(Term::Succ(Rc::new(Term::Z))))),
        Rc::new(Term::Succ(Rc::new(Term::Succ(Rc::new(Term::Z))))));
    println!("ex = {:?}", ex);
    println!("ex.eval() = {:?}", ex.eval());
}
