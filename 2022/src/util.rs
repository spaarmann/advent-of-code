use std::str::{pattern::Pattern, FromStr};

pub trait SplitAndParse {
    fn split_and_parse<'a, T: FromStr, P: Pattern<'a>>(
        &'a self,
        pattern: P,
    ) -> Result<Vec<T>, T::Err>;
}

impl SplitAndParse for str {
    fn split_and_parse<'a, T: FromStr, P: Pattern<'a>>(
        &'a self,
        pattern: P,
    ) -> Result<Vec<T>, T::Err> {
        self.split(pattern).map(|c| c.parse()).collect()
    }
}
