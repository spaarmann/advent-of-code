use std::{
    ops::{Index, IndexMut, Range},
    str::{pattern::Pattern, FromStr},
};

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

pub fn zip_arr<T, const N: usize>(
    mut its: [impl Iterator<Item = T>; N],
) -> impl Iterator<Item = [T; N]> {
    std::iter::from_fn(move || {
        let nexts = its.each_mut().map(|it| it.next());
        if nexts.iter().any(|n| n.is_none()) {
            None
        } else {
            Some(nexts.map(Option::unwrap))
        }
    })
}

#[derive(Clone, Debug)]
pub struct Grid<T> {
    grid: Vec<T>,
    pub width: usize,
    pub height: usize,
}

impl<T: From<char>> Grid<T> {
    pub fn from_char_grid(input: &str) -> Self {
        let width = input.lines().next().unwrap().len();
        let height = input.lines().count();

        let grid = input
            .lines()
            .map(|l| l.chars())
            .flatten()
            .map(T::from)
            .collect();
        Self {
            grid,
            width,
            height,
        }
    }
}

impl<T> Grid<T> {
    pub fn in_bounds(&self, (x, y): (i64, i64)) -> bool {
        x >= 0 && (x as usize) < self.width && y >= 0 && (y as usize) < self.height
    }

    pub fn find<P>(&self, mut predicate: P) -> Option<(i64, i64)>
    where
        P: FnMut(&T) -> bool,
    {
        for y in 0..self.height as i64 {
            for x in 0..self.width as i64 {
                if predicate(&self[(x, y)]) {
                    return Some((x, y));
                }
            }
        }
        None
    }

    pub fn positions<'a, P>(&'a self, mut predicate: P) -> impl Iterator<Item = (i64, i64)> + 'a
    where
        P: FnMut(&T) -> bool,
        P: 'a,
    {
        (0..self.height)
            .flat_map(|y| (0..self.width).map(move |x| (x, y)))
            .map(|(x, y)| (x as i64, y as i64))
            .filter(move |&(x, y)| predicate(&self[(x, y)]))
    }
}

impl<T> Index<(i64, i64)> for Grid<T> {
    type Output = T;
    fn index(&self, (x, y): (i64, i64)) -> &T {
        &self.grid[(y as usize) * self.width + x as usize]
    }
}

impl<T> Index<(usize, usize)> for Grid<T> {
    type Output = T;
    fn index(&self, (x, y): (usize, usize)) -> &T {
        &self.grid[y * self.width + x]
    }
}

impl<T> Index<(Range<i64>, i64)> for Grid<T> {
    type Output = [T];
    fn index(&self, (xs, y): (Range<i64>, i64)) -> &[T] {
        let row_start = (y as usize) * self.width;
        &self.grid[(row_start + xs.start as usize)..(row_start + xs.end as usize)]
    }
}

impl<T> IndexMut<(i64, i64)> for Grid<T> {
    fn index_mut(&mut self, (x, y): (i64, i64)) -> &mut T {
        &mut self.grid[(y as usize) * self.width + x as usize]
    }
}
