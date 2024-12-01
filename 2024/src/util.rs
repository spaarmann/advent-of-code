use std::{
    fmt::Debug,
    ops::{Index, IndexMut, Range},
    str::{FromStr, pattern::Pattern},
};

use itertools::Itertools;

pub trait SplitAndParse {
    fn split_and_parse<'a, T: FromStr, P: Pattern + 'a>(
        &'a self,
        pattern: P,
    ) -> impl Iterator<Item = T> + 'a
    where
        T::Err: Debug;

    fn split_once_and_parse<T: FromStr, P: Pattern>(&self, pattern: P) -> (T, T)
    where
        T::Err: Debug;
}

impl SplitAndParse for str {
    fn split_and_parse<'a, T: FromStr, P: Pattern + 'a>(
        &'a self,
        pattern: P,
    ) -> impl Iterator<Item = T> + 'a
    where
        T::Err: Debug,
    {
        self.split(pattern).map(|c| c.parse().unwrap())
    }

    fn split_once_and_parse<T: FromStr, P: Pattern>(&self, pattern: P) -> (T, T)
    where
        T::Err: Debug,
    {
        let (l, r) = self.split_once(pattern).unwrap();
        (l.parse().unwrap(), r.parse().unwrap())
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

#[derive(PartialEq, Eq, Hash, Debug)]
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

impl<T> Grid<T>
where
    for<'a> &'a T: Into<char>,
{
    #[allow(dead_code)] // Usually only used temporarily for debugging
    pub fn to_char_grid(&self) -> String {
        (0..self.height)
            .map(|y| {
                self.grid[y * self.width..(y + 1) * self.width]
                    .iter()
                    .map(|t| <&T as Into<char>>::into(t))
                    .collect::<String>()
            })
            .join("\n")
    }
}

impl<T: Clone> Clone for Grid<T> {
    fn clone(&self) -> Grid<T> {
        Self {
            width: self.width,
            height: self.height,
            grid: self.grid.clone(),
        }
    }

    fn clone_from(&mut self, source: &Self) {
        self.width = source.width;
        self.height = source.height;
        self.grid.clone_from(&source.grid);
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

    pub fn rows(&self) -> impl Iterator<Item = impl Iterator<Item = &T> + '_> {
        (0..self.height).map(move |y| (0..self.width).map(move |x| &self[(x, y)]))
    }

    pub fn cols(&self) -> impl Iterator<Item = impl Iterator<Item = &T> + '_> {
        (0..self.width).map(move |x| (0..self.height).map(move |y| &self[(x, y)]))
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
