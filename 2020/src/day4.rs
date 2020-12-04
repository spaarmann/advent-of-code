use lazy_static::lazy_static;
use regex::Regex;

// This one isn't very pretty, but I didn't find time to make it better so far.

#[derive(Clone, Debug, Default)]
struct Passport<'a> {
    byr: Option<u64>,
    iyr: Option<u64>,
    eyr: Option<u64>,
    hgt: Option<&'a str>,
    hcl: Option<&'a str>,
    ecl: Option<&'a str>,
    pid: Option<&'a str>,
    cid: Option<&'a str>,
}

impl<'a> Passport<'a> {
    fn is_complete(&self) -> bool {
        self.byr.is_some()
            && self.iyr.is_some()
            && self.eyr.is_some()
            && self.hgt.is_some()
            && self.hcl.is_some()
            && self.ecl.is_some()
            && self.pid.is_some()
        // allow missing self.cid
    }

    fn is_valid(&self) -> bool {
        if !self.is_complete() {
            return false;
        }

        let byr = self.byr.unwrap();
        if byr < 1920 || byr > 2002 {
            return false;
        }

        let iyr = self.iyr.unwrap();
        if iyr < 2010 || iyr > 2020 {
            return false;
        }

        let eyr = self.eyr.unwrap();
        if eyr < 2020 || eyr > 2030 {
            return false;
        }

        let hgt = self.hgt.unwrap();
        let hgt_val: u64 = match hgt[..hgt.len() - 2].parse() {
            Ok(v) => v,
            Err(_) => return false,
        };
        if hgt.ends_with("cm") {
            if hgt_val < 150 || hgt_val > 193 {
                return false;
            }
        } else if hgt.ends_with("in") {
            if hgt_val < 59 || hgt_val > 76 {
                return false;
            }
        } else {
            return false;
        }

        lazy_static! {
            static ref HCL_RE: Regex = Regex::new(r"^#[a-f0-9]{6}$").unwrap();
        }
        if !HCL_RE.is_match(self.hcl.unwrap()) {
            return false;
        }

        match self.ecl.unwrap() {
            "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" => {}
            _ => return false,
        }

        lazy_static! {
            static ref PID_RE: Regex = Regex::new(r"^[0-9]{9}$").unwrap();
        }
        if !PID_RE.is_match(self.pid.unwrap()) {
            return false;
        }

        return true;
    }
}

fn parse(input: &str) -> Vec<Passport> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"(\w+):([#a-z0-9]+)").unwrap();
    }

    let mut passports = Vec::new();
    let mut current = Passport::default();
    for line in input.lines() {
        if line.is_empty() {
            passports.push(current);
            current = Passport::default();
            continue;
        }

        for captures in RE.captures_iter(line) {
            let key = captures.get(1).unwrap().as_str();
            let value = captures.get(2).unwrap().as_str();
            match key {
                "byr" => current.byr = Some(value.parse().unwrap()),
                "iyr" => current.iyr = Some(value.parse().unwrap()),
                "eyr" => current.eyr = Some(value.parse().unwrap()),
                "hgt" => current.hgt = Some(value),
                "hcl" => current.hcl = Some(value),
                "ecl" => current.ecl = Some(value),
                "pid" => current.pid = Some(value),
                "cid" => current.cid = Some(value),
                _ => panic!("unknown field: {}", key),
            }
        }
    }

    passports.push(current);

    passports
}

pub fn part1(input: &str) -> u64 {
    parse(input).iter().filter(|p| p.is_complete()).count() as u64
}

pub fn part2(input: &str) -> u64 {
    parse(input).iter().filter(|p| p.is_valid()).count() as u64
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE1_INPUT: &'static str = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\
                                          byr:1937 iyr:2017 cid:147 hgt:183cm\n\
\n\
                                          iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n\
                                          hcl:#cfa07d byr:1929\n\
\n\
                                          hcl:#ae17e1 iyr:2013\n\
                                          eyr:2024\n\
                                          ecl:brn pid:760753108 byr:1931\n\
                                          hgt:179cm\n\
\n\
                                          hcl:#cfa07d eyr:2025 pid:166559648\n\
                                          iyr:2011 ecl:brn hgt:59in";

    #[test]
    fn p1_input() {
        let input = std::fs::read_to_string("input/day4").expect("reading input file");
        assert_eq!(part1(&input), 233);
    }

    #[test]
    fn p1_example1() {
        assert_eq!(part1(EXAMPLE1_INPUT), 2);
    }

    #[test]
    fn p2_input() {
        let input = std::fs::read_to_string("input/day4").expect("reading input file");
        assert_eq!(part2(&input), 111);
    }

    /*#[test]
    fn p2_example1() {
        assert_eq!(part2(EXAMPLE1_INPUT), 336);
    }*/
}
