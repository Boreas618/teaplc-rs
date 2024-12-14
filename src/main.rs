mod ast;
mod ir;
lalrpop_mod!(pub teapl);

use lalrpop_util::lalrpop_mod;
use std::fs::File;
use std::io::Read;

fn main() {
    let path = "tests/progs/dfs.tea";
    let mut file = File::open(path).unwrap();
    let mut prog = String::new();
    file.read_to_string(&mut prog).unwrap();
    let ast = teapl::ProgramParser::new().parse(&prog).unwrap();
    println!("{:#?}", ast);

    ir::gen(ast);
}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::Read;
    lalrpop_mod!(pub teapl);
    use lalrpop_util::lalrpop_mod;

    #[test]
    fn test_dfs() {
        let path = "tests/progs/dfs.tea";
        let mut file = File::open(path).unwrap();
        let mut prog = String::new();
        file.read_to_string(&mut prog).unwrap();
        assert!(teapl::ProgramParser::new().parse(&prog).is_ok());
    }
}
