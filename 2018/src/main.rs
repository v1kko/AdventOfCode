mod problem_1;
mod problem_2;

fn main() {
    let problems: Vec<(&str,&dyn Fn())> = 
            vec![("1a",&problem_1::a)
                ,("1b",&problem_1::b)
                ,("2a",&problem_2::a)
                ,("2b",&problem_2::b)
                
                ];

    for pair in problems {
        let (title, problem) = pair;
        println!("problem {title}:");
        problem();
    }
}
