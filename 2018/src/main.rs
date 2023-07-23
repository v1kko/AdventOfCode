use std::env;
mod problem_1;
mod problem_2;
mod problem_3;
mod problem_4;
mod problem_5;
mod problem_6;
mod problem_7;
mod problem_8;
mod problem_9;
mod problem_10;
mod problem_11;
mod problem_12;
mod problem_13;
mod problem_14;

fn main() {
    let problems: Vec<(&str,&dyn Fn())> = 
        vec![ (" 1a",&problem_1::a),
              (" 1b",&problem_1::b),
              (" 2a",&problem_2::a),
              (" 2b",&problem_2::b),
              (" 3a",&problem_3::a),
              (" 3b",&problem_3::b),
              (" 4a",&problem_4::a),
              (" 4b",&problem_4::b),
              (" 5a",&problem_5::a),
              (" 5b",&problem_5::b),
              (" 6a",&problem_6::a),
              (" 6b",&problem_6::b),
              (" 7a",&problem_7::a),
              (" 7b",&problem_7::b),
              (" 8a",&problem_8::a),
              (" 8b",&problem_8::b),
              (" 9a",&problem_9::a),
              (" 9b",&problem_9::b),
              ("10a",&problem_10::a),
              ("10b",&problem_10::b),
              ("11a",&problem_11::a),
              ("11b",&problem_11::b),
              ("12a",&problem_12::a),
              ("12b",&problem_12::b),
              ("13a",&problem_13::a),
              ("13b",&problem_13::b),
              ("14a",&problem_14::a),
              ("14b",&problem_14::b),
            ];
    let args : Vec<String> = env::args().collect();
    if args.len() == 1 {
      for pair in problems {
          let (title, problem) = pair;
          print!("problem {title}: ");
          problem();
      }
    } else {
      for arg in &args[1..] {
        let problem = arg.parse::<usize>().unwrap();
        let index = (problem-1)*2;
        for i in index..index+2 { 
          if let Some(problem) = problems.get(i) {
            let (title, problem) = problem;
            print!("problem {title}: ");
            problem();
          } else {
            println!("problem {} is not available",problem);
            break;
          }
        }
      }
    }
}
