(* Charles-Antoine Leger - E2 - TP2 *)

(* 1.1.1. build_line function returns a string of n times the string str *)
let rec build_line n str =
  match n with
    n when n < 0 -> invalid_arg "build_line: n must be a natural"
  | 0 -> ""
  | 1 -> str
  | _ -> str ^ build_line (n - 1) str
;;

(* build_line 5 "*" ;; *)

(* 1.1.2 square function prints a square of n * n times the string str *)

let square n str =
  if n < 0 then invalid_arg "square: n must be a natural"
  else (
    let rec print_line = function
        0 -> print_string ""
      | 1 -> (
        print_string (build_line n str) ;
        print_newline()
      )
      | x -> (
        print_string (build_line n str) ;
        print_newline() ;
        print_line (x - 1)
      )
    in print_line n
  )
;;

(* square 5 "*" ;; *)

(* 1.1.3 square2 prints a square of n * n times the couple (str1, str2) *)
let square2 n (str1, str2) =
  if n < 0 then invalid_arg "square2: n must be a natural"
  else (
    let rec print_line x =
      let str =
        if x mod 2 = 1 then str1 ^ str2
        else str2 ^ str1
      in match x with
           0 -> print_string ""
         | 1 -> (
           print_string (build_line n str) ;
           print_newline()
         )
         | x -> (
           print_string (build_line n str) ;
           print_newline() ;
           print_line (x - 1)
         )
    in print_line n
  )
;;

(* square2 4 ("*", ".") ;; *)

(* 1.1.4 triangle function prints a rectangle and isoscles triangle of side length n *)
let triangle n str =
  if n < 0 then invalid_arg "triangle: n must be a natural"
  else (
  let rec print_line x =
       let a = n - (x - 1)
       in match x with
            0 -> print_string ""
           | 1 -> (
             print_string (build_line a str) ;
             print_newline()
           )
           | x ->(
             print_string (build_line a str) ;
             print_newline() ;
             print_line (x - 1)
           )
  in print_line n
  )
;;

(* triangle 6 "*" ;; *)

(* 1.2.1 pyramid function prints a pyramid build with two given strings*)
let pyramid n (str1, str2) =
  if n < 0 then invalid_arg "pyramid: n must be natural"
  else (
    let rec print_line = function
        0 -> print_string ""
      | x when x = 1 -> (
        print_string (build_line (n * 2) str2) ;
        print_newline()
      )
      | x -> (
        let n1 = x - 1 and n2 = n - (x - 1)
        in (
            print_string (
                (build_line n1 str1) ^
                  (build_line n2 str2) ^
                    (build_line n2 str2) ^
                      (build_line n1 str1)
              ) ;
            print_newline() ;
            print_line (x - 1)
          )
      )
    in print_line n
  )
;;
  
(* pyramid 6 (".", "*") ;; *)

(* 1.2.2 cross function prints a cross with two given strings *)
let cross n (str1, str2) =
  if n <= 0 then invalid_arg "cross: n must be positive"
  else (
  let rec print_line x =
       let line_part_one = build_line (x - 1) str1
       in match x with
            0 -> print_string ""
          | x when x = n -> (
            print_string (line_part_one ^ str2 ^ line_part_one) ;
            print_newline()
          )
          | x -> (
            let line = line_part_one ^ str2 ^ build_line ((2 * n) - 1 - (2 * x)) str1 ^ str2 ^ line_part_one
            in print_string line ;
               print_newline() ;
               print_line (x + 1) ;
               print_string line ;
               print_newline()
          )
  in print_line 1
)
;;

(* cross 7 (".", "&") ;; *)
