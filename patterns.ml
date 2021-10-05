(* 1.1.1. build_line function returns a string of n times the string str *)

let rec build_line n str =
  match n with
    0 -> ""
  | 1 -> str
  | _ -> str ^ build_line (n - 1) str
;;

build_line 5 "*" ;;
build_line 4 ".*" ;;


(* 1.1.2 square function prints a square of n * n times the string str *)

let square n str =
  let x = n
  in let rec print_line n =
       if n = 1 then (
         print_string (build_line x str) ;
         print_newline()
       )
       else (
         print_string (build_line x str) ;
         print_newline() ;
         print_line (n - 1)
       )
     in print_line n
;;

square 5 "*" ;;
square 6 "@" ;;


(* 1.1.3 square2 prints a square of n * n times the tupple (str1, str2) *)

let square2 n (str1, str2) =
  let x = n
  in let rec print_line n =
       let str =
         if n mod 2 = 1 then str1 ^ str2
         else str2 ^ str1
       in if n = 1 then (
            print_string (build_line x str) ;
            print_newline()
          )
          else (
            print_string (build_line x str) ;
            print_newline() ;
            print_line (n - 1)
          )
     in print_line n
;;

square2 5 ("*", ".") ;;
square2 6 ("&", " ") ;;


(* 1.1.4 triangle function prints a rectangle and isoscles triangle of side length n *)

let triangle n str =
  let x = n
  in let rec print_line n =
       let a = x - (n - 1)
       in if n = 1 then (
            print_string (build_line a str) ;
            print_newline()
          )
          else (
            print_string (build_line a str) ;
            print_newline() ;
            print_line (n - 1)
          )
     in print_line n
;;

triangle 5 "*" ;;
triangle 6 "+" ;;


(* 1.2.1 pyramid function prints a pyramid *)

let pyramid n (str1, str2) =
  let x = n
  in let rec print_line n =
       if n = 1 then (
         print_string (build_line (x * 2) str2) ;
         print_newline()
       )
       else (
         let n1 = n - 1 and n2 = x - (n - 1)
         in (
             print_string (
                 (build_line n1 str1) ^
                   (build_line n2 str2) ^
                     (build_line n2 str2) ^
                       (build_line n1 str1)
               ) ;
             print_newline() ;
             print_line (n - 1)
           )
       )
     in print_line n
;;

pyramid 5 (".", "*") ;;
pyramid 6 ("-", "|") ;;


(* 1.2.2 cross function prints a cross *)

let cross n (str1, str2) =
  let x = n
  in let rec print_line n =
       let line_part_one = build_line (n - 1) str1
       in if n = x then (
            print_string (line_part_one ^ str2 ^ line_part_one) ;
            print_newline()
          )
       else (
            let line = (
                line_part_one ^ str2 ^
                  build_line ((2 * x) - 1 - (2 * n)) str1 ^
                    str2 ^ line_part_one
              )
            in (
                print_string line ;
                print_newline() ;
                print_line (n + 1) ;
                print_string line ;
                print_newline()
              )
          )
     in print_line 1
;;

cross 5 (".", "&") ;;
cross 6 (" ", "o") ;;
