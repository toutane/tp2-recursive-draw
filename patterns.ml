(* 1.1.1. build_line function returns a string of n times the string str *)

let rec build_line n str =
  if n = 1 then str
  else str ^ build_line (n - 1) str
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
       in if n = 1 then
            (
              print_string (build_line a str) ;print_newline()
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
