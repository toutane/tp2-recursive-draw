(* Initialize the random generator *)
Random.init 1 ;;

(* Load the Graphics library *)

(* Load Graphics module on OSX with ocaml 4.0.2*)
#load "graphics.cma"

(* Load Greaphics module on Linux with ocaml 4.12 *)
#use "topfind" ;;
#require "graphics" ;;

(* Open Graphics library *)
open Graphics ;;

(* Show the graphics window *)
open_graph " 800x600" ;;

(* draw_line function draws a line from point (x, y) to point (z, t) *)

let draw_line (x, y) (z, t) =
  set_line_width 1 ;
  set_color black ;
  moveto x y ;
  lineto z t
;;


(* 2.1.1 mountain function draws a mountain of n sides from point p1 to point p2 *)

let mountain n p1 p2 =
  clear_graph() ;
  let rec draw_mountain c p1 p2 =
    if c = 1 then draw_line p1 p2
    else (
      let (x, y) = p1 and (z, t) = p2
      in let h = (y + t) / 2 + (Random.int (10 * c ))
         in let m = ((x + z) / 2, h)
            in (
                draw_mountain (c - 1) p1  m ;
                draw_mountain (c - 1) m p2
              )
    )
  in draw_mountain n p1 p2
;;

mountain 10 (100, 200) (700, 200) ;;


(* 2.1.2 dragon function draws a dragon *)

let dragon n p1 p2 =
  clear_graph() ;
  let rec draw_dragon c p1 p2 =
    if c = n then draw_line p1 p2
    else (
      let (x, y) = p1 and (z, t) = p2
      in let u = ((x + z) / 2 + (t - y) / 2) and v = ((y + t) / 2 - (z - x) / 2)
         in (
             draw_dragon (c + 1) p1 (u, v) ;
             draw_dragon (c + 1) p2 (u, v)
           )
    )
  in draw_dragon 0 p1 p2
;;

dragon 19 (150 ,150) (350 ,350) ;;


(* 2.2.1 sierpinski_sponge function draws a sponge of sierpinski *)

let sierpinski_sponge (x, y) n =
  clear_graph() ;
  let rec draw_sponge c (x, y) =
    let tiere = c / 3
    in if c < 9 then (
         set_color black ;
         fill_rect x y c c ;
         set_color white ;
         fill_rect (x + tiere) (y + tiere) tiere tiere
       )
       else (
         set_color black ;
         fill_rect x y c c ;
         set_color white ;
         fill_rect (x + tiere) (y + tiere) tiere tiere ;
         draw_sponge tiere (x, y) ;
         draw_sponge tiere ((x + tiere), y) ;
         draw_sponge tiere ((x + 2 * tiere), y) ;
         draw_sponge tiere (x, (y + tiere)) ;
         draw_sponge tiere (x, (y + 2 * tiere)) ;
         draw_sponge tiere ((x + tiere), (y + 2 * tiere)) ;
         draw_sponge tiere ((x + 2 * tiere), (y + 2 * tiere)) ;
         draw_sponge tiere ((x + 2 * tiere), (y + tiere))
       )
  in draw_sponge n (x, y)
;;

sierpinski_sponge (50, 50) 500 ;;


(* sierpinski_triangle function draws a triangle of sierpinski *)

let sierpinski_triangle n (x1, y1) (x2, y2) (x3, y3) =
  clear_graph() ;
  let rec draw_triangle c (x1, y1) (x2, y2) (x3, y3) =
    let x_mid = (x2 - x1) / 2 + x1 and y_mid = (y3 - y1) / 2 + y1
    in if c = n then (
         set_color black ;
         fill_poly [|(x1, y1); (x2, y2); (x3, y3)|] ;
         set_color white ;
         fill_poly [|(x_mid, y1); ((x_mid - x1) / 2 + x1, y_mid); ((x2 - x_mid) / 2 + x_mid, y_mid)|]
       )
       else (
         set_color black ;
         fill_poly [|(x1, y1); (x2, y2); (x3, y3)|] ;
         set_color white ;
         fill_poly [|(x_mid, y1); ((x_mid - x1) / 2 + x1, y_mid); ((x2 - x_mid) / 2 + x_mid, y_mid)|] ;
         draw_triangle (c + 1) (x1, y1) (x_mid, y1) ((x_mid - x1) / 2 + x1, y_mid) ;
         draw_triangle (c + 1) (x_mid, y1) (x2, y2) ((x2 - x_mid) / 2 + x_mid, y_mid) ;
         draw_triangle (c + 1) ((x_mid - x1) / 2 + x1, y_mid) ((x2 - x_mid) / 2 + x_mid, y_mid) (x3, y3)
       )
     in draw_triangle 1 (x1, y1) (x2, y2) (x3, y3)
;;

sierpinski_triangle 5 (100, 50) (500, 50) (300, 346) ;;
