(* Initialize the random generator *)
Random.init 1 ;;

(* Load the Graphics library *)

(* Load Graphics module on OSX with ocaml 4.0.2*)
#load "graphics.cma"

(* Load Graphics module on Linux with ocaml 4.12 *)
(* #use "topfind" ;;
#require "graphics" ;; *)
open Graphics ;;

(* Show the graphics window *)
open_graph " 300x300" ;;

(* draw_line function draws a line from point (x, y) to point (z, t) *)

let draw_line (x, y) (z, t) =
  (*set_line_width 1 ;*)
  (*set_color black ;*)
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
      in let h = (y + t) / 2 + Random.int(abs(z - x)/5 + 20)
         in let m = ((x + z) / 2, h)
            in (
                draw_mountain (c - 1) p1  m ;
                draw_mountain (c - 1) m p2
              )
    )
  in draw_mountain n p1 p2
;;

mountain 10 (0, 100) (400, 100) ;;


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


(* 2.3 Bonuses *)

(* 2.3.1 Circles function draws  *)

let circles (x, y) r r_limit =
  let rec rec_circles (x, y) r =
    let demi_r = r / 2
    in if demi_r < r_limit then draw_circle x y r
       else (
         draw_circle x y r ;
         rec_circles ((x + demi_r), y) demi_r ;
         rec_circles ((x - demi_r), y) demi_r
       )
  in  rec_circles (x, y) r
;;

circles (100, 100) 100 5 ;;

(* 2.3.1 Fleche function draws a fleche of radius r *)

let fleche (ox, oy) rad n direction =
  clear_graph() ;
  set_color black ;
  let rec repeat_draw (x, y) rad direction =
    if (rad / 2) < n then (
      fill_circle x y rad
    )
    else (
      fill_circle x y rad ;
      let new_rad = rad / 2
      in let coords_left = ((x - rad) - new_rad, y)
             and coords_right = ((x + rad) + new_rad, y)
             and coords_up = (x, ((y + rad) + new_rad))
             and coords_down = (x, ((y - rad) - new_rad))
         in match direction with
           "up" -> (
           repeat_draw coords_left new_rad "left" ;
           repeat_draw coords_right new_rad "right";
           repeat_draw coords_up new_rad "up"
         )
         | "left" -> (
           repeat_draw coords_left new_rad "left";
           repeat_draw coords_up new_rad "up" ;
           repeat_draw coords_down new_rad "down"
         )
         | "right" -> (
           repeat_draw coords_right new_rad "right" ;
           repeat_draw coords_up new_rad "up" ;
           repeat_draw coords_down new_rad "down"
         )
         | _ -> (
           repeat_draw coords_left new_rad "left" ;
           repeat_draw coords_right new_rad "right" ;
           repeat_draw coords_down new_rad "down"
             )
         )
  in repeat_draw (ox, oy) rad direction
;;

fleche (200, 200) 50 1 "up" ;;


(* 2.3.3 koch_curve is a recursive function that draws a curve of von Koch of length d and order n. *)


let koch_curve (x, y) d n =
  clear_graph() ;
  let rec recursive_draw (x0, y0) (x1, y1) d n =
    let h = ((sqrt 3.) /. 2.) *. (float_of_int d) /. 3.
    in match n with
         n when n < 0 -> invalid_arg "koch_curve: n must be positive."
       | 0 -> (
         set_color black ;
         draw_line (x0, y0) (x1, y1) ;
         set_color white ;
         draw_line (x0 + d / 3, y0) (x0 + 2 * d / 3, y0) ;
         set_color black ;
         draw_line (x0 + d / 3, y0) (x0 + d / 2, y0 + int_of_float h);
         draw_line (x0 + d / 2, y0 + int_of_float h) (x0 + 2 * d / 3, y0)
       )
       | n -> (
         set_color black ;
         draw_line (x0, y0) (x1, y1);
         set_color blue ;
         if y0 = y1 then (
           draw_line (x0 + d / 3, y0) (x0 + 2 * d / 3, y1) ;
           set_color black ;
           draw_line (x0 + d / 3, y0) (x0 + d / 2, y0 + int_of_float h) ;
           draw_line (x0 + d / 2, y0 + int_of_float h) (x0 + 2 * d / 3, y0) ;
           recursive_draw (x0 + d / 3, y0) (x0 + d / 2, y0 + int_of_float h) (d / 3) (n - 1)
         )
         else (
           draw_line (x0 + d / 6, y0 + (y1 - y0) / 3) (x0 + d / 3, y0 + 2 * (y1 - y0) / 3) ;
           set_color black ;
           draw_line (,) (,) ;
           draw_line (,) (,) ;
           recursive_draw (,) (,) 
         ) 
       )
  in recursive_draw (x, y) (x + d, y) d n
;;

(* draw_line (100, 200) (600, 200) ;; *)
koch_curve (100, 300) 500 3;;
clear_graph() ;;
