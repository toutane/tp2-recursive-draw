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

(*fleche (200, 200) 50 1 "up" ;;*)

(* draw_line_float function draws a line from (x, y) to (z, t) and takes float numbers. *)
let draw_line_float (x, y) (z, t) =
  moveto (int_of_float x) (int_of_float y) ;
  lineto (int_of_float z) (int_of_float t)
;;

(* deg_to_rad converts a degre in radian. *)
let deg_to_rad = Float.pi /. 180. ;;

(* draw_koch_curve function draws a curve of Koch of length d from (sX, sY). *) 
let rec draw_koch_curve (sX, sY) (eX, eY) i a d =
  let len = (eX -. sX) /. cos(a *. deg_to_rad) /. 3.
  in let pX = sX +. len *. cos(a *. deg_to_rad)
     and pY = sY +. len *. sin(a *. deg_to_rad)
     in let qX = pX +. len *. cos((a +. d *. 60.) *. deg_to_rad)
        and qY = pY +. len *. sin((a +. d *. 60.) *. deg_to_rad)
        in let rX = qX +. len *. cos((a +. d *. (-60.)) *. deg_to_rad)
           and rY = qY +. len *. sin(( a +. d *. (-60.)) *. deg_to_rad)
           in if i = 0 then draw_line_float (sX, sY) (eX, eY)
              else (
                draw_koch_curve (sX, sY) (pX, pY) (i - 1) (a +. 0.) d ;
                draw_koch_curve (pX, pY) (qX, qY) (i - 1) (a +. d *. (60.)) d ;
                draw_koch_curve (qX, qY) (rX, rY) (i - 1) (a +. d *. (-60.)) d ;
                draw_koch_curve (rX, rY) (eX, eY) (i - 1) (a +. 0.) d ;
              )
;;

(* koch_curve function draws a curve of Koch of length d and rank n.*)
let koch_curve d n =
  if d <= 0 then invalid_arg "koch_curve: The length of the starting segment much be positive."
  else (
    if n < 0 then invalid_arg "koch_curve: The rank of the curve must be positibe."
    else (
      clear_graph() ;
      set_color black ;
      let (x0, y0) = (150., 200.)
      and (x1, y1) = (100. +. float_of_int d, 200.)
      in draw_koch_curve (x0, y0) (x1, y1) n 0. 1.
    )
  )
;;

(*koch_curve 500 5 ;;*)

(* koch_snowflake function draws a snowflake of Koch of rank n. *)
let koch_snowflake n =
  if n < 0 then invalid_arg "koch_snowflake: The rank of the snowflake must be positive"
  else (
    clear_graph() ;
    set_color black ;
    let (x0, y0) = (200., 200.)
    and len = 500.
    in let x1 = x0 +. len *. cos(60. *. deg_to_rad)
       and y1 = y0 +. len *. sin(60. *. deg_to_rad)
       and x2 = x0 +. len
       and y2 = y0
       in draw_koch_curve (x0, y0) (x1, y1) n 60. 1. ;
          draw_koch_curve (x1, y1) (x2, y2) n (-60.) 1. ;
          draw_koch_curve (x0, y0) (x2, y2) n 0. (-1.) ;
  )
;;

(*koch_snowflake 4 ;;*)

(* vicsek_star function draws a star of Vicsek of rank n. *)
let vicsek_star n =
  if n < 0 then invalid_arg "vicsek_star: The rank of the star must be positive"
  else (
    clear_graph() ;
    set_color black ;
    let (x0, y0) = (200, 200)
    and len = 300
    in let rec draw_star (x, y) i len =
          let c = len / 3
         in let (x2, y2) = ((x + c), (y + c))
            and (x3, y3) = ((x + 2 * c), (y + 2 * c))
            in if i = 0 then (
                 fill_rect x y c c ;
                 fill_rect x2 y2 c c ;
                 fill_rect x3 y3 c c ;
                 fill_rect x y3 c c ;
                 fill_rect x3 y c c
               )
               else (
                 draw_star (x, y) (i - 1) c ;
                 draw_star (x2, y2) (i - 1) c ;
                 draw_star (x3, y3) (i - 1) c ;
                 draw_star (x, y3) (i - 1) c ;
                 draw_star (x3, y) (i - 1) c
               )
       in draw_star (x0, y0) n len
  )
;;

(*vicsek_star 3 ;;*)

(* vicsek_cross function draws a cross of Vicsek of rank n. *)
let vicsek_cross n =
  if n < 0 then invalid_arg "vicsek_cross: The rank of the cross must be positibe."
  else (
    clear_graph() ;
    set_color black ;
    let (x0, y0) = (200, 200)
    and len = 300
    in let rec draw_cross (x, y) i len =
         let c = len / 3
         in let(x2, y2) = (x + c, y + c)
            and (x3, y3) = (x + 2 * c, y + 2 * c)
            in if i = 0 then (
                 fill_rect x2 y c c ;
                 fill_rect x2 y2 c c ;
                 fill_rect x2 y3 c c ;
                 fill_rect x y2 c c ;
                 fill_rect x3 y2 c c 
               )
               else (
                 draw_cross (x2, y) (i - 1) c ;
                 draw_cross (x2, y2) (i - 1) c ;
                 draw_cross (x2, y3) (i - 1) c ;
                 draw_cross (x, y2) (i - 1) c ;
                 draw_cross (x3, y2) (i - 1) c
               )
       in draw_cross (x0, y0) n len 
  )
;;

(*vicsek_cross 5 ;;*)

(* Opens the Complex module that we use for the mandelbrot function. *)
open Complex ;;

(* mandelbrot function draws the mandelbrot set. *)
let mandelbrot =
  clear_graph() ;
  let xmin = (-2.)
  and ymin = (-2.)
  and xmax = 2.
  and ymax = 2.
  and width = 500.
  and height = 500.
  and iterations = 200
  and contrast = 5
  in let set_plot_color z = (* Set the color of each point.*)
       let rec suite v i =
         if i > iterations then i
         else (
           if Complex.norm v > 2. then (
             i
           )
           else (
             suite (Complex.add (Complex.mul v v) z) (i + 1)
           )
         )
       in let n = suite Complex.zero 0
          in if n > iterations then set_color (rgb 147 112 219) 
             else set_color (rgb (230 - contrast * n) (232 - contrast * n) (250 - contrast * n))
     in let rec draw_line py = (* Draws each line. *)
          if py < (int_of_float height) then (
            let y = ((float_of_int py) /. height *. (ymax -. ymin) +. ymin)
            in let rec draw_plot px = (* Draws each point of the line. *)
                 if px < (int_of_float width) then (
                   let x = ((float_of_int px) /. width *. (xmax -. xmin) +. xmin)
                   in let z = {re = x; im = y}
                      in set_plot_color z ;
                         plot px py ;
                         draw_plot (px + 1)
                 )
               in draw_plot 0 ;
                  draw_line (py + 1)
          )
        in draw_line 0
;;
