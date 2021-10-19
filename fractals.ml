(* Charles-Antoine Leger - E2 - TP2 *)

(* Initializes the random generator *)
Random.init 1 ;;

(* Loads the Graphics library
#load "graphics.cma"
#use "topfind" ;;
#require "graphics" ;; *)

open Graphics ;;
(* Opens the graphics screen*)
open_graph " 1000x1000" ;;

(* draw_line function draws a line from point (x, y) to point (z, t) *)
let draw_line (x, y) (z, t) =
  moveto x y ;
  lineto z t
;;

(* 2.1.1 mountain function draws a mountain of n sides from point p1 to point p2 *)
let mountain n p q =
  if n < 0 then invalid_arg "mountain: The rank of the mountain must be a natural"
  else (
    clear_graph() ;
    set_color black ;
    let rec draw_mountain i p q =
      if i = 0 then draw_line p q
      else (
        let (x, y) = p
        and (z, t) = q
        in let h = (y + t) / 2 + Random.int(abs(z - x)/5 + 20)
           in let m = ((x + z) / 2, h)
              in draw_mountain (i - 1) p  m ;
                 draw_mountain (i - 1) m q
      )
    in draw_mountain n p q
  )
;;

(* mountain 6 (100, 300) (500, 300) ;; *)

(* 2.1.2 dragon function draws a dragon *)
let dragon n p q =
  if n < 0 then invalid_arg "dragon: The rank of the dragon must be positive."
  else (
    clear_graph() ;
    set_color red ;
    let rec draw_dragon i p q =
      if i = 0 then draw_line p q
      else (
        let (x, y) = p
        and (z, t) = q
        in let u = ((x + z) / 2 + (t - y) / 2)
           and v = ((y + t) / 2 - (z - x) / 2)
           in draw_dragon (i - 1) p (u, v) ;
              draw_dragon (i - 1) q (u, v)
      )
    in draw_dragon n p q
  )
;;

(* dragon 19 (150, 150) (350, 350) ;; *)

(* 2.2 Les surfaces *)

(* 2.2.1 sierpinski_sponge function draws a sponge of sierpinski *)
let sierpinski_sponge (x, y) width =
  if width <= 0 then invalid_arg "sierpinski_sponge: The width of the sponge must be positive"
  else (
    if x < 0 || y < 0 then invalid_arg "sierpinski_sponge: The coordonates of the origin must be positives."
    else (
      clear_graph() ;
      let rec draw_sponge w (x, y) =
        let tiere = w / 3
        in if w < 9 then (
             set_color black ;
             fill_rect x y w w ;
             set_color white ;
             fill_rect (x + tiere) (y + tiere) tiere tiere
           )
           else (
             set_color black ;
             fill_rect x y w w ;
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
      in draw_sponge width (x, y)
    )
  )
;;

(* sierpinski_sponge (50, 50) 500 ;; *)

(* sierpinski_triangle function draws a triangle of sierpinski of rank n.*)
let sierpinski_triangle n =
  if n < 0 then invalid_arg "sierpinski_triangle: The rank of the triangle must be positive."
  else (
    clear_graph() ;
    let (x1, y1) = (100, 50)
    and (x2, y2) = (500, 50)
    and (x3, y3) = (300, 346)
    in let draw_triangle (x1, y1) (x2, y2) (x3, y3) =
         draw_line (x1, y1) (x2, y2) ;
         draw_line (x1, y1) (x3, y3) ;
         draw_line (x2, y2) (x3, y3)
       in let rec draw c (x1, y1) (x2, y2) (x3, y3) =
            let x_mid = (x2 - x1) / 2 + x1
            and y_mid = (y3 - y1) / 2 + y1
            in if c = n then (
                 set_color blue ;
                 draw_triangle (x1, y1) (x2, y2) (x3, y3) ;
               )
               else (
                 set_color blue ;
                 draw_triangle (x1, y1) (x2, y2) (x3, y3) ;
                 draw (c + 1) (x1, y1) (x_mid, y1) ((x_mid - x1) / 2 + x1, y_mid) ;
                 draw (c + 1) (x_mid, y1) (x2, y2) ((x2 - x_mid) / 2 + x_mid, y_mid) ;
                 draw (c + 1) ((x_mid - x1) / 2 + x1, y_mid) ((x2 - x_mid) / 2 + x_mid, y_mid) (x3, y3)
               )
          in draw 0 (x1, y1) (x2, y2) (x3, y3)
  )
;;

(* sierpinski_triangle 6 ;; *)

(* 2.3 Bonuses *)

(* 2.3.1 circles function draws a figure of a circle contening n other circles. *)
let circles (x, y) rad =
  if rad <= 0 then invalid_arg "circles: The radius must be positive."
  else (
    if x < 0 || y < 0 then invalid_arg "circles: The coordinates of the origin must be positives."
    else (
      let limit = 5
      in clear_graph() ;
         let rec rec_circles (x, y) rad =
           let demi_rad = rad / 2
           in if demi_rad < limit then draw_circle x y rad
              else (
                draw_circle x y rad ;
                rec_circles ((x + demi_rad), y) demi_rad ;
                rec_circles ((x - demi_rad), y) demi_rad
              )
         in  rec_circles (x, y) rad
    )
  )
;;

(* circles (100, 100) 100 ;; *)

(* 2.3.1 arrow function draws a arrow of radius r *)
let arrow (ox, oy) rad =
  if rad <= 0 then invalid_arg "arrow: The radius of the arrow must be positive."
  else (
    if ox < 0 || oy < 0 then invalid_arg "arrow: The coordinates of the origin must be positives."
    else (
      let limit = 2 (* We set the radius from which we stop the recursion. *)
      and direction = "up" (* We set the starting direction of the arrow to "up".*)
      in clear_graph() ;
         set_color black ;
         let rec repeat_draw (x, y) rad direction =
           if (rad / 2) < limit then (
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
    )
  )
;;

(* arrow (200, 200) 50 ;; *)

(* draw_line_float function draws a line from (x, y) to (z, t) and takes float numbers in parameter. *)
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

(* 2.3.3 koch_curve function draws a curve of Koch of length d and rank n.*)
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

(* koch_curve 500 5 ;; *)

(* 2.3.3 koch_snowflake function draws a snowflake of Koch of rank n. *)
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

(* koch_snowflake 4 ;;*)

(* 2.3.4 vicsek_star function draws a star of Vicsek of rank n. *)
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

(* vicsek_star 3 ;; *)

(* 2.3.4 vicsek_cross function draws a cross of Vicsek of rank n. *)
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

(* 2.3.5 mandelbrot function draws the mandelbrot set. *)
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
