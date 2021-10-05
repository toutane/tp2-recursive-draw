(* Initialize the random generator *)
Random.init 1 ;;

(* Load the Graphics library *)
#load "graphics.cma"
open Graphics ;;

(* Show the graphics window *)
open_graph " 800x600" ;;

(* draw_line function draw a line from point (x, y) to point (z, t) *)

let draw_line (x, y) (z, t) =
  set_line_width 1 ;
  set_color black ;
  moveto x y ;
  lineto z t
;;


(* 2.1.1 mountain function draw a mountain of n sides from point p1 to point p2 *)

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

mountain 8 (100, 200) (700, 200) ;;


(* 2.1.2 dragon function draw a dragon *)

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
