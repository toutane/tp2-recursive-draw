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


(* 2.1.1 moutain function draw a moutain of n sides from point p1 to point p2 *)

let moutain n p1 p2 =
  clear_graph() ;
  let rec draw c p1 p2 =
    if c = 1 then draw_line p1 p2
    else (
      let (x, y) = p1 and (z, t) = p2
      in let h = (y + t) / 2 + (Random.int (10 * c ))
         in let m = ((x + z) / 2, h)
            in (
                draw (c - 1) p1  m ;
                draw (c - 1) m p2
              )
    )
  in draw n p1 p2
;;

moutain 8 (100, 200) (700, 200) ;;
