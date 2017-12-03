(*Advent of Code Day 1*)

let captcha s = 
 let rec sumup s n sum initial = 
  match s with
  |"" -> if n = initial then sum + initial else sum +initial
  |ch -> let x = String.get ch 0 in if (int_of_char x - int_of_char '0') = n then sumup (String.sub s 1 ((String.length s)-1)) 
   (int_of_char x - int_of_char '0') (sum + (int_of_char x - int_of_char '0')) initial else
    sumup (String.sub s 1 ((String.length s)-1)) 
    (int_of_char x - int_of_char '0') sum initial
 in sumup s 0 0 (int_of_char (String.get s 0) - int_of_char '0')


let new_captcha s =
 let rec sumup s1 s2 sum=
  match s1, s2 with
  |"", "" -> sum
  |ch1, ch2 -> let x = String.get ch1 0 in let y = String.get ch2 0 in 
   if (x=y) then sumup (String.sub ch1 1 ((String.length ch1)-1)) (String.sub ch2 1 ((String.length ch2)-1)) (sum + (int_of_char x - int_of_char '0')*2) 
   else sumup (String.sub ch1 1 ((String.length ch1)-1)) (String.sub ch2 1 ((String.length ch2)-1)) sum
  in let split = ((String.length s) / 2) in sumup (String.sub s 0 split) (String.sub s split split) 0


let infile = open_in "Day1Puzzle1Input.txt"
let s = input_line infile;;

(*SPLIT STRING IN HALF IN INITIAL CALL AND THEN LOOP THE FUCK THROUGH*)