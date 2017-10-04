(* datatype declarations *)
type color = int*int*int;		(* RGB in 0..255 *)
type xy = int*int;				(* points (x, y) and sizes (w, h) *)
datatype image = Image of xy * color array array;

(* constructs a uniform image *)
fun image ((w, h) : xy) (col : color) : image = Image ((w,h), Array.tabulate (h, fn n => Array.array(w, col)));

(* maps an image to its size (w,h) *)
fun size (Image(wh, _)) = wh;

(* maps an image to the number of its pixels *)
fun intSize (Image((w,h), _)) = w*h;

(* draws a pixel to an image *)
fun drawPixel (Image(_, arr)) col ((x,y) : xy) = Array.update(Array.sub(arr, y), x, col);

(* maps a color to a string suitable for the .ppm format *)
fun colToStr ((r,g,b) : color) : string = 
	(StringCvt.padLeft #" " 4 (Int.toString r)) ^ 
	(StringCvt.padLeft #" " 4 (Int.toString g))	^
	(StringCvt.padLeft #" " 4 (Int.toString b));	
	
(* maps a row of color array to a row of .ppm output *)
fun rowToStr arr = 
	let 
		val index = ref 0
		val str = ref ""
	in
		while (!index < Array.length arr) do
			(str := !str ^ (colToStr (Array.sub(arr, !index)));
			index := !index + 1);
		!str
	end;
	
(* maps a color array to .ppm output *)
fun imToStr arr =
	let 
		val index = ref 0
		val str = ref ""
	in
		while (!index < Array.length arr) do
			(str := (!str ^ "\n") ^ (rowToStr (Array.sub(arr, !index)));
			index := !index + 1);
		!str
	end;
	
(* prints a .ppm file corresponding to a color array to the output channel oc *)
fun prnt oc arr = TextIO.output(oc, (imToStr arr));

(* creates a .ppm file corresponding to the image im *)
fun toPPM image filename =
	let val oc = TextIO.openOut filename
		val (w,h) = size image
		val arr = case image of Image(_, a) => a
	in
		TextIO.output(oc,"P3\n" ^ Int.toString w ^ " " ^ Int.toString h ^ "\n255");
		(* code to output image rows, one per line goes here *)
		prnt oc arr;
		TextIO.closeOut oc
	end;

fun gradient ((x,y):xy) : color =
	(((x div 30) * 30) mod 256, 0, ((y div 30) * 30) mod 256);

(* computes the mandelbrot value for a point (x,y) *)
fun mandelbrot (maxIter : int) ((x,y) : real * real) : real =
	let 
		fun solve (a,b) c = 
			if c = maxIter then 
				1.0
			else
				if (a*a + b*b <= 4.0) then 
					solve (a*a - b*b + x, 2.0*a*b + y) (c+1)
				else 
					(real c)/(real maxIter)
	in
		solve (x,y) 0
	end;

(* maps a real value to a color for the pixel *)
fun chooseColour (n : real) : color =
	let
		val r = round ((Math.cos n) * 255.0)
		val g = round ((Math.cos n) * 255.0)
		val b = round ((Math.sin n) * 255.0)
	in
		(r,g,b)
	end;

(* maps a pixel position to the next pixel position in the color array *)
fun succPix (Image((w,h), arr)) ((x,y):xy) : xy = 
	if (x < (w-1)) then 
		(x+1, y)
	else 
		if (y < (h-1)) then 
			(0, y+1)
		else 
			(0,0);

local 
	fun drawNext f im pos 0 = ()
	| drawNext f im pos counter =
		(drawPixel im (f pos) pos;
		drawNext f im (succPix im pos) (counter-1));
in
	(* draws to an image using a function f mapping each pixel to a color *)
	fun drawAll f im = drawNext f im ((0,0)) (intSize im)
end;

(* draws to an image using a function f mapping each pixel to a color *)
fun gradImage () : unit = 
	let
		val im = image (640,480) (0,255,255)
	in
		(drawAll gradient im;
		toPPM im "gradient.ppm");
		()
	end;

	
(* used to rescale a rectangular image *)
fun rescale ((w, h) : xy) ((cx, cy, s) : real * real * real) ((x, y) : xy) : real * real =
	let
		val p = (s * (((real x)/(real w)) - 0.5)) + cx
		val q = (s * (((real y)/(real h)) - 0.5)) + cy
	in 
		(p,q)
	end;
	
(* executes the program: writes a .ppm picture of a mandelbrot set with given parameters *)
fun compute ((w,h) : xy) ((cx,cy,s) : real * real * real) (maxiter : int) : unit =
	let
		val im = image (w,h) (0,255,255)
	in
		(drawAll (fn (x,y) => (chooseColour (mandelbrot maxiter (rescale (w, h) (cx, cy, s) (x,y))))) im;
		toPPM im "mandelbrot.ppm");
		()
end;