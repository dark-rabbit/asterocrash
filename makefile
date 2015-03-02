
asteroids: asteroids.ml
	ocamlc -thread graphics.cma unix.cma asteroids.ml -o asteroids
