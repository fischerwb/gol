# gol
Conway's Game Of Life
Erlang implementation

All commands below should be executed from the "gol" directory.

Building:
erlc -o ebin src/*.erlc

Running:
erl -pa ebin

Once the erlang shell open, enter "application:start(gol)." and press Enter.
Then, you are ready to enter any of the following commands.


Command Summary

gol:set_universe_size(Width, Height).
	Sets the universe size within which cells exist.
	The game defaults to a Width and Height of 100.
	
gol:add_shape(center | {X, Y}, 
			  glider | small_exploder | exploder | spaceship | tumbler | ten_cell_row | [{X, Y}]).
	Adds the cells for the specified pre-existing shape, or the shape defined by the list of {X, Y} coordinates to the universe.

gol:clear().
	Removes all live cells.
	
gol:next().
	Creates the next generation of live cells from the existing live cells.

gol:start().
	Continually creates the next generation of live cells every second.
	
gol:start(Frequency).
	Continually creates the next generation of live cells, pausing for the specified number of seconds.
	
gol:stop().
	Stops the automatic generation of live cells.
	
gol:save(Filename).
	Saves the current universe to the specified file name.
	
gol:load(Filename).
	Loads the a universe from the specified file name, clearing out the current universe.
	
