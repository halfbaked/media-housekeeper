-module(media).
-import(lists).
-export([cleanUp/1,move/1]).

cleanUp (Dir) ->
	file:list_dir(Dir).

move({mp3, File}) -> file:copy(File, ["music", File]),
	file:delete(File);

move({divx, File}) -> file:copy(File, ["movies", File]),
	file:delete(File);

move({jpg, File}) -> 
	file:copy(File, ["pics/",File]),
	file:delete(File).

