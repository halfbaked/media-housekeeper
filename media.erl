-module(media).
-import(lists).
-import(filelib).
-compile(export_all).

%Categories = [
%    {{name, "movies"}, {fileExtensions, [".divx", ".mov"]}, {folderDesignate, "movies"}},
%    {{name, "music"}, {fileExtensions, [".mp3", "ogg"]}, {folderDesignate, "movies"}},
%    {{name, "pics", {fileExtensions, [".jpg", ".jpeg"]}, {folderDesignate, "pics"}}
%].

organiseFilesInDirectory (Dir) ->
    {ok, Files} = file:list_dir(Dir),
    [move({music, lists:append([Dir,"/", F])}) || F <- Files].

%move({music, SourceDir, SourceFile}) -> file:copy(File, ["music", File]),
%    file:copy(File, ["music/",File]),
%    file:delete(File);

moveTrack(OriginalFile) ->
    {DestFolder,DestFile} = determineFolderAndFileNames(extractTrackDetails(OriginalFile)),
    filelib:ensure_dir(DestFolder).
%    file:copy(File, 


move({movies, File}) -> file:copy(File, ["movies", File]),
    file:delete(File);

move({pics, File}) ->
    file:copy(File, ["pics/",File]),
    file:delete(File).

determineFolderAndFileNames({track,
                            {type, Type},
                            {position, Position},
                            {trackName, Track},
                            {artist, Artist},
                            {album, Album},
                            _}) ->
    FileName = lists:append([camelCase(Track),"-",Position,".",Type]),
    Dir = lists:append(["music", "/", camelCase(Artist),"/",camelCase(Album)]),
    {FileName, Dir}.

camelCase(Word) ->
    [First|Rest] = string:tokens(Word, " "),
    lists:flatten([decapitalize(First)|lists:map(fun capitalize/1, Rest)]).

decapitalize([H|T]) ->
    [string:to_lower(H)|T].

capitalize([H|T]) ->
    [string:to_upper(H)|T].

    



% determine folder structure, for every element of folder structure check for directory
% if no directory, make one.
% then make file as last element in directory
% filelib:ensure_dir("/this/path/will/soon/exist").
       
extractTrackDetails(Track) ->
    % if no id3 values, read file name, 
    % and folder name to determine track details
    {track, 
        {type, "mp3"},
        {position, "1"},
        {trackName, "How To Behave"},
        {artist, "Iron Maiden"},
        {album, "death metal"},
        {genre, "hard-rock"}
    }.

%findCatForFile(File, [firstCat|restOfCats])
%    when lists:any(lists:suffix(FE), extractFileExtensions(firstCat)) ->
%    firstCat;
%findCatForFile(File, [firstCat|restOfCats]) ->
%    findCatForFile(File, restOfCats).   

%extractFileExtensions(Category) ->
%    {_,{fileExtensions, FileExtensions}_} = Category,
%    FileExtension.
    
%findFileExtensions(music) -> ["mp3", "mp4", "ogg"];
%findFileExtensions(movies) -> ["divx", "mpeg"];
%findFileExtensions(pics) -> ["jpeg", "jpg", "gif", "bmp", "png"].

%getCategoryFolder(music) -> ["movies"];
%getCategoryFolder(movies) -> ["music"];
%getCategoryFolder(pics) -> ["pics"];

%getCategories() -> [music, movies, pics].



