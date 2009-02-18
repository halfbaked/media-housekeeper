-module(media).
-import(lists).
-export([cleanUp/1,move/1]).

organiseFilesInDirectory (Dir) ->
    file:list_dir(Dir).

move({music, File}) -> file:copy(File, ["music", File]),
    file:delete(File);

move({movies, File}) -> file:copy(File, ["movies", File]),
    file:delete(File);

move({pics, File}) ->
    file:copy(File, ["pics/",File]),
    file:delete(File).

findCatForFile(File, [firstCat|restOfCats])
    when lists:any(lists:suffix(FE), extractFileExtensions(firstCat)) ->
    firstCat;
findCatForFile(File, [firstCat|restOfCats]) ->
    findCatForFile(File, restOfCats).   

extractFileExtensions(Category) ->
    {_,{fileExtensions, FileExtensions}_} = Category,
    FileExtension.
    
findFileExtensions(music) -> ["mp3", "mp4", "ogg"];
findFileExtensions(movies) -> ["divx", "mpeg"];
findFileExtensions(pics) -> ["jpeg", "jpg", "gif", "bmp", "png"].

getCategoryFolder(music) -> ["movies"];
getCategoryFolder(movies) -> ["music"];
getCategoryFolder(pics) -> ["pics"];

getCategories() -> [music, movies, pics].

Categories = [
    {{name, "movies"}, {fileExtensions, [".divx", ".mov"]}, {folderDesignate, "movies"}},
    {{name, "music"}, {fileExtensions, [".mp3", "ogg"]}, {folderDesignate, "movies"}},
    {{name, "pics", {fileExtensions, [".jpg", ".jpeg"]}, {folderDesignate, "pics"}}
]

