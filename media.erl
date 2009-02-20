-module(media).
-import(lists, [reverse/1]).
-import(filelib).
-compile(export_all).
-include_lib("kernel/include/file.hrl").

organise_files_in_directory (Dir) ->
    case file:list_dir(Dir) of 
        {ok, Files} -> organise_files_in_directory( Dir, Files);
        _           -> error
    end.

organise_files_in_directory (Dir, [File|Rest]) ->
    
    FullName = filename:join([Dir, File]),
   
    case file_type(FullName) of 
        regular ->
                case regexp:match(FullName, regexp:sh_to_awk("*.mp3")) of
                    {match, _, _} ->
                        moveTrack(FullName),    
                        organise_files_in_directory(Dir, Rest);                
                    _ ->
                        organise_files_in_directory(Dir, Rest)
                end;
        directory ->
                organise_files_in_directory(FullName),
                organise_files_in_directory(Dir,Rest)
    end.

file_type (File) ->
    case file:read_file_info(File) of
        {ok, Facts} ->
            case Facts#file_info.type of 
                regular   -> regular;
                directory -> directory;
                _         -> error
            end;
        _ ->
            error
    end.

moveTrack(OriginalFile) ->
    {DestFolder,DestFile} = determineFolderAndFileNames(read_id3_tag(OriginalFile)),   
    filelib:ensure_dir(DestFolder),
    file:copy(OriginalFile, lists:flatten([DestFolder,DestFile])),
    ok.

move({movies, File}) -> file:copy(File, ["movies", File]),
    file:delete(File);

move({pics, File}) ->
    file:copy(File, ["pics/",File]),
    file:delete(File).

determineFolderAndFileNames({Id3Type,
                            {position, Position},
                            {title, Title},
                            {artist, Artist},
                            {album, Album}}) ->
    FileName = lists:append([camelCase(Title),"-",integer_to_string(Position),".mp3"]),
    Dir = lists:append(["music", "/", camelCase(Artist),"/",camelCase(Album),"/"]),
    {Dir, FileName}.

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
        {album, "Death of the fair maiden"},
        {genre, "hard-rock"}
    }.

read_id3_tag(File) ->
    case file:open(File, [read,binary,raw]) of 
        {ok, S} ->
            Size = filelib:file_size(File),
            {ok, B2} = file:pread(S, Size-128, 128),
            Result = parse_v1_tag(B2),
            file:close(S),
            Result;
        Error ->
            {File, Error}
    end.

% parse an id3 tag of an mp3
parse_v1_tag(<<$T,$A,$G,
    Title:30/binary, Artist:30/binary, Album:30/binary, _Year:4/binary,
    _Comment:28/binary, 0:8, Pos:8, _Genre:8>>)->
        {"ID3v1.1",     
            {position, Pos}, 
            {title, extract_string_from_binary(Title)}, 
            {artist, extract_string_from_binary(Artist)}, 
            {album, extract_string_from_binary(Album)}};

% parse an id3 tag of an mp3
parse_v1_tag(<<$T,$A,$G,
    Title:30/binary, Artist:30/binary, Album:30/binary, _Year:4/binary,
    _Comment:30/binary,_Genre:8>>) ->
    {"ID3v1",
            [{title, extract_string_from_binary(Title)}, 
            {artist, extract_string_from_binary(Artist)}, 
            {album, extract_string_from_binary(Album)}]};

parse_v1_tag(_) ->
    error.

% A segment of binary that refers to an element of a protocol/format
% is likely to have not been fully utilised. Consequently the excess void
% needs to be removed in processing
extract_string_from_binary(Bin) ->
    reverse(skip_blanks_and_zero(reverse(binary_to_list(Bin)))).

skip_blanks_and_zero([$\s|T]) -> skip_blanks_and_zero(T);
skip_blanks_and_zero([0|T]) -> skip_blanks_and_zero(T);
skip_blanks_and_zero(L) -> L.    

integer_to_string(Integer) ->
    case Integer of
         
         0 -> "0";
         1 -> "1";
         2 -> "2";
         3 -> "3";
         4 -> "4";
         5 -> "5";
         6 -> $6;
         7 -> $7; 
         8 -> $8;
         9 -> $9;
         _ -> $a
    end.
                    

