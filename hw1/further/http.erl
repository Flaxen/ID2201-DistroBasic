-module(http).
-export([parse_request/1, ok/1, get/1, ok2/3, get_type/1, file_size/1]).

parse_request(R0) ->
  {Request, R1} = request_line(R0),
  {Headers, R2} = headers(R1),
  {Body, _} = message_body(R2),
  {Request, Headers, Body}.

request_line([$G, $E, $T, 32 | R0]) ->
  {URI, R1} = request_uri(R0),
  {VER, R2} = http_version(R1),
  [13,10|R3] = R2,
  {{get, URI, VER}, R3}.

request_uri([32|R0]) ->
    {[], R0};
request_uri([C|R0]) ->
    {Rest, R1} = request_uri(R0),
    {[C|Rest], R1}.

http_version([$H, $T, $T, $P, $/, $1, $., $1 | R0]) ->
  {v11, R0};
http_version([$H, $T, $T, $P, $/, $1, $., $0 | R0]) ->
  {v10, R0}.

headers([13,10|R0]) ->
  {[], R0};
headers(R0) ->
  {Header, R1} = header(R0),
  {Rest, R2} = headers(R1),
  {[Header|Rest], R2}.

header([13,10|R0]) ->
  {[], R0};
header([C|R0]) ->
  {Rest, R1} = header(R0),
  {[C|Rest], R1}.

message_body(R) ->
  {R, []}.

ok(Body) ->
  "HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ Body.

get(URI) ->
  "GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n".


get_type(URI) ->
  {ok, Type, Rest} = parse_type(URI),
  case Type of
    "gif" -> {ok, "image/gif", Rest};
    "png" -> {ok, "image/png", Rest};
    "jpg" -> {ok, "image/jpg", Rest};
    "html" -> {ok, "text/html", Rest};
    "php" -> {ok, "text/html", Rest}
  end.

parse_type([H|T]) ->
  parse_type([H], T).

parse_type(Rest, [$.|Type]) ->
  {ok, Type, Rest};
parse_type(Rest, [C|T]) ->
  parse_type(Rest ++ [C], T).

file_size(Filename) ->
  {ok, {_, Size, _,_,_,_,_,_,_,_,_,_,_,_}} = file:read_file_info(Filename),
  {ok, Size}.






ok2(Body, Size, Type) ->
  "HTTP/1.1 200 OK\r\n" ++ "Content-Length : " ++ [Size] ++"\r\nContent-Type : " ++ Type ++ "\r\n" ++ "\r\n" ++ Body.
%
% ok2(Body) ->
%   "HTTP/1.1 200 OK\r\n" ++ "Content-Length : 2225937\r\nContent-Type : text/gif\r\n" ++ "\r\n" ++ Body.























%
