-module(onp).
-author("piotrek").

-export([onp/1]).


onp(Expr) -> onp_calc(string:tokens(Expr, " "), []).

onp_parse_num(Num) -> element(1, string:to_float(Num ++ ".0")).

onp_calc([], [H | _]) -> H;
onp_calc(["+" | T], [X, Y | S]) -> onp_calc(T, [Y + X | S]);
onp_calc(["-" | T], [X, Y | S]) -> onp_calc(T, [Y - X | S]);
onp_calc(["*" | T], [X, Y | S]) -> onp_calc(T, [Y * X | S]);
onp_calc(["/" | _], [0.0 | _]) -> erlang:error("Zero division error.");
onp_calc(["/" | T], [X, Y | S]) -> onp_calc(T, [Y / X | S]);
onp_calc(["pow" | T], [X, Y | S]) -> onp_calc(T, [math:pow(Y, X) | S]);
onp_calc(["sqrt" | _], [X | _]) when X < 0 -> erlang:error("Value error.");
onp_calc(["sqrt" | T], [X | S]) -> onp_calc(T, [math:sqrt(X) | S]);
onp_calc(["sin" | T], [X | S]) -> onp_calc(T, [math:sin(X) | S]);
onp_calc(["cos" | T], [X | S]) -> onp_calc(T, [math:cos(X) | S]);
onp_calc(["tan" | T], [X | S]) -> onp_calc(T, [math:tan(X) | S]);
onp_calc(["ctg" | T], [X | S]) -> onp_calc(T, [1 / math:tan(X) | S]);
onp_calc(["pi" | T], S) -> onp_calc(T, [math:pi() | S]);
onp_calc([Num | T], S) -> onp_calc(T, [onp_parse_num(Num) | S]).

%% Expressions:
%%    Raw
%%    ONP
%%    Value

%%    1 + 2 * 3 - 4 / 5 + 6
%%    1 2 3 * + 4 5 / - 6 +
%%    12.2

%%    1 + 2 + 3 + 4 + 5 + 6 * 7
%%    1 2 + 3 + 4 + 5 + 6 7 * +
%%    57

%%    ( (4 + 7) / 3 ) * (2 - 19)
%%    4 7 + 3 / 2 19 - *
%%    -62.333333333333

%%    17 * (31 + 4) / ( (26 - 15) * 2 - 22 ) - 1
%%    17 31 4 + * 26 15 - 2 * 22 - / 1 -
%%    Zero division error
