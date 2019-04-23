:- begin_tests(typeInf).
:- include(typeInf). 

/* Note: when writing tests keep in mind that 
	the use of of global variable and function definitions
	define facts for gvar() predicate. Either test
	directly infer() predicate or call
	deleteGVars() predicate to clean up gvar().
*/

% NOTE: use nondet as option to test if the test is nondeterministic

/* Test for bTypes */

% Real types
test(bType_int) :- 
	bType(int).

test(bType_float) :- 
	bType(float).

test(bType_bool) :- 
	bType(bool).

test(bType_char) :- 
	bType(char).

test(bType_string) :- 
	bType(string).

test(bType_unit) :- 
	bType(unit).

% Fake type
test(bType_fake, [fail]) :- 
	bType(fake).

% Array tests
test(bType_array_int, [nondet]) :- 
	bType([int, int]).

test(bType_array_mixed, [nondet]) :- 
	bType([float, string]).

test(bType_array_head, [nondet]) :- 
	bType([char]).

test(bType_array_empty, [fail]) :- 
	bType([]).

/* Test for fType */

% Real default functions
test(fType_iplus) :- 
	fType(iplus, [int, int, int]).

test(fType_fplus) :- 
	fType(fplus, [float, float, float]).

test(fType_fToInt) :- 
	fType(fToInt, [float, int]).

test(fType_iToFloat) :- 
	fType(iToFloat, [int, float]).

test(fType_print_string) :- 
	fType(print, [string, unit]).

test(fType_print_int) :- 
	fType(print, [int, unit]).

test(fType_print_unit) :- 
	fType(print, [unit, unit]).

test(fType_identity_int) :- 
	fType(identity, [int, int]).

test(fType_identity_char) :- 
	fType(identity, [char, char]).

test(fType_identity_int) :- 
	fType(identity, [T, T]).

% Fake functions
test(fType_fake_one, [fail]) :- 
	fType(fake, [T, T]).

test(fType_fake_two, [fail]) :- 
	fType(fake, [bool]).
	
test(fType_fake_three, [fail]) :- 
	fType(fake, []).

/* Tests for typeExp */

% Using integer plus
test(typeExp_iplus) :- 
	typeExp(iplus(int, int), int).

% this test should fail
test(typeExp_iplus_F, [fail]) :-
	typeExp(iplus(int, int), float).

test(typeExp_iplus_T, [nondet, true(T == int)]) :-
	typeExp(iplus(int, int), T).

% Using print
test(typeExp_print) :- 
	typeExp(print(string), unit).

test(typeExp_print_F, [fail]) :-
	typeExp(print(int), float).

test(typeExp_print_T, [true(T == unit)]) :-
	typeExp(print(int), T).

% Using identity
test(typeExp_identity) :- 
	typeExp(identity(string), string).

test(typeExp_identity_F, [fail]) :-
	typeExp(identity(int), unit).

test(typeExp_identity_T, [true(T == unit)]) :-
	typeExp(identity(unit), T).

/* Test for exprList */

% Real functions
test(typeExpList_iplus) :-
	typeExpList([iplus(int, int)], [int]).

test(typeExpList_iplus_multiple) :-
	typeExpList([iplus(int, int), iplus(int, int)], [int, int]).

test(typeExpList_plus_mixed) :-
	typeExpList([iplus(int, int), fplus(float, float)], [int, float]).

test(typeExpList_plus_mixed_f, [fail]) :-
	typeExpList([iplus(int, int), fplus(float, float)], [float, int]).

test(typeExpList_empty) :-
	typeExpList([], []).

% Fake functions
test(typeExpList_fake, [fail]) :-
	deleteDb(),
	typeExpList([fake(int, int)], [int]).

test(typeExpList_fake_missing_out, [fail]) :-
	deleteDb(),
	typeExpList([fake(int, int)], []).

test(typeExpList_fake_var, [fail]) :-
	deleteDb(),
	typeExpList([x], [int]).

/* Test for statements */

% Test calling an exprestion 
test(typeStatement_expr_plain, [nondet]) :-
	typeStatement(int, T),
	assertion(T == int).

test(typeStatement_expr_iplus, [nondet]) :-
	typeStatement(iplus(X, Y), Z),
	assertion(X == int), assertion(Y == int), assertion(Z == int).

test(typeStatement_expr_print, [nondet]) :-
	typeStatement(print(_X), Y),
	assertion(Y == unit).

% Test gvLet
test(typeStatement_gvLet_lessThanF, [nondet]) :-
	deleteDb(),
	typeStatement(gvLet(v, T, lessThanF(X, Y)), unit),
	assertion(T == bool), assertion(X == float), assertion(Y == float),
	gvar(v, bool).

test(typeStatement_gvLet_iplus, [nondet]) :-
	deleteDb(),
	typeStatement(gvLet(v, T, iplus(X, Y)), unit),
	assertion(T == int), assertion(X == int), assertion(Y == int),
	gvar(v, int).

% Test gvFun
test(typeStatement_gvFun_typed, [nondet]) :-
	deleteDb(),
	typeStatement(gvFun(add, [int, int], int, [iplus(int, int)]), int),
	gvar(add, [int, int, int]).

test(typeStatement_gvFun_check, [nondet]) :-
	deleteDb(),
	typeStatement(gvFun(subtract, [A, B], C, [iplus(int, int)]), int),
	assertion(C == int),
	gvar(subtract, [A, B, C]).

test(typeStatement_gvFun_noop, [nondet]) :-
	deleteDb(),
	typeStatement(gvFun(noop, [], C, []), unit),
	assertion(C == unit),
	gvar(noop, [C]).

% Test lvLet
test(typeStatement_lvLet_lessThanF, [nondet]) :-
	deleteDb(),
	typeStatement(lvLet(g, T, lessThanF(X, Y), []), unit),
	assertion(T == bool), assertion(X == float), assertion(Y == float).

test(typeStatement_lvLet_iplus, [nondet]) :-
	deleteDb(),
	typeStatement(lvLet(k, T, iplus(X, Y), [k]), unit),
	assertion(T == int), assertion(X == int), assertion(Y == int).

test(typeStatement_lvLet_removed, [fail]) :-
	deleteDb(),
	typeStatement(lvLet(a, T, iplus(X, Y), [k]), unit),
	assertion(T == int), assertion(X == int), assertion(Y == int),
	lvar(a, T).

test(typeStatement_lvLet_global, [fail]) :-
	deleteDb(),
	typeStatement(lvLet(q, T, iplus(X, Y), print(q)), unit),
	assertion(T == int), assertion(X == int), assertion(Y == int),
	gvar(q, T).

% Independently do a test to see if locals exist after running test

% Test if
test(typeStatement_if_types, [nondet]) :-
	typeStatement(if(B, [], []), T),
	assertion(B == bool), assertion(T == unit).

test(typeStatement_if_one, [nondet]) :-
	typeStatement(if(B, iplus(X, Y), []), T),
	assertion(B == bool), assertion(X == int), assertion(Y == int), assertion(T == int).

test(typeStatement_if_two, [nondet]) :-
	typeStatement(if(B, iplus(X, X), fToInt(Y)), T),
	assertion(B == bool), assertion(X == int), assertion(Y == float), assertion(T == int).

test(typeStatement_if_two_f, [fail]) :-
	typeStatement(if(B, iplus(X, X), fplus(Y, Y)), T),
	assertion(B == bool), assertion(X == int), assertion(Y == float), assertion(T == int).

% Test for

test(typeStatement_for_types, [nondet]) :-
	typeStatement(for(_, B, _, []), T),
	assertion(B == bool), assertion(T == unit).

test(typeStatement_for_head, [nondet]) :-
	typeStatement(for(int, greaterThanEqualI(Y, Y), int, []), T),
	assertion(Y == int), assertion(T == unit).

test(typeStatement_for_full, [nondet]) :-
	deleteDb(),
	typeStatement(for(fplus(X, X), greaterThanEqualI(Y, Y), fplus(Z, Z), gvLet(x, int, int)), T),
	assertion(X == float), assertion(Y == int), assertion(Z == float), assertion(T == unit).

% Test while

test(typeStatement_while_types, [nondet]) :-
	typeStatement(while(B, []), T),
	assertion(B == bool), assertion(T == unit).

test(typeStatement_while_head, [nondet]) :-
	typeStatement(while(greaterThanEqualI(Y, Y), []), T),
	assertion(Y == int), assertion(T == unit).

test(typeStatement_while_full, [nondet]) :-
	typeStatement(while(greaterThanEqualI(Y, Y), gvLet(x, int, int)), T),
	assertion(Y == int), assertion(T == unit).

/* Test for code blocks */

test(typeCode_gvLet, [nondet]) :-
	deleteDb(),
	typeCode([gvLet(v, int, identity(int))], unit).

test(typeCode_gvLet_mulit, [nondet]) :-
	deleteDb(),
	typeCode([gvLet(v, Y, identity(Y)), gvLet(v, _, fplus(float, float))], unit).

test(typeCode_empty, [nondet]) :-
	typeCode([], unit).

test(typeCode_gvLet_bad, [fail]) :-
	deleteDb(),
	typeCode([gvLet(v, int, identity(iny)), gvLet(v, float, fplus(float, float))], int).


/* Integration testing */
test(infer_multipleScopes, [nondet]) :- 
	infer([lvLet(a, int, int, [lvLet(b, int, int, [lvLet(c, int, int, [a,b,c])])])], unit).

test(infer_overridingScopes, [nondet]) :- 
	infer([gvLet(a,bool,bool), lvLet(a, int, int, [lvLet(b, int, int, [lvLet(a, float, float, [a,b])])])], unit),
	assertion(gvar(a,bool)), assertion(lvar([])).

test(infer_if, [nondet]) :- 
	infer([gvLet(a, float, float), if(lessThanF(float, float), [fplus(a, float)], [fminus(a, float)])], float).

test(infer_functionLoop, [nondet]) :- 
	infer([gvFun(add, [int, int], int, [return(iplus(int, int))]), gvLet(a, T, int), while(lessThanI(a, int), [add(a, int), unit])], unit),
	assertion(T == int).

test(infer_nestedFor, [nondet]) :- 
	infer([for(gvLet(i, T, int), greaterThanEqualI(i, int), iminus(i, int), [for(gvLet(j, U, int), greaterThanEqualI(j, int), iminus(j, int), [for(gvLet(k, V, int), greaterThanEqualI(k, int), iminus(k, int), [print(i), print(j), print(k)])])])], unit),
	assertion(T == int) , assertion(U == int), assertion(V == int).

/* Example tests */

% test for statement with state cleaning
test(typeStatement_gvar, [nondet, true(T == int)]) :- % should succeed with T=int
	deleteDb(), /* clean up variables */
	typeStatement(gvLet(v, T, iplus(X, Y)), unit),
	assertion(X == int), assertion(Y == int), % make sure the types are int
	gvar(v, int). % make sure the global variable is defined

% same test as above but with infer 
test(infer_gvar, [nondet]) :-
	infer([gvLet(v, T, iplus(X, Y))], unit),
	assertion(T == int), assertion(X == int), assertion(Y == int),
	gvar(v, int).

% test custom function with mocked definition
test(mockedFct, [nondet]) :-
	deleteDb(), % clean up variables since we cannot use infer
	asserta(gvar(my_fct, [int, float])), % add my_fct(int)-> float to the gloval variables
	typeExp(my_fct(X), T), % infer type of expression using or function
	assertion(X == int), assertion(T == float). % make sure the types infered are correct

:-end_tests(typeInf).
