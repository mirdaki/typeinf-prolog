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

test(typeExp_iplus_T, [true(T == int)]) :-
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
	typeExpList([fake(int, int)], [int]).

test(typeExpList_fake_missing_out, [fail]) :-
	typeExpList([fake(int, int)], []).

test(typeExpList_fake_var, [fail]) :-
	typeExpList([x], [int]).

/* TODO: Test for statements */

/* Test for code blocks */

test(typeCode_gvLet, [nondet]) :-
	typeCode([gvLet(v, int, identity(int))], unit).

test(typeCode_gvLet_mulit, [nondet]) :-
	typeCode([gvLet(v, Y, identity(Y)), gvLet(v, _, fplus(float, float))], unit).

test(typeCode_empty, [fail]) :-
	typeCode([], unit).

test(typeCode_gvLet_bad, [fail]) :-
	typeCode([gvLet(v, int, identity(iny)), gvLet(v, float, fplus(float, float))], int).

/* TODO: Test for user defined variables? */

/* TODO: Test for user defined functions */

/* Integration testing */


% test for statement with state cleaning
test(typeStatement_gvar, [nondet, true(T == int)]) :- % should succeed with T=int
	deleteGVars(), /* clean up variables */
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
	deleteGVars(), % clean up variables since we cannot use infer
	asserta(gvar(my_fct, [int, float])), % add my_fct(int)-> float to the gloval variables
	typeExp(my_fct(X), T), % infer type of expression using or function
	assertion(X == int), assertion(T == float). % make sure the types infered are correct

:-end_tests(typeInf).
