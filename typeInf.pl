/* match functions by unifying with arguments 
	and inferring the result
*/
typeExp(Fct, T):-
	\+ var(Fct), /* make sure Fct is not a variable */ 
	\+ atom(Fct), /* or an atom */
	functor(Fct, Fname, _Nargs), /* ensure we have a functor */
	!, /* if we make it here we do not try anything else */
	Fct =.. [Fname|Args], /* get list of arguments */
	append(Args, [T], FType), /* make it look like a function signature */
	functionType(Fname, TArgs), /* get type of arguments from definition */
	typeExpList(FType, TArgs). /* recursively match types */

/* propagate types */
typeExp(T, T).

/* list version to allow function machine */
typeExpList([], []).
typeExpList([Hin|Tin], [Hout|Tout]):-
	typeExp(Hin, Hout), /* type infer the head */
	typeExpList(Tin, Tout). /* recursive */

/* TODO: add statements types and their type checking (such as if, loops, etc) */

/* Statements */

/* 
	Expression
	Example: 
		expr(int) ~ 1 + 1;
		expr(unit) ~ print "hello world";
*/
typeStatement(expr(Code), T) :-
	typeExp(Code, T),
	bType(T).

/*
	Return
	Example:
		return(int) ~ return 2;
		return(float) ~ return 3.1 + 2.0;
*/
typeStatement(return(Code), T) :-
	typeExp(Code, T),
	bType(T).

/* 
	Global variable definition
	Example:
		gvLet(v, T, int) ~ let v = 3;
		gvLet(a, float, float) ~ let a: float = 2.5;
*/
typeStatement(gvLet(Name, T, Code), unit):-
	atom(Name), /* make sure we have a bound name */
	typeExp(Code, T), /* infer the type of Code and ensure it is T */
	bType(T), /* make sure we have an inferred type */
	asserta(gvar(Name, T)). /* add definition to database */

/* TODO: Function defintions 
	Example:
		let add x y = x+y
*/
typeStatement(gvFun(Name, Args, Code), T):-
	atom(Name), /* make sure we have a bound name */
	typeCode(Code, T), /* infer the type of Code and ensure it is T */
	bType(T), /* make sure we have an inferred type */
	is_list(Args),
	asserta(gvar(Name, T)). /* add definition to database */

/*
	If
	Examples: 
		if(bool, gvLet(x, T, T)) ~ if x == y then gvLet x = 1;
		if(bool, gvLet(x, T, T), gvLet(x, T, T)) ~ if x == y then gvLet x = 1; else gvLet x = 2;
*/
typeStatement(if(Cond, TCode), T) :-
	typeStatement(if(Cond, TCode), T) :-
	typeExp(Cond, B),
	bType(B),
	B = bool,
	bType(T), 
	typeCode(TCode, T).
typeStatement(if(Cond, TCode, FCode), T) :-
	typeExp(Cond, B),
	bType(B),
	B = bool,
	bType(T), 
	typeCode(TCode, T),
	typeCode(FCode, T).

/* TODO: For `let ... in ...` you can use this map to recreate scopes (http://www.swi-prolog.org/pldoc/man?section=pairs) */

/* 
	For loop 
	Example:
		for(int, bool, int, unit) ~ for (i = 0; i < 10; i++) { let v = v + 1 }
*/
typeStatement(for(Init, Cond, Tail, Code), unit) :-
	typeExp(Init, I),
	bType(I),
	typeExp(Cond, B),
	bType(B),
	B = bool,
	typeExp(Tail, A),
	bType(A),
	typeCode(Code, T),
	bType(T), 
	T = unit.

/* 
	While loop 
	Example:
		while(bool, unit) ~ while (i < 10) { let i = i + 1 }
*/
typeStatement(while(Cond, Code), unit) :-
	typeExp(Cond, B),
	bType(B),
	B = bool,
	typeCode(Code, T),
	bType(T), 
	T = unit.

/* TODO: Code block? */

/* Code is simply a list of statements. The type is 
	the type of the last statement 
*/
typeCode(S, T):- typeStatement(S, T).
typeCode([S], T):- typeStatement(S, T).
typeCode([S, S2|Code], T):-
	typeStatement(S, _T),
	typeCode([S2|Code], T).

/* top level function, this is clean up code */
infer(Code, T) :-
	is_list(Code), /* make sure Code is a list */
	deleteGVars(), /* delete all global definitions */
	typeCode(Code, T).

/* Basic types */
bType(int).
bType(float).
bType(bool).
bType(char).
bType(string).
bType(unit). /* unit type for things that are not expressions */

/*  functions type.
	The type is a list, the last element is the return type
	E.g. add: int->int->int is represented as [int, int, int]
	and can be called as add(1,2)->3
*/
bType([H]):- bType(H).
bType([H|T]):- bType(H), bType(T).

/*
	TODO: as you encounter global variable definitions
	or global functions add their definitions to 
	the database using:
		asserta( gvar(Name, Type) )
	To check the types as you encounter them in the code
	use:
		gvar(Name, Type) with the Name bound to the name.
	Type will be bound to the global type
	Examples:
		g

	Call the predicate deleteGVars() to delete all global 
	variables. Best way to do this is in your top predicate
*/

deleteGVars():-retractall(gvar), asserta(gvar(_X, _Y):- false()).

/*  builtin functions
	Each definition specifies the name and the 
	type as a function type

	TODO: add more functions
*/

fType(lessThanF, [float, float, bool]).
fType(lessThanI, [int, int, bool]).
fType(lessThanEqualF, [float, float, bool]).
fType(lessThanEqualI, [int, int, bool]).
fType(greaterThanF, [float, float, bool]).
fType(greaterThanI, [int, int, bool]).
fType(greaterThanEqualF, [float, float, bool]).
fType(greaterThanEqualI, [int, int, bool]).
fType(equalF, [float, float, bool]).
fType(equalI, [int, int, bool]).

fType(iplus, [int, int, int]).
fType(fplus, [float, float, float]).
fType(fToInt, [float, int]).
fType(iToFloat, [int, float]).
fType(print, [_X, unit]). /* simple print */
fType(identity, [T, T]). /* Get the type of the input and output it */

/* Find function signature
	A function is either build in using fType or
	added as a user definition with gvar(fct, List)
*/

% Check the user defined functions first
functionType(Name, Args):-
	gvar(Name, Args),
	is_list(Args). % make sure we have a function not a simple variable

% Check first built in functions
functionType(Name, Args) :-
	fType(Name, Args), !. % make deterministic

% This gets wiped out but we have it here to make the linter happy
% gvar(_, _) :- false().
:- dynamic(gvar/2).
