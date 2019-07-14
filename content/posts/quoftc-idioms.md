+++
title = "Rare C idioms in the Quoft compiler"
date = "2019-07-12T23:38:20-04:00"
tags = ["c", "idioms", "compilers"]
+++

About two years ago, I wrote a compiler for a C-like programming language called [Quoft](https://github.com/io12/quoftc). I wrote this compiler in C. In hindsight, this was a bad idea. C has subjected programmers to a thousand hells of buffer overflows, heap corruptions, and stale-pointer bugs. Rust provides high-level abstractions, guarantees of safety, and performance comparable with C. However, I decided to use C, because it was the language I knew best at the time, and because C compilers are widely-available as a build dependency. Its lack of modern features prompted creative ways to mimic the functionality of newer languages.

## Pseudo-infallible allocation

By default, memory allocation in C is *fallible*. This means it can fail and alert the caller of failure. In most modern languages, memory allocation is *pseudo-infallible*, meaning programmers can generally assume the result of allocation is always valid. When the allocator fails, these languages crash with an *"out of memory"* message. This resolves C's cumbersome `NULL` checks after every `malloc()` call. Luckily, pseudo-infallible allocation can be implemented in C too!

```c
static void *ptr_sanitize(void *p)
{
	if (p == NULL) {
		fprintf(stderr, "%s: error: %s\n", argv0, strerror(errno));
		exit(EXIT_FAILURE);
	}
	return p;
}

void *xmalloc(size_t size)
{
	return ptr_sanitize(malloc(size));
}

void *xcalloc(size_t size)
{
	return ptr_sanitize(calloc(1, size));
}

void *xrealloc(void *p, size_t size)
{
	return ptr_sanitize(realloc(p, size));
}

char *xstrdup(const char *s)
{
	return strcpy(xmalloc(strlen(s) + 1), s);
}
```

Note that `ptr_sanitize()` is used to abstract away the boilerplate of `NULL` checks in the *pseudo-infallible* allocators.

You may wonder why `xstrdup()` isn't defined like this:

```c
char *xstrdup(const char *s)
{
	return ptr_sanitize(strdup(s));
}
```

This looks cleaner, but is less portable. `strdup(s)` is not a part of any C standard. It's only listed in Unix interface standards, such as *System V release 4*, *4.3BSD*, and *POSIX*.

## Global `argv0`

Consider the common signatures of `main()`:

```c
int main(void)
```

```c
int main(int argc, const char *argv[])
```

Creators of modern languages noticed that `argc` and `argv` do not need to change throughout execution. This means that they can be made global constants, without introducing any mutable state.
([Rust `std::env::args`](https://doc.rust-lang.org/std/env/fn.args.html),
[Haskell `getArgs`](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-Environment.html#v:getArgs),
[Go `os.Args`](https://golang.org/pkg/os/#pkg-variables))

In GNU C, one can use `program_invocation_name` to get `argv0` outside `main()`. However, this limits portability to compilers that support GNU C.

In `quoftc`, I stored `argv[0]` in a variable `argv0`.

```c
const char *argv0;

// ...

int main(int argc, const char *argv[])
{
	// ...

	argv0 = argv[0];

	// ...
}
```

The header `quoftc.h` allows other files to access `argv0`.

```c
extern const char *argv0;
```

*Why can `argv0` be assigned if it's declared as `const`?*

The placement of `const` in `const char *argv0` means that the items through the pointer are `const`. The pointer itself can still be changed. For example, `argv[0] = 'a'` would error. To make the pointer itself `const`, this would work:

```c
char *const argv0;
```

And this would make every aspect of `argv0` constant:

```c
const char *const argv0;
```

This syntax is uncommon, but valid by the [spiral rule](http://c-faq.com/decl/spiral.anderson.html).

The reason it's useful to access `argv[0]` outside main is for error printing. This allows errors of the form `<argv0>: <error message>`.

This can be seen with `ptr_sanitize()`.

```c
static void *ptr_sanitize(void *p)
{
	if (p == NULL) {
		fprintf(stderr, "%s: error: %s\n", argv0, strerror(errno));
		exit(EXIT_FAILURE);
	}
	return p;
}
```

## Optional GNU C macros

GNU C has an
[`__attribute__`](https://gcc.gnu.org/onlinedocs/gcc-3.2/gcc/Function-Attributes.html)
syntax for tagging functions with certain properties.

One could declare a function with `__attribute__((noreturn))` to tell the compiler that it never returns. For example, `exit()` satisfies this property. This allows the compiler to elide code generation after calls to `noreturn` functions.

Since this functionality is not portable outside GNU C, it is useful to conditionally define a macro for it.

```c
#ifdef __GNUC__
	#define NORETURN __attribute__((noreturn))
#else
	#define NORETURN
#endif
```

This provides the benefits of GNU C without breaking builds on other compilers.

## Utility macros

### `IN_RANGE()`

```c
#define IN_RANGE(x, min, max) ((x) >= min && (x) <= max)
```

The `IN_RANGE()` macro tests whether *min* <= *x* <= *max*. While this may seem trivial, the intent is clearer than `x >= min && x <= max`.

### `NEW()`

There's a common pattern of allocating objects with `allocfunc(sizeof(type))`. This boilerplate can be reduced with macros.

```c
#define NEW(type) ((type *) xmalloc(sizeof(type)))
#define NEWC(type) ((type *) xcalloc(sizeof(type)))
```

The cast allows the compiler to warn of pointer type mismatches.

To illustrate this, the code below has no compilation warnings, but a major bug. Can you spot it?

```c
struct foo *foo = xmalloc(sizeof(struct foo *)); // bug here
```

The type inside the `sizeof()` shouldn't be a pointer. The allocation should be the size of the object, not the size of a pointer to the object.

The compiler doesn't catch this bug because allocator functions like `xmalloc()` return a `void *`. Void pointers implicitly cast to any other pointer type.

The cast fixes this.

```c
struct foo *foo = NEW(struct foo *); // compile error
```

```c
struct foo *foo = NEW(struct foo); // perfecto
```

Fun fact: this macro is used in [Plan 9](https://en.wikipedia.org/wiki/Plan_9_from_Bell_Labs)'s [`rc`](https://en.wikipedia.org/wiki/Rc) shell and preprocessor.

### `ARRAY_LEN()`

```c
#define ARRAY_LEN(arr) (sizeof(arr) / sizeof((arr)[0]))
```

This gets the length of a global or stack array. It's cleaner than declaring as `int arr[NUM_STUFF]` and iterating with `NUM_STUFF`.

```c
#define NUM_AMOUNTS 10

int amounts[NUM_AMOUNTS];

// This is potentially unclear for large codebases
for (int i = 0; i < NUM_AMOUNTS; i++) {
	// ...
}
```

```c
#define NUM_AMOUNTS 10

int amounts[NUM_AMOUNTS];

// Clear specification of intent
for (int i = 0; i < ARRAY_LEN(amounts); i++) {
	// ...
}
```

## Opaque types

In OOP languages, it's common to implement methods as interfaces to an object. This encapsulates the state of the object and abstracts over its definition. While C is not an OOP language, it's possible to achieve the same effect with opaque types.

`foo.c`:
```c
#include "foo.h"

struct foo {
	int x, y, z;
};

struct foo *foo_new(int x, int y, int z)
{
	struct foo *foo = NEW(struct foo);
	foo.x = x;
	foo.y = y;
	foo.z = z;
	return foo;
}

int foo_get_val(struct foo *foo)
{
	return foo.x * foo.y * foo.z;
}
```

`foo.h`:
```c
struct foo; // Opaque type

struct foo *foo_new(int, int, int);
int foo_get_val(struct foo *);
```

Includers of `foo.h` can't access the fields of `struct foo`. They will only be able to interact with it through the public "method" `foo_get_val()`.

In a class-based language, this would be the equivalent of all fields being declared private. Here is an example of how this might look in Java.

```java
class Foo {
	private int x, y, z;

	public Foo(int x, int y, int z) {
		this.x = x;
		this.y = y;
		this.z = z;
	}

	public int getVal() {
		return x * y * z;
	}
}
```

This trick is used in `quoftc` to implement a [data structures library](https://github.com/io12/quoftc/blob/master/ds.h).

## Quasi-namespaces

C doesn't have the luxury of namespaces. However, this can be emulated in C's global namespace anarchy.

Say you have a file `vec.c` implementing a vector library. What would you name the methods for the operations, like *len*, *get*, *push*, *pop*, and *top*? Short names like `len` are likely to cause collisions in the global namespace.

The solution is to prefix each exported (non-`static`) function with something resembling the filename. Here, the prefix can be `vec_`. This makes the function names `vec_len()`, `vec_get()`, `vec_push()`, `vec_pop()`, and `vec_top()`.

It is also useful to quasi-namespace `enum`s. An example of this is the `flags` parameter of POSIX [`open()`](https://pubs.opengroup.org/onlinepubs/009695399/functions/open.html), which can be `O_RDONLY`, `O_WRONLY`, etc.

## Fake sum types

Conventionally, the output of parsers is an [abstract syntax tree (AST)](https://en.wikipedia.org/wiki/Abstract_syntax_tree). An AST is a tree data structure representing the syntactic structure of the code. Syntactic elements of a language can have several *variants*. For example, an expression can be a function call, an operator call, an identifier, etc.

In languages with sum types (like Rust) implementing these variants is easy.

```rust
enum OperatorKind {
	Add,
	Sub,
	Mul,
	Div,
}

// The use of Box<> here is similar to pointers in C.
// Indirection is required for the type to have a finite size.
enum Expr {
	FuncCall(Box<Expr>, Vec<Expr>),
	Operator(OperatorKind, Box<Expr>, Box<Expr>),
	Identifier(String),
}
```

Note that `enum` in Rust is a generalization of `enum` in C. It supports sum types. In this code, `Expr` can be either `FuncCall(func, args)`, `Operator(kind, left, right)`, or `Identifier(name)`. This is ideal for implementing ASTs.

C lacks sum types, but they can be emulated with tagged unions.

C has `union`s, which have the same syntax as `struct`s, but the compiler considers them the size of only the largest member (rather than the size of the sum of the members). Accessing a field of a `union` interprets the byte pattern of its data as the field's type. For example, the code below will get an integer with the same pattern of bytes as the float `3.14`.

```c
union {
	int i;
	float f;
} u;

u.f = 3.14;
printf("%d\n", u.i)
```

With `union`, sum types can be faked in C. Below is a translation of the Rust example.

```c
enum operator_kind {
	OP_ADD,
	OP_SUB,
	OP_MUL,
	OP_DIV,
}

struct expr {
	enum {
		EXPR_FUNC_CALL,
		EXPR_OPERATOR,
		EXPR_IDENTIFIER,
	} kind;

	union {
		struct {
			struct expr *func;
			struct expr **args;
		} func_call;

		struct {
			enum operator_kind kind;
			struct expr *left;
			struct expr *right;
		} operator;

		struct {
			char *name;
		} identifier;
	} u;
}
```

This is actually close to how compilers implement sum types internally, although tagged unions are easy to misuse. If code accesses a field that doesn't match `kind`, this can cause memory unsafety.

## Arbitrary `struct` allocators

Tagged unions are useful for defining AST nodes, but they don't solve the problem of how to create constructors for the nodes that allocate on the heap. One could use `xmalloc()` and set the fields each time a new node is required, but this is error-prone and requires repeated code.

The repeated construction code can be abstracted into a constructor (something like `alloc_expr()`) but this approach requires boilerplate *within* constructors.

My solution was to create generic `struct` and `union` constructor macros that can initialize arbitrary objects. I then created wrappers around these macros for each variant.

This depends on a somewhat arcane ISO C99 feature called compound literals, which are casts combined with `struct` initializers.

```c
struct point {
	int x, y;
};

(struct point){ 2, 3 }
```

Here, `(struct point){ 2, 3 }` acts as a constructor for `struct point`. This can be used to create a stack constructor for arbitrary `struct`s.

```c
#define STACK_STRUCT(struct_tag, ...) \
	((struct struct_tag) { \
		__VA_ARGS__ \
	})
```

This can be expanded into a heap constructor for arbitrary `struct`s.

```c
#define ALLOC_STRUCT(struct_tag, ...) \
	((struct struct_tag *) \
		memcpy(NEW(struct struct_tag), \
			&STACK_STRUCT(struct_tag, __VA_ARGS__), \
			sizeof(struct struct_tag)))
```

This creates the `struct` on the stack and `memcpy()`s it to the heap.

Now, we can create a constructor for `struct point` with minimal boilerplate.

```c
#define ALLOC_POINT(...) ALLOC_STRUCT(point, __VA_ARGS__)
```

## Hash table initialization

In `quoftc`'s lexer, there is a function `lookup_keyword()` mapping keyword strings to `enum` variants. Switch statements can't be used for this, because switch cases can't be string literals in C. An alternative is to use deeply nested switch statements on each character, but this is messy. Another alternative is to make an if-else chain calling `strcmp()`, but this has bad performance. It's best to use a hash table to map strings to objects.

However, there are multiple difficulties with the hash table approach. First, hash tables are not in the standard library. Second, hash tables usually need to be initialized at runtime.

The first problem can be solved by implementing your own [hash table library](https://github.com/io12/quoftc/blob/master/hash_table.c). For the second, the function mapping strings to other objects can have a `static` hash table inside. When called, it can check whether it's initialized, and create it if not.

```c
static enum tok_kind lookup_keyword(const char *keyword)
{
	static HashTable *keywords = NULL;

	if (keywords == NULL) {
		keywords = alloc_hash_table();
		hash_table_set(keywords, "let", (void *) LET);
		hash_table_set(keywords, "var", (void *) VAR);
		hash_table_set(keywords, "if", (void *) IF);
		hash_table_set(keywords, "then", (void *) THEN);
		hash_table_set(keywords, "else", (void *) ELSE);
		// ...
	}
	// Returns INVALID_TOK if not found
	return (enum tok_kind) hash_table_get(keywords, keyword);
}
```

### Optimizing the check

This works well, but checking if the hash table is initialized each call is wasteful. The hash table could be made global and its initialization could be in `init_lex()`. However, this pollutes the global namespace and introduces more global state.

Fortunately, GNU C provides [`__builtin_expect()`](https://gcc.gnu.org/onlinedocs/gcc/Other-Builtins.html). Semantically, `__builtin_expect(x, 1)` is the same as `x`. In other words, `__builtin_expect()` returns its first argument. This may seem useless. However, the compiler treats it as a hint that the value of the first argument is *almost always* the second argument. Compilers use these hints to optimize for the case when the first argument equals the second.

Because `__builtin_expect()` is only available in GNU C, it can break the build on certain compilers. The [optional macros](#optional-gnu-c-macros) idiom resolves this.

```c
#ifdef __GNUC__
	#define UNLIKELY(x) __builtin_expect((x), false)
#else
	#define UNLIKELY(x) (x)
#endif
```

We can apply this to the check in `lookup_keyword()`, since the hash table is *almost always* initialized.

```c
static enum tok_kind lookup_keyword(const char *keyword)
{
	static HashTable *keywords = NULL;

	if (UNLIKELY(keywords == NULL)) {
		keywords = alloc_hash_table();
		hash_table_set(keywords, "let", (void *) LET);
		hash_table_set(keywords, "var", (void *) VAR);
		hash_table_set(keywords, "if", (void *) IF);
		hash_table_set(keywords, "then", (void *) THEN);
		hash_table_set(keywords, "else", (void *) ELSE);
		// ...
	}
	// Returns INVALID_TOK if not found
	return (enum tok_kind) hash_table_get(keywords, keyword);
}
```

### Temporary macros

Another area for improvement is eliminating code repetition for the `hash_table_set()` calls. Each call contains `hash_table_set`, `keywords`, and `(void *)`. If the signature of `hash_table_set()` changed, every single call would need to be modified. We can abstract the boilerplate behind a temporary macro.

```c
static enum tok_kind lookup_keyword(const char *keyword)
{
	static HashTable *keywords = NULL;

	if (UNLIKELY(keywords == NULL)) {
		keywords = alloc_hash_table();
#define K(keyword, tok) hash_table_set(keywords, keyword, (void *) tok)
		K("let", LET);
		K("var", VAR);
		K("if", IF);
		K("then", THEN);
		K("else", ELSE);
		// ...
#undef K
	}
	// Returns INVALID_TOK if not found
	return (enum tok_kind) hash_table_get(keywords, keyword);
}
```

This looks much better! If the list of keywords is long, this can be refactored even more by placing the `K()` calls into an `#include`d file.

```c
static enum tok_kind lookup_keyword(const char *keyword)
{
	static HashTable *keywords = NULL;

	if (UNLIKELY(keywords == NULL)) {
		keywords = alloc_hash_table();
#define K(keyword, tok) hash_table_set(keywords, keyword, (void *) tok)
#include "keywords.inc"
#undef K
	}
	// Returns INVALID_TOK if not found
	return (enum tok_kind) hash_table_get(keywords, keyword);
}
```

## Unix philosophy for functions

> This is the Unix philosophy: Write programs that do one thing and do it well.
>
> --- Doug McIlroy

The Unix philosophy can also apply to subroutines. Ideally, functions should do one thing and do it well. They should have descriptive names to describe the one thing that they do well. Long functions that do many things should be split into logical components.

I tried to use this philosophy when writing my compiler. For example, this is a top-level function called from `main()`.

```c
static void compile_file(char *target_file, const char *source_file)
{
	struct ast ast;

	ast = parse_file(source_file);
	check_ast(ast);
	compile_ast(target_file, ast);
	free_ast(ast);
}
```

This logically divides the three main stages of the compilation pipeline: parsing `parse_file()`, type checking `check_ast()`, and code generation `compile_ast()`.

## Conclusion

Overall, C is an ancient relic with few features by today's standards. However, many abstractions from newer languages can be emulated (albeit sub-optimally) in C.
