# CONSPATH

```lisp
(match '(:a (car :b))
  '((:a (:b x)
        (:b y)
        (:c z))))

;; => ((x y))
```

Conspath is a simple path-like matching system for "structured" lists
and other objects, similar to XPath for XML.  It provides some
regex-like facilities:

```lisp
;; The "+" matcher means "one-or-more"
(match '(:a (+ :b))
  '((:a
     (:b x (:b n))
     (:c (:b y z)))))

;; => ((N))
```

It is also easily extended by implementing a simple method:

```lisp
(defmethod match-complex ((match (eql 'cl:and)) args item)
  (when (every #'identity
               (mapcar (lambda (x) (sub-match x item nil)) args))
    (found item)))
```

## Input

Conspath generally assumes input is a *list of lists* in the following
form:

```lisp
((A0 (B0 ...) [... (Bn ...)])
  :
 (An (B0 ...) [... (Bn ...))))
```

Where A and B represent various atoms-to-match.  There are, however,
ways to match non-list elements of A or B, so Conspath is pretty
flexible.

However, if you are looking at doing recursive destructuring or
similar, solutions such as (optima)[https://github.com/m2ym/optima]
are probably more suited to such problems.

## Conspaths

Matching is done by specifying a *conspath*.  Conspaths are lists of
elements-to-match, similar to path or regular expression.  Each
element is called a "matcher".

What is returned is a series of "found" items.  These vary by the type
of matcher used.

The simplest matches are as follows:

- Symbol, string: match the `CAR` of a list, finding the `CDR`.
- Integer: Match and find the Nth item.
- Function: Each list is passed to the function; the function should
  use `CONSPATH:FOUND` on any items deemed relevant.
- `*`: Wildcard, match any list (can still match a literal * with
  the complex matcher QUOTE).  This does not, itself, find any items.
- List: A complex matcher, see complex matchers
- `null`: The implicit matcher at the end of the conspath; this finds
  any item.

For example:

```lisp
(match '(:a * :b)
  '((:a
     (:b x (:b n))
     (:c (:b y z)))))

;; Note this has 3 matches:
;; => ((X (:B N)) (N) (Y Z))
```

If the match above was `(:a *)` instead of `(:a * :b)`, this would
find *every sub-list of `:a`* because of the null matcher.

Indexes:

```lisp
(match '(:a 2)
  '((:a
     (0 1 2)
     (1 2 3)
     (2 2 2))))

;; => (2 3 2)
```

## Complex Matchers

Beyond the simple matchers, complex matchers expand the capabilities
of Conspath greatly:

- `(QUOTE x)`: Match x in the CAR of a list.  This is useful for
  numbers, `*`, or other items that the simple matching
  specializes.
- `(* M...)`: Match `M...` zero or more times.  Greedy.
- `(+ M...)`: Match `M...` one or more times.  Greedy.
- `(AND (M...) ...)`: Match if every matcher matches.  Finds the
  *topmost node* in the matching subpath.
- `(OR (M...) ...)`: Match if *any* matcher matches.  Finds the
  *topmost node* in the matching subpath.
- `(ATOM x)`: Match x, but match an atom instead of the CAR of a list;
  e.g., to match `:b` in `((:a :b))`, the matcher would be `(:a (atom
  :b))`.
- `(CAR M...)`: Match the path `M...` and find the `CAR`.
- `(FUNCTION x)`: This is the same as the simple matcher with a
  literal function specified. (If you specify `#'function`, you are
  calling this instead of the simple matcher, of course.)

**Note:** the symbols above, such as `QUOTE`, `*`, `+`, `FUNCTION`,
etc, are from the `COMMON-LISP` package.

## Extension

Conspath provides a simple way to extend it by specializing
`MATCH-COMPLEX` on `(EQL 'symbol)`.  This is used internally to
implement all the complex matchers above.

`MATCH-COMPLEX (MATCH ARGS ITEM)`

- `MATCH` is the symbol to specialize.  Do not specialize on symbols
  from `COMMON-LISP` or keywords; these are reserved for use by future
  Conspath expansion.  The exception is patch submissions, which
  should specialize on `COMMON-LISP` symbols, or new symbols in
  `CONSPATH.MATCHERS`.
- `ARGS` are all arguments to the complex matcher.  These may be
  handled arbitrarily.
- `ITEM` is the current item being matched.  It may be a list, or
  anything else.

Some helper functions are provided:

- `(match-next LIST)`: This will continue matching with the next
  element in the path on `LIST`.
- `(sub-match CONSPATH ITEM &optional (COLLECT-P T))`:
  This will perform a sub-match using `CONSPATH` on `LIST`.  Unlike
  the function `MATCH`, it does not expect to take a list-of-items.
  The usual use-case for this is matching on the `ITEM` parameter to
  `MATCH-COMPLEX`.  If `COLLECT-P` is `NIL`, items that would
  otherwise be added to the found list are instead returned.
- `(sub-match-list CONSPATH LIST &optional (COLLECT-P T))`:
  This is essentially identical to `SUB-MATCH`, except it takes a
  list of items, like `MATCH`.
- `(found ITEM)`: Add an item to the found list which is ultimately
  returned.

## Notes

There are probably a few caveats:

- This documentation makes things out to be a lot more complicated
  than they really are.
- This is really not built (or tested) for speed; it uses recursion
  quite heavily and doesn't compile any matchers for reuse.
- The code is pretty simple and it should probably be more
  complicated.  There are probably hard things you can't make it do.

## License

This is (C) 2013, Ryan Pavlik, and licensed under the [BSD
2-Clause](http://opensource.org/licenses/BSD-2-Clause) license.
