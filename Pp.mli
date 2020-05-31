(**
   {1 Pretty Printing}

   Pretty printing is the problem of finding a good layout for structured text
   under some constraints.  John Huges has advanced the design of pretty
   printers considerably by taking an algebraic approach: Pretty printers are a
   set of operators like horizontal or vertical concatenation which obey
   algebraic rules.  This has lead to a consistent pretty printing library; a
   variant of his library is part of some Haskel implementations.  Based on
   Hughes' work Philip Wadler has proposed another algebraic pretty printer.  It
   uses only six operators and a uniform document model that is well suited for
   pretty printing tree structures like source code.  Like Hughes' he has also
   suggested an implementation of his approach in the functional language
   Haskell.  It relies heavily on the lazy evaluation of Haskell and can not be
   easily ported to a strict language without loss of efficiency.  This module
   implements the pretty printer as proposed by Wadler but adopted for the
   strict evaluation of Ocaml.

   @author Christian Liding
   @author Norman Ramsey
   @author Till Varoquaux
 *)
type t
val empty : t

(**
   The infix operator [x ^^ y] concatenates two documents [x] and [y] into a
   larger one.  It does not introduce any space or other separation between the two
   original documents.
 *)
val (^^) : t -> t -> t

(**
   The [text] function turns a string into a document. The pretty printed
   representation of the resulting document is exactly the string [text] was
   applied to.
*)
val text : string -> t

(**
   The important points in a document are so-called [break]s.  A [break] can
   be either represented by a single space or a newline followed by a number of
   spaces.  The pretty printer makes this decision based on the available space.
   So think of a [break] as a space that might come out as a newline.  To give
   the pretty printer enough flexibility documents must be joined with
   [break]s: [x ^^ break ^^ y].
 *)
val break : t

(**
   The space character used by [break] my be not always appropriate. The
   function [breakWith s] behaves like [break] except that it uses a user
   supplied string [s] instead of the space.
 *)
val breakWith : string -> t

(** {2 Nesting and Grouping} *)

(**
   When the pretty printer decides to represent a [break] as a newline it also
   prints some spaces after it to indent the following line.  The number of
   spaces is controlled by the [nest] operator that takes a document as
   argument: [nest n d].  All breaks turned to newlines inside document [d]
   are followed by [n] spaces.  The [nest] operator nests properly such that
   it takes the spaces introduced by [nest]s on the outer level also into
   account.
 *)
val nest : int -> t -> t

(**
   The pretty printer considers the representation of [break]s not one by one but
   looks at all [break]s of a sub-document. Documents are structured into
   sub-documents by group-operators. Different group operators exist to control
   the behavior of the pretty printer.  A group operator takes a document and
   let it become a group.  The [hgrp] operator creates a {i horizontal}
   group.  Breaks inside a [hgrp] are never turned into newlines but always come
   out as spaces.  This group has a very limited usefulness because it easily
   overruns any given line length.  )
 *)
val hgrp : t -> t

(**
   The [vgrp] operator creates a {i vertical} group.  All [break]s inside a
   [vgrp] are represented as newlines followed by spaces.  Although all [break]s
   come out as newlines the indentation of lines inside the group may differ:
   nesting is independent of grouping and thus different nesting levels can be
   active in the same group.  Because of the fixed pretty printing strategy
   [vgrp]s are used mostly at the top level of documents only.)
 *)
val vgrp : t -> t

(**
   The {i automatic} group [agrp] is the most versatile.  Breaks inside this
   group are either all turned into newlines (followed by spaces), or into
   spaces.  Subgroups are, of course, not affected but considered individually.
 *)
val agrp : t -> t

(**
   The break policy inside an [agrp] is fixed for all breaks of the group.
   Inside a {i flexible} group [fgrp] each [break] is considered individually:
   when the document up to the next [break] fits into the current line the
   [break] comes out as space.  Otherwise it comes out as newline followed by
   spaces.
*)
val fgrp : t -> t

(** {2 Pretty Printing}

   After small documents have been assembled to a larger one this finally can be
   printed either to a string, or a file.  Printing to a string is a costly
   operation and should be avoided for large documents.  Both functions for
   pretty printing take a line width as argument.  The pretty printer tries hard
   not to overrun this limit.  However, very long [text] documents or [hgrp]s can
   cause overruns anyway.

   It is usually a good idea to stress test your document by looking at output
   printed for a small line width.  This helps to detect flaws in the document
   creation.

   The documents passed to the pretty printing functions are wrapped by a
   virtual [agrp]. To escape from this default behavior you can easily wrap your
   document on the outer level with another group such that the [agrp] will have
   no effect.
 *)

val ppToString : int -> t -> string
val ppToFile : out_channel -> int -> t -> unit

(** {2 Auxiliaries}
   When using the pretty printer it turned out that some functions are
   frequently defined.
 *)

(**
   A list of objects which are seperated by some separator is very common.  The
   [list sep f] function takes care to insert the separator only bewteen objects
   but not at the end of the list.  It creates a [sep] separated list.  Individual
   items are printed using [f].  For the common case where commas are used for
   separating we also provide an extra definition.
 *)
val list      : t -> ('a -> t) -> 'a list -> t
val commalist : ('a -> t) -> 'a list -> t

(**
   Instead of writing [x ^^ break ^^ y] to insert a [break] it is convenient to
   define an operator for this: [x ^/ y] joins [x] and [y] with a [break].
 *)
val (^/)      : t -> t -> t

(**
   A [block] contains objects [xs] formatted by [f] and enclosed by curly
   braces.  Its body will be indented in case it does not fit on a single line.
 *)
val block     : ('a -> t) -> 'a list -> t
