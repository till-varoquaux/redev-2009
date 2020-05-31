(*
 * Users build document of type [t].  The different kinds of documents are
 * mapped to different constructors.
 *
 *
 * {2 Auxiliaries}
 *
 * To make groups more visible during debugging set [debug] to [true].
 *
 *)
(* Papers to read : http://www.cs.chalmers.se/~rjmh/Software/NewPP.hs *)
(* http://www.intellifactory.com/articles/PrettyPrinter.aspx *)
let debug   = false
let strlen  = String.length

(*
 * Unix uses a single character to represent a newline.  Other operating systems
 * use different or more characters.  So we better abstract from this.
 *)
let nl      = "\n"

(*
 * There are four different kinds of groups or modes at the user level; they are
 * captured by [gmode]. The names used for the different groups do not match the
 * names of the functions in the interface---this is a historic legacy.
 *)
type gmode =
    | GFlat             (* hgrp *)
    | GBreak            (* vgrp *)
    | GFill             (* fgrp *)
    | GAuto             (* agrp *)


type t =
    | DocNil
    | DocCons           of t * t
    | DocText           of string
    | DocNest           of int * t
    | DocBreak          of string
    | DocGroup          of gmode * t

(*
 * The constructor functions are mapped straight to the [t] type.  When
 * [debug] is [true] additional markup for the groups with run time computed
 * pretty printing is added.
 *)
let (^^) x y            = DocCons(x,y)
let empty               = DocNil
let text s              = DocText(s)
let nest i x            = DocNest(i,x)
let break               = DocBreak(" ")
let breakWith s         = DocBreak(s)

let hgrp d              = DocGroup(GFlat, d)
let vgrp d              = DocGroup(GBreak,d)
let agrp d              = if   debug
                          then DocGroup(GAuto, text "[" ^^ d ^^ text "]")
                          else DocGroup(GAuto, d)
let fgrp d              = if   debug
                          then DocGroup(GFill, text "{" ^^ d ^^ text "}")
                          else DocGroup(GFill, d)

(*
 * Pretty printing takes two steps: users build documents of type [t].  The
 * pretty printer transforms a [t] value into a simple document of type [sdoc].
 * The transformation from [t] to [sdoc] decides about the representation of
 * each [break] in the original document.  A [sdoc] value can be easily printed
 * into a string or file.  The algebraic properties of the pretty printer
 * guarantee that every [t] value can be turned into a much more restricted
 * [sdoc] value.
 *
 * A [sdoc] value is either an empty document ([SNil]), or a literal text
 * followed by some [sdoc] ([SText]), or a newline, followed by a number of
 * spaces and another [sdoc] value.
 *)
type sdoc =
    | SNil
    | SText             of string * sdoc
    | SLine             of int    * sdoc    (* newline + spaces *)

let sdocToString sdoc =
    let buf = Buffer.create 256 in
    let rec loop = function
        | SNil              -> ()
        | SText(s,d)        -> ( Buffer.add_string buf s
                               ; loop d
                               )
        | SLine(i,d)        -> let prefix = String.make i ' ' in
                               ( Buffer.add_char   buf '\n'
                               ; Buffer.add_string buf prefix
                               ; loop d
                               )
    in
        ( loop sdoc
        ; Buffer.contents buf
        )

let sdocToFile oc doc =
    let pstr = output_string oc in
    let rec loop = function
        | SNil          -> ()
        | SText(s,d)    -> pstr s; loop d
        | SLine(i,d)    -> let prefix = String.make i ' '
                           in  pstr nl;
                               pstr prefix;
                               loop d
    in
        loop doc

(*
 * During pretty printing [break]s in [agrp]s are either turned into spaces or
 * newlines.  After this decision is made only three groups or modes remain:
 *
 * - [Flat]:  every [break] is printed as space.
 * - [Break]: every [break] is printed as newline.
 * - [Fill]:  a [break] at the end of the line is printed as newline and as
 *            space otherwise.
 *)
type mode =
    | Flat
    | Break
    | Fill

(*
 * Pretty printing means do decide whether an [agrp]'s [break]s are spaces or
 * newlines.  This is done by [format] that takes three arguments: the (maximal)
 * line width [w], the number of characters used already used on the current
 * line [k], and a list of documents.  The list results from flattening the
 * tree-structure introduced by the [DocCons] constructor.
 *
 * Each document in the list comes with two more informations: the number [i] of
 * spaces that are to be printed after a newline, and the [mode] (or group) the
 * document is part of.  The mode [m] dictates the appearance of [break]s:
 * inside a [Flat] group a [DocBreak(s)] just becomes [s] (usually a space),
 * inside a [Break] group a [DocBreak(s)] becomes a newline followed by [i]
 * spaces.  When in [Fill] mode [break]s only become a newline in case otherwise
 * the current line length would be exceeded.
 *
 * The mode [m] is derived from the original group which is straight forward
 * except for an [agrp].  To decide about the mode for the contents of an [agrp]
 * [format] uses [fit] to check, whether the whole group fits into the rest of
 * the line.  In case it does, the mode will be [Flat] which means its [break]s
 * will be spaces.
 *
 * The [fits] predicate treats all groups it encounters as flat groups.  This
 * implements just the policy that a group including all its sub groups must fit
 * into the current line in order to have its [break]s rendered as spaces.  The
 * predicate is efficient because it can stop whenever it encounters a real new
 * line (from a [vgrp])i, or it exceeds the current line width, or it reaches the
 * end of the group---whatever happens first.
 *)
let rec fits w = function
    | _ when w < 0                   -> false
    | []                             -> true
    | (i,m,DocNil)              :: z -> fits w z
    | (i,m,DocCons(x,y))        :: z -> fits w ((i,m,x)::(i,m,y)::z)
    | (i,m,DocNest(j,x))        :: z -> fits w ((i+j,m,x)::z)
    | (i,m,DocText(s))          :: z -> fits (w - strlen s) z
    | (i,Flat, DocBreak(s))     :: z -> fits (w - strlen s) z
    | (i,Fill, DocBreak(_))     :: z -> true
    | (i,Break,DocBreak(_))     :: z -> true
    | (i,m,DocGroup(_,x))       :: z -> fits w ((i,Flat,x)::z)


(* format is cps to avoid stack overflow *)
let cons  s post z = post (SText (s, z))
let consl i post z = post (SLine (i, z))
let rec format w k l post = match l with
    | []                             -> post SNil
    | (i,m,DocNil)              :: z -> format w k z post
    | (i,m,DocCons(x,y))        :: z -> format w k ((i,m,x)::(i,m,y)::z) post
    | (i,m,DocNest(j,x))        :: z -> format w k ((i+j,m,x)::z) post
    | (i,m,DocText(s))          :: z -> format w (k + strlen s) z (cons s post)
    | (i,Flat, DocBreak(s))     :: z -> format w (k + strlen s) z (cons s post)
    | (i,Fill, DocBreak(s))     :: z -> let l = strlen s in
                                            if   fits (w - k - l) z
                                            then format w (k+l) z (cons s post)
                                            else format w  i    z (consl i post)
    | (i,Break,DocBreak(s))     :: z -> format w i z (consl i post)
    | (i,m,DocGroup(GFlat ,x))  :: z -> format w k ((i,Flat ,x)::z) post
    | (i,m,DocGroup(GFill ,x))  :: z -> format w k ((i,Fill ,x)::z) post
    | (i,m,DocGroup(GBreak,x))  :: z -> format w k ((i,Break,x)::z) post
    | (i,m,DocGroup(GAuto, x))  :: z -> if fits (w-k) ((i,Flat,x)::z)
                                        then format w k ((i,Flat ,x)::z) post
                                        else format w k ((i,Break,x)::z) post
(*
 * The pretty printing functions called by the user just enclose the user
 * document with a virtual [agrp] and start [format]'ing.
 *)

let ppToString w doc =
  format w 0 [0,Flat,agrp(doc)] sdocToString

let ppToFile oc w doc = format w
  0 [0,Flat,agrp(doc)] (sdocToFile oc)

(* {2 Useful functions} *)
let rec list sep f xs =
    let rec loop acc = function
        | []    -> acc
        | [x]   -> acc ^^ f x
        | x::xs -> loop (acc ^^ f x ^^ sep) xs
    in
        loop empty xs

let commalist f = list (text "," ^^ break) f

let (^/) x y   = x ^^ break ^^ y

let block f xs =
    text "{"
    ^^ nest 4 begin
          break
       ^^ list break f xs
       end
    ^/ text "}"

