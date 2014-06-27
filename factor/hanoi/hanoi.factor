! Copyright (C) 2014 Your name.
! See http://factorcode.org/license.txt for BSD license.

USING: kernel math math.parser sequences io math.primes.factors math.primes ;
USING: io.streams.string prettyprint ;
IN: hanoi

: fact ( n -- factn )
    dup 2 < [ drop 1 ] [ dup 1 - fact * ] if ;

: move ( a  b -- str )
    [ number>string ] bi@ " vers " glue ;

: other ( a b -- c ) + 6 swap - ;

: partial ( a b -- a c ) [ dup ] dip other ;

: partial2 ( a b -- c b ) dup [ other ] dip ;

: hanoi ( start end n -- )
    dup 1  =
    [ drop move print ]
    [
        3dup 1 - [ partial ] dip hanoi
        [ 2dup move print partial2 ] dip 
        1 - hanoi
    ] if ; 
