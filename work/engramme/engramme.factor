! Copyright (C) 2014 Your name.
! See http://factorcode.org/license.txt for BSD license.

USING: kernel math sequences io ;
USING:  math.primes.factors math.primes math.parser ;
IN: engramme

DEFER: factors>engramme
: >engramme ( n -- str ) 
    dup 2 < 
    [ number>string ]
    [
        "" 2 [ group-factors ] 2dip factors>engramme "(" ")" surround
    ] if ;

: 2pull ( a b c -- b c a ) 2over drop [ drop ] 3dip ;

DEFER: expand
: factors>engramme ( arrays str prime -- str )
    [ dup ] 2dip 2pull empty?
    [
        drop swap drop
    ] [
        [ unclip ] 2dip 2pull first2 expand factors>engramme
    ] if ;

! [ 2 [ first2 expand ] map ] with-string-writer 

: expand ( str prime n pow -- str nextPrime )
    2over =
    [
        >engramme [ append ] curry 2dip drop next-prime
    ] [
        [ next-prime ] 2dip "0" [ append ] curry 3dip expand
    ] if ;

: parse ( str -- engramme ) ;
    