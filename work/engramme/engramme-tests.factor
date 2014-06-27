! Copyright (C) 2014 Your name.
! See http://factorcode.org/license.txt for BSD license.
USING: tools.test engramm io.streams.string ;
IN: engramme.tests

[ "1" ] [ 1 >engramme ] unit-test 
[ "(1)" ] [ 2 >engramme ] unit-test
[ "(01)" ] [ 3 >engramme ] unit-test
[ "((1))" ] [ 4 >engramme ] unit-test
[ "((1)(1)101)" ] [ 1980 >engramme ] unit-test
[ "(0010000000000000000000000000000000000000000000000000000000000000000000000000001)" ] [ 2005 >engramme ] unit-test