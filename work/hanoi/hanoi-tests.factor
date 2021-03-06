! Copyright (C) 2014 Your name.
! See http://factorcode.org/license.txt for BSD license.
USING: tools.test hanoi io.streams.string ;
IN: hanoi.tests

[ "2 vers 3" ] [ 2 3 move ] unit-test

[ 3 ] [ 1 2 other ] unit-test
[ 3 ] [ 2 1 other ] unit-test
[ 2 ] [ 1 3 other ] unit-test
[ 1 ] [ 2 3 other ] unit-test

[ "1 vers 2\n" ] [ [ 1 2 1 hanoi ] with-string-writer ] unit-test

[ "1 vers 3\n1 vers 2\n3 vers 2\n1 vers 3\n2 vers 1\n2 vers 3\n1 vers 3\n"]
[ [ 1 3 3 hanoi ] with-string-writer ]
unit-test

[ "1" ] [ 1 >engramme ] unit-test 
[ "(1)" ] [ 2 >engramme ] unit-test
[ "(01)" ] [ 3 >engramme ] unit-test
[ "((1))" ] [ 4 >engramme ] unit-test
[ "((1)(1)101)" ] [ 1980 >engramme ] unit-test
[ "(0010000000000000000000000000000000000000000000000000000000000000000000000000001)" ] [ 2005 >engramme ] unit-test