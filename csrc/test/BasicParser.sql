--Malformed SQL statements
--specific bugfix
--https://trello.com/c/8YXSi9LP
select a+1 from a, b where ( a + 1 ) > ( c + 1) / d;
select foo , baz , (1+a) from bar , bat , boo where baz < 1;
select a from foo as \"1\";
--garbage strings 
--
--;
--12345;
--as0dakMACZAS:Lqpo234u9124-0123012;l1wm,d./asmaL/SNQ/LKHJE12P4 9I-305R93=WEFPK MZXC:LS  ALKSJALSIJDLAKSJDAL SJDAKSDJALS239122-/.AS,FG ,ZC?(*A&SDMNAS FKAJSDLAS
--asdAS()_!*#12 ,as D!123
--
--syntax errors
--elsec a from A,B,C;
--select a,b fro A,B,C;
--select from A,B,C;
--select a,b,c A,B,C;
--select a,b,c from;
--select a,b,c fromA,B,C;
--Data Manipulation Language
--SELECT statments
--select list constructions
select a from A,B,C;
select a+1 from A,B,C;
select a+1, b from A,B,C;
select a+1, a from A,B,C;
select a,b,c from A,B,C;
select A.a, A.b, C.c from A,B,C;
select a from A,B,C;
select * from A,B,C;
select a,* from A,B,C;
select a , * , b , * , c from A,B,C;
select a/2, b+2, c, d*2 from A,B,C;
select a/(2+4), b+2, c, d*2 from A,B,C;
select a/(2+4), b+(6/(3-1)), c, d*2 from A,B,C;
--select list aliases
select a as c1 , b as c2 , c as c3 from A , B , C;
select a/(2+4) as c1 , b+(6/(3-1)) as c2 , c as c3 , d*2 as c4 from A,B,C;
--table expression aliases
select tab1.a / (2 + 4) as c1 , tab1.b+(6/(3-1)) as c2 , tab2.c as c3 , tab3.d * 2  as c4 from A as tab1 ,B as tab2 ,C as tab3;
