--Malformed SQL statements
--garbage strings 
--
--;
12345;
as0dakMACZAS:Lqpo234u9124-0123012;l1wm,d./asmaL/SNQ/LKHJE12P4 9I-305R93=WEFPK MZXC:LS  ALKSJALSIJDLAKSJDAL SJDAKSDJALS239122-/.AS,FG ,ZC?(*A&SDMNAS FKAJSDLAS
asdAS()_!*#12 ,as D!123
--
--syntax errors
elsec a from A,B,C;
select a,b fro A,B,C;
select from A,B,C;
select a,b,c A,B,C;
select a,b,c from;
select a,b,c fromA,B,C;
--Data Manipulation Language
--SELECT statments
--select list constructions
select a from A,B,C;
select a+1 from A,B,C;
select a+1, b from A,B,C;
select a+1, a from A,B,C;
select a,b,c from A,B,C;
select A.a, A.b, C.c from A,B,C;
select * from A,B,C;
select a,* from A,B,C;
select a/2, b+2, c, d*2 from A,B,C
select a/(2+4), b+2, c, d*2 from A,B,C
select a/(2+4), b+(6/(3-1)), c, d*2 from A,B,C
select a/(2+4) as c1 , b+(6/(3-1)) as c2 , c as c3 , d*2 as c4 from A,B,C
select a/(2+4) as tab1.c1 , b+(6/(3-1)) as tab1.c2 , c as tab2.c3 , d*2  as tab3.c4 from A as tab1 ,B as tab2 ,C as tab3
