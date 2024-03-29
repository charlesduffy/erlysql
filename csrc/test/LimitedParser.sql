select a, b, c from A;
select a, b, c from A where a < 1;
select a, b, c from A where a < 1 and b in (1,2,3,4,5);
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
--select a from foo as \"1\";
--select \"1\".a from foo as \"1\";
select tab1.a as col1, tab2.b as col2 from A as tab1, B as tab2 where col1 < 20 and col2  > 5 and col1 > col2 ;
SELECT n_name + 1 FROM customer, orders, lineitem, supplier, nation, region WHERE c_custkey = o_custkey AND l_orderkey = o_orderkey AND l_suppkey = s_suppkey AND c_nationkey = s_nationkey AND s_nationkey = n_nationkey AND n_regionkey = r_regionkey AND r_name = 'ASIA';
