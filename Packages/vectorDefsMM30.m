(*
These Algorithms are Copyright Alan A. Barhorst, 1993,1994,1995,1996,1997.  
They are distributed in there present form to the students of ME3331 at Texas 
Tech University for their use in the class and further scholastic study of 
dynamics.  By no means are they to be distributed to other users until 
permission is given by the Copyright holder Alan A. Barhorst.
*)

Print["These Engineering Vector algorithms are copyright Alan A. Barhorst"]


(*Function to check for scalars*)
scalarQ[a_]:=
(Head[a]=!=unitVector)&&(Head[a]=!=Cross)&&(Head[a]=!=unitDyad)

(*Function to check for vectors, Crosses, and Dyads*)
notscalarQ[a_]:=!scalarQ[a]

(*Standard i,j, and k unit vectors are predefined.*)
i:=unitVector[N,"i",1]
j:=unitVector[N,"j",2]
k:=unitVector[N,"k",3]

unitDyad[0,a_]:=0
unitDyad[a_,0]:=0


(*Cross product definitions*)

(*Off[Cross::nonn1];(*Turn off the size error for these defs*)*)


Unprotect[Cross];

Clear[Cross] (*Clear the predifined values for now. Fix the problems later*)

Cross/: Cross[v1_unitVector, v2_unitVector]^2 := 
v1.v1 v2.v2 - (v1.v2)^2

Cross[v1_unitVector,0]:=0
Cross[0,v1_unitVector]:=0
Cross[a_? scalarQ v1_unitVector,0]:=0
Cross[0,a_?scalarQ v1_unitVector]:=0
Cross[0,0]:=0
Cross[v1_unitVector, v1_unitVector]:=0
  
Cross/: Literal[a_?scalarQ Cross[ v1_?notscalarQ , Literal[ Plus[
args2___]]]]:= 
Apply[Cross,Distribute[dummy[a v1,Plus[args2]]],1]
Cross/: Literal[Plus[args1___]~Cross~a_?scalarQ v1_?notscalarQ] := 
Apply[Cross,Distribute[dummy[Plus[args1],a v1]],1]
Cross/: Literal[Cross[ Literal[Plus[args1___]],a_?scalarQ] v1_?notscalarQ] := 
Apply[Cross,Distribute[dummy[Plus[args1],a v1]],1]

Cross[ Literal[ Plus[args1___] ],Literal[ Plus[args2___] ] ]:= 
Plus @@ Distribute[ Cross[{args1},{args2}],List]
Cross[ Literal[ args1___ ],Literal[ Plus[args2___] ] ]:= 
Plus @@ Distribute[ Cross[{args1},{args2}],List]
Cross[ Literal[ Plus[args1___] ],Literal[ args2___ ] ]:= 
Plus @@ Distribute[ Cross[{args1},{args2}],List]

Cross[(a_?scalarQ) v1_unitVector, v2_unitVector]:= a Cross[v1,v2]
Cross[v1_unitVector, (b_?scalarQ) v2_unitVector]:= b Cross[v1,v2]
Cross[(a_?scalarQ) v1_unitVector, (b_?scalarQ) v2_unitVector]:= a b Cross[v1,v2]
Cross[Times[a_?scalarQ, v1_?notscalarQ],Times[b_?scalarQ, v2_?notscalarQ]]:= a b Cross[v1,v2]

Cross/: Literal[v1_unitVector~Cross~(b_?scalarQ) v2_unitVector]:= b Cross[v1,v2]

Cross[v1_unitVector,Cross[v2_unitVector,v3_unitVector]]:=
(v1.v3) v2 - (v1.v2) v3
Cross[Cross[v1_unitVector,v2_unitVector],v3_unitVector]:=
(v1.v3) v2 - (v2.v3) v1

Literal[Cross[(a_?scalarQ) v1_unitVector,
Cross[v2_unitVector,v3_unitVector]]]:=a Cross[v1,Cross[v2,v3]]
Literal[Cross[v1_unitVector,
(a_?scalarQ) Cross[v2_unitVector,v3_unitVector]]]:=a Cross[v1,Cross[v2,v3]]
Literal[Cross[(a_?scalarQ) Cross[v1_unitVector,v2_unitVector],
v3_unitVector]]:=a Cross[Cross[v1,v2],v3]
Cross/: Literal[Cross[Cross[v1_unitVector,v2_unitVector],
(a_?scalarQ)] v3_unitVector]:=a Cross[Cross[v1,v2],v3]
Cross/: Literal[Cross[(a_?scalarQ) Cross[v1_unitVector,
v2_unitVector],(b_?scalarQ)] v3_unitVector]:=
a b Cross[Cross[v1,v2],v3]

(*Cross relations for vectors of same base*)

(* For Vectors i,j,k *)

Literal[Cross[unitVector[N,"i",1],unitVector[N,"j",2]]]:= 
unitVector[N,"k",3]
Literal[Cross[unitVector[N,"i",1],unitVector[N,"k",3]]]:= 
-unitVector[N,"j",2]
Literal[Cross[unitVector[N,"j",2],unitVector[N,"i",1]]]:= 
-unitVector[N,"k",3]
Literal[Cross[unitVector[N,"j",2],unitVector[N,"k",3]]]:= 
unitVector[N,"i",1]
Literal[Cross[unitVector[N,"k",3],unitVector[N,"i",1]]]:= 
unitVector[N,"j",2]
Literal[Cross[unitVector[N,"k",3],unitVector[N,"j",2]]]:= 
-unitVector[N,"i",1]

(*General vectors first of same uvSymbol*)

Cross[v1_unitVector, v2_unitVector]:=
unitVector[v1[[1]],v1[[2]],1] /; v1[[1]] === v2[[1]] &&
v1[[2]] === v2[[2]] && (v1[[3]] === 2 && v2[[3]] === 3)
Cross[v1_unitVector, v2_unitVector]:=
unitVector[v1[[1]],v1[[2]],2] /; v1[[1]] === v2[[1]] &&
v1[[2]] === v2[[2]] && (v1[[3]] === 3 && v2[[3]] === 1)
Cross[v1_unitVector, v2_unitVector]:=
unitVector[v1[[1]],v1[[2]],3] /; v1[[1]] === v2[[1]] &&
v1[[2]] === v2[[2]] && (v1[[3]] === 1 && v2[[3]] === 2)
Cross[v1_unitVector, v2_unitVector]:=
-unitVector[v1[[1]],v1[[2]],1] /; v1[[1]] === v2[[1]] &&
v1[[2]] === v2[[2]] && (v1[[3]] === 3 && v2[[3]] === 2)
Cross[v1_unitVector, v2_unitVector]:=
-unitVector[v1[[1]],v1[[2]],2] /; v1[[1]] === v2[[1]] &&
v1[[2]] === v2[[2]] && (v1[[3]] === 1 && v2[[3]] === 3)
Cross[v1_unitVector, v2_unitVector]:=
-unitVector[v1[[1]],v1[[2]],3] /; v1[[1]] === v2[[1]] &&
v1[[2]] === v2[[2]] && (v1[[3]] === 2 && v2[[3]] === 1)

(*Operations with dyads and vectors*)

Cross[(a_?scalarQ) d1_unitDyad, v2_unitVector]:= a Cross[d1,v2]
Cross[v1_unitVector, (b_?scalarQ) d2_unitDyad]:= b Cross[v1,d2]
Cross[(a_?scalarQ) d1_unitDyad, (b_?scalarQ) v2_unitVector]:= a b Cross[d1,v2]

Cross[(a_?scalarQ) v1_unitVector, (b_?scalarQ) d2_unitDyad]:= a b Cross[v1,d2]

Cross/: (b_?scalarQ) Literal[v1_unitVector~Cross~d2_unitDyad]:= b Cross[v1,d2]

Cross/: Literal[v1_unitVector~Cross~(b_?scalarQ) d2_unitDyad]:= b Cross[v1,d2]


Cross[d1_unitDyad,v1_unitVector]:= unitDyad[d1[[1]],Cross[d1[[2]],v1]]
Cross[v1_unitVector,d1_unitDyad]:= unitDyad[Cross[v1,d1[[1]]], d1[[2]]]

Protect[Cross];

(*Dot product definitions*)

Unprotect[Dot];

v1_unitVector.v1_unitVector:=1

Dot[v1_unitVector, v2_unitVector]:=0 /; 
v1[[1]] === v2[[1]]  && (v1[[3]] =!= v2[[3]])

v1_unitVector . 0 := 0
0 . a_ := 0
Dot[_,0] := 0

v1_unitVector . Cross[v1_unitVector,v2_unitVector] := 0
v1_unitVector . Cross[v2_unitVector,v1_unitVector] := 0

Cross[v1_unitVector,v2_unitVector] . v1_unitVector := 0
Cross[v1_unitVector,v2_unitVector] . v2_unitVector := 0

Dot[Cross[v1_unitVector,v2_unitVector],
Cross[v1_unitVector,v2_unitVector]]:=1

Dot/: Literal[Dot[(-v1_unitVector),v2_unitVector]]:=-Dot[v1,v2]
Dot/: Literal[v1_unitVector.Times[(a_?scalarQ), v2_unitVector] ]:= 
a v1.v2
Literal[Dot[(a_?scalarQ) v1_unitVector, 
(b_?scalarQ) v2_unitVector]]:=a b Dot[v1,v2]
Literal[Dot[(a_?scalarQ v1_unitVector), 
(b_?scalarQ v2_unitVector)]]:=a b Dot[v1,v2]
Literal[Dot[a_?scalarQ v1_unitVector, 
(b_?scalarQ v2_unitVector)]]:=a b Dot[v1,v2]
Literal[Dot[(a_?scalarQ v1_unitVector), 
b_?scalarQ v2_unitVector]]:=a b Dot[v1,v2]
Literal[Dot[(a_?scalarQ v1_unitVector),
v2_unitVector]]:=a Dot[v1,v2]

Dot/: Literal[a_?scalarQ Dot[ v1_?notscalarQ , Literal[ Plus[args2___]]]]:= 
Apply[Dot,Distribute[dummy[a v1,Plus[args2]]],1]
Dot/: Literal[Dot[ Literal[ Plus[args1___] ],a_?scalarQ] v1_?notscalarQ] := 
Apply[Dot,Distribute[dummy[Plus[args1],a v1]],1]
Dot/: Literal[Dot[a_?scalarQ v1_?notscalarQ , Literal[ Plus[args2___]]]]:= 
Apply[Dot,Distribute[dummy[a v1,Plus[args2]]],1]
Dot/: Literal[Dot[ v1_?notscalarQ , Literal[ Plus[args2___]]]]:= 
Apply[Dot,Distribute[dummy[v1,Plus[args2]]],1]
Dot/: Literal[Dot[ Literal[ Plus[args1___] ],v1_?notscalarQ] ] := 
Apply[Dot,Distribute[dummy[Plus[args1], v1]],1]
Dot/: Literal[Dot[ Literal[ Plus[args1___] ],a_?scalarQ v1_?notscalarQ] ] := 
a Apply[Dot,Distribute[dummy[Plus[args1], v1]],1]
  
Dot[ Literal[ Plus[args1___] ],
Literal[ Plus[args2___] ] ]:= 
Apply[Dot,Distribute[dummy[Plus[args1],Plus[args2]]],1]

Dot/: Cross[v1_unitVector,v2_unitVector] . 
(a_?scalarQ) v3_unitVector := a Cross[v1,v2] . v3
Dot/: Times[(a_?scalarQ),
Cross[v1_unitVector,v2_unitVector]] . v3_unitVector := a Cross[v1,v2] . v3
Dot/: v1_unitVector . Times[(a_?scalarQ),
Cross[v2_unitVector,v3_unitVector]] := a v1 . Cross[v2,v3] 
Dot/: v1_unitVector . Cross[v2_unitVector,v3_unitVector] := 
Cross[v2,v3] . v1 

Dot[d1_unitDyad,v1_unitVector] := v1.d1[[2]] d1[[1]]
Dot[v1_unitVector,d1_unitDyad] := v1.d1[[1]] d1[[2]]
Dot[a_?scalarQ d1_unitDyad,v1_unitVector] := a Dot[d1,v1]
Dot[v1_unitVector,a_?scalarQ d1_unitDyad] := a Dot[v1,d1]
Dot[d1_unitDyad,a_?scalarQ v1_unitVector] := a Dot[d1,v1]
Dot[a_?scalarQ v1_unitVector,d1_unitDyad] := a Dot[v1,d1]
Dot/: Literal[Dot[d1_unitDyad,a_?scalarQ] v1_unitVector] := a Dot[d1,v1]
Dot[a_?scalarQ d1_unitDyad,b_?scalarQ v1_unitVector] := a b Dot[d1,v1]
Dot[a_?scalarQ v1_unitVector,b_?scalarQ d1_unitDyad] := a b Dot[v1,d1]
Dot[Times[a_?scalarQ, v1_?notscalarQ],Times[b_?scalarQ, v2_?notscalarQ]] := a b Dot[v1,v2]

Dot[Times[a_?scalarQ, v1_?notscalarQ], v2_?notscalarQ] := a Dot[v1, v2]
Dot[v1_?notscalarQ, Times[a_?scalarQ, v2_?notscalarQ]] := a Dot[v1, v2]

Dot[d_unitDyad, v_?notscalarQ] := (d[[2]].v) d[[1]]
Dot[v_?notscalarQ, d_unitDyad] := (d[[1]].v) d[[2]]

(*Canonical order for pattern matching*)
Dot[v1_unitVector,v2_unitVector]:= Dot[v2,v1] /; !(Order[v1,v2]===1)

Dot/: Literal[Cross[v3_unitVector,v2_unitVector] . v1_unitVector] := 
-Cross[v1,v2] . v3 /; (Order[v1,v2]===1 && Order[v2,v3]===1)
Dot/: Literal[Cross[v2_unitVector,v3_unitVector] . v1_unitVector] := 
Cross[v1,v2] . v3 /; (Order[v1,v2]===1 && Order[v2,v3]===1)
Dot/: Literal[Cross[v1_unitVector,v3_unitVector] . v2_unitVector] := 
-Cross[v1,v2] . v3 /; (Order[v1,v2]===1 && Order[v2,v3]===1)
Dot/: Literal[Cross[v3_unitVector,v1_unitVector] . v2_unitVector] := 
Cross[v1,v2] .v3 /; (Order[v1,v2]===1 && Order[v2,v3]===1)
Dot/: Literal[Cross[v2_unitVector,v1_unitVector] . v3_unitVector] := 
-Cross[v1,v2] .v3 /; (Order[v1,v2]===1 && Order[v2,v3]===1)

(*In case the vectors are of the same base and in order*)
Dot/: Literal[Cross[v1_unitVector,v2_unitVector] . v3_unitVector ] := 
Cross[v2,v3] . v1 /; 
(v2[[1]]===v3[[1]] && (Order[v1,v2]===1 && Order[v2,v3]===1))

Protect[Dot];

(*derivative definitions*)

omega[base_,base_]:=0

Literal[ DvDt[base_,Plus[args___] ] ]:= 
Plus @@ Distribute[ dummy[base,Plus[args]] ] /. dummy->DvDt

Literal[DvDt[base_,a_ b_]]:=
DvDt[base,a] b + a DvDt[base,b] (*Chain rule for products*)

Literal[DvDt[base_,v_unitVector]]:=omega[base,v[[1]]]~Cross~v

Literal[DvDt[base_,v_Cross]]:=
DvDt[base,v[[1]]]~Cross~v[[2]] + v[[1]]~Cross~DvDt[base,v[[2]]]

Literal[DvDt[base_,a_Dot]]:= DvDt[base,a[[1]]] . a[[2]] +
a[[1]] . DvDt[base,a[[2]]]

Literal[DvDt[base_,a_?scalarQ]]:=Dt[a,t]  (* must be a scalar *)


(*Simplification functions*)
distributeScalars[x_]:=
  Module[{iter},iter=Max[Table[Length[x[[i]]],{i,1,Length[x]}]]; 
    ReplaceRepeated[x,  a_ (b_ + c_)->a b + a c,MaxIterations->iter]] (* 
  through the scalars at level 1*)
unSquare[x_]:=x //.  (a_ + b_)^2->a^2 + 2 a b + b^2
gatherScalars[x_]:= x //. a_ b_ + a_ c_ -> a (b + c)
minDivisions[z_]:=z //. Rational[a_,b_] x_ + Rational[c_,d_] y_ -> (a d x + 
b c y )/(b d)
gatherDivisors[x_]:=x//. a_ b_^n_ + c_ b_^n_ -> (a+c)b^n
stripTimeSpace[y_]:= y //. a_[i__][x__]->SequenceForm[a,i]
symbolQ[x_]:=Head[x]===Symbol
stripIndex[y_]:= y //. a_?symbolQ[i_]->SequenceForm[a,i]
stripTime[y_]:= y//. a_[t]->a
factorDivisor[x_]:=x//.{a_/c_ + b_/c_ -> (a+b)/c}

distributeScalars[0] := 0
distributeScalars[1] := 1

simplerRhs[x_]:=x//. 
{Times[Z[i__],b_,c___]+Times[Z[i__],d_,e___]->Z[i](b c+d e),
Times[co[i__][t_],b_,c___]+Times[co[i__][t_],d_,e___]->co[i][t](b c+d e),
Times[si[i__][t_],b_,c___]+Times[si[i__][t_],d_,e___]->si[i][t](b c+d e)}

simplerIm[x_]:=x//.{m[i__] a_ + m[i__] b_ ->m[i](a + b),
rho[i__] a_ + rho[i__] b_ ->rho[i](a + b),
Times[a_, b___, L[i__]^n_.] +  
Times[c_, d___, L[i__]^n_.] -> (a b + c d) L[i]^n,
phi[i__][t_] a_ + phi[i__][t] b_ ->phi[i][t](a + b),
qp[i__][t_] a_ + qp[i__][t] b_ ->qp[i][t](a + b),
q[i__][t_] a_ + q[i__][t] b_ ->q[i][t](a + b) }

(*gatherVectors[x_]:= Module[{terms,otherterms,vectterms,Crossterms,unitVectorterms,
Crosstermcoeffs,unitVectortermcoeffs,Crossgroup,unitVectorgroup},
terms=List @@ x;
vectterms=Union @ Cases[terms,a__ b_?notscalarQ,{1}];
otherterms=Complement[terms,vectterms];
Crossterms = Union @ Join[Cases[vectterms, _Cross, {2}],
Cases[vectterms, _Cross, {1}],Cases[vectterms, _Cross, {0}]];
unitVectorterms = Union @ Join[Cases[vectterms, _unitVector, {2}],
Cases[vectterms, _unitVector, {1}],Cases[vectterms, _unitVector, {0}]];
Crosstermcoeffs = Map[Coefficient[x,#]&,Crossterms];
unitVectortermcoeffs = Map[Coefficient[x,#]&,unitVectorterms];
Crossgroup = Crosstermcoeffs*Crossterms;
unitVectorgroup = unitVectortermcoeffs*unitVectorterms;
Plus @@ Join[otherterms,unitVectorgroup,Crossgroup] ]*)

gatherVectors[x_]:=Collect[x,a_?notscalarQ]

findVectors[x_]:= Module[{Crossterms,unitVectorterms,Crosstermcoeffs,
unitVectortermcoeffs,Crossgroup,unitVectorgroup},
Crossterms = Union @ Join[Cases[x, _Cross, {2}],
Cases[x, _Cross, {1}],Cases[x, _Cross, {0}]];
unitVectorterms = Union @ Join[Cases[x, _unitVector, {2}],
Cases[x, _unitVector, {1}],Cases[x, _unitVector, {0}]];
Join[unitVectorterms,Crossterms] ]

findDots[x_]:=Union[Cases[x,_Dot^n_.,Infinity]]

gatherDots[x_]:=Module[{terms,dotterms,dot2terms,otherterms,dots,dots2,
dotscoef,dots2coef,dotsgathered,dots2gathered},
terms=List @@ x;
dotterms=Union @ Cases[terms,a__ b_Dot,{1}];
dot2terms=Union @ Cases[terms,a__ Power[_Dot,2]|Times[Dot[b_,c_],Dot[d_,e_]],{1}];
otherterms=Complement[terms,dotterms,dot2terms];
dots=Union @ Cases[dotterms,_Dot,{2}];
dotscoef=Map[Coefficient[x,#]&,dots];
dots2=Union @ Cases[dot2terms, Power[_Dot,2],{2}];
dots2coef=Map[Coefficient[x,#]&,dots2];
dotsgathered=dotscoef*dots;
dots2gathered=dots2coef*dots2;
Plus @@ Join[otherterms,dotsgathered,dots2gathered] ]

gatherDots2[x_]:=Module[{terms,dotterms,dot2terms,otherterms,dots,dots2,
dotscoef,dots2coef,dotsgathered,dots2gathered},
terms=List @@ x;
dotterms=Union @ Cases[terms,a_ b_Dot /;FreeQ[a,c_Dot],{1}];
dot2terms=
      Union @ Cases[
          terms,(a_. Power[_Dot,2])|(a_. Times[Dot[b_,c_],Dot[d_,e_]]),{1}];
otherterms=Complement[terms,dotterms,dot2terms];
dots=Union @ Cases[dotterms,_Dot,{2}];
dotscoef=Map[Coefficient[x,#]&,dots];
dots2=Union @ 
        Cases[dot2terms, Power[_Dot,2]|Times[Dot[b_,c_],Dot[d_,e_]],{2}];
dots2coef=Map[Coefficient[x,#]&,dots2];
dotsgathered=dotscoef*dots;
dots2gathered=dots2coef*dots2;
Plus @@ Join[otherterms,dotsgathered,dots2gathered] ]

gatherDotsInCoeffs[x_]:= 
Module[{Crossterms,unitVectorterms,Crosstermcoeffs,
unitVectortermcoeffs,Crossgroup,unitVectorgroup},
Crossterms = Union @ Cases[x, _Cross, {2}];
unitVectorterms = Union @ Cases[x, _unitVector, {2}];
Crosstermcoeffs = Map[Coefficient[x,#]&,Crossterms];
Crosstermcoeffs = Map[gatherDots,Crosstermcoeffs];
unitVectortermcoeffs = Map[Coefficient[x,#]&,unitVectorterms];
unitVectortermcoeffs = Map[gatherDots,unitVectortermcoeffs];
Crossgroup = Crosstermcoeffs*Crossterms;
unitVectorgroup = unitVectortermcoeffs*unitVectorterms;
Plus @@ Join[unitVectorgroup, Crossgroup] ]

loosenDots[x_]:=x //. {Dot[a_ v1_Cross,b_ v2_unitVector]->
a b Dot[v1,v2],Dot[a_ v1_Cross,b_ v2_Cross]->
a b Dot[v1,v2],(a__?scalarQ v1_unitVector) . (b__?scalarQ v2_unitVector ~
Cross~ 
v3_unitVector) -> a b v1 . v2 ~Cross~ v3,
Dot[v1_unitVector, a_?scalarQ Dot[b_?scalarQ v2_unitDyad,
        c_?scalarQ v3_Cross]]-> a b c Dot[v1,Dot[v2,v3]]}

loosenDyadDots[x_] := 
 x //. {a_?scalarQ unitDyad[v1_, v2_].v3_?notscalarQ b_?scalarQ -> 
    a b v1 (v2.v3), (a_ v1_?notscalarQ) . v2_?notscalarQ  -> a  v1.v2,
    Dot[(a__ v1_?notscalarQ), ( 
      b__ (c__ v2_?notscalarQ + d__ v3_?notscalarQ ))] -> 
    a Dot[v1, b c v2 + b d v3]}


(* other functions *)
Pvel[vel_,x_]:=D[vel,x]



(* Output Formats*)

Format[unitVector[frame_,uvSymbol_,direction_]]:=
\!\(\(uvSymbol\&^\) \_direction\)

Format[unitVector[frame_,"i",direction_]]:=
\!\(\("i"\&^\) \)
Format[unitVector[frame_,"j",direction_]]:=
\!\(\("j"\&^\) \)
Format[unitVector[frame_,"k",direction_]]:=
\!\(\("k"\&^\) \)


Format[unitDyad[v1_,v2_]]:=SequenceForm[v1,v2]

