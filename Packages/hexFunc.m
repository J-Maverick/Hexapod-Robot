(* ::Package:: *)

(* ::Title:: *)
(*Initialize*)


Print["Package loaded successfully."]


(* ::Title:: *)
(*Graphic/Animation Functions*)


GetBodyGraphic[CONST_Association]:=Module[{bLength,bSide,vectorLengthB,bHeight,halfBHeight,angledBLength,bShape,bGraphic,bGraphicF},
bLength  = CONST["Body Unit Length"];
bSide = CONST["Body Side Length"];
vectorLengthB = CONST["Body Vector Length"];
bHeight= CONST["Body Height"];
halfBHeight=bHeight/2;
angledBLength = (1+Sqrt[3])(bLength);
bShape = {Polygon[{
{bSide,bLength,-halfBHeight},
{bSide,-bLength,-halfBHeight},
{bLength,-angledBLength,-halfBHeight},
{-bLength,-angledBLength,-halfBHeight},
{-bSide,-bLength,-halfBHeight},
{-bSide,bLength,-halfBHeight},
{-bLength,angledBLength,-halfBHeight},
{bLength,angledBLength,-halfBHeight}}],

Polygon[{
{bSide,bLength,halfBHeight},
{bSide,-bLength,halfBHeight},
{bLength,-angledBLength,halfBHeight},
{-bLength,-angledBLength,halfBHeight},
{-bSide,-bLength,halfBHeight},
{-bSide,bLength,halfBHeight},
{-bLength,angledBLength,halfBHeight},
{bLength,angledBLength,halfBHeight}}],

Polygon[
{{bSide,bLength,-halfBHeight},
{bSide,-bLength,-halfBHeight},
{bSide,-bLength,halfBHeight},
{bSide,bLength,halfBHeight}}],

Polygon[
{{-bSide,bLength,-halfBHeight},
{-bSide,-bLength,-halfBHeight},
{-bSide,-bLength,halfBHeight},
{-bSide,bLength,halfBHeight}}],

Polygon[
{{-bLength,angledBLength,-halfBHeight},
{bLength,angledBLength,-halfBHeight},
{bLength,angledBLength,halfBHeight},
{-bLength,angledBLength,halfBHeight}}],

Polygon[
{{-bLength,-angledBLength,-halfBHeight},
{bLength,-angledBLength,-halfBHeight},
{bLength,-angledBLength,halfBHeight},
{-bLength,-angledBLength,halfBHeight}}],

Polygon[
{{bLength,angledBLength,-halfBHeight},
{bSide,bLength,-halfBHeight},
{bSide,bLength,halfBHeight},
{bLength,angledBLength,halfBHeight}}],

Polygon[
{{bLength,-angledBLength,-halfBHeight},
{bSide,-bLength,-halfBHeight},
{bSide,-bLength,halfBHeight},
{bLength,-angledBLength,halfBHeight}}],

Polygon[
{{-bLength,-angledBLength,-halfBHeight},
{-bSide,-bLength,-halfBHeight},
{-bSide,-bLength,halfBHeight},
{-bLength,-angledBLength,halfBHeight}}],

Polygon[
{{-bLength,angledBLength,-halfBHeight},
{-bSide,bLength,-halfBHeight},
{-bSide,bLength,halfBHeight},
{-bLength,angledBLength,halfBHeight}}]
};

bGraphicF= {bShape,
{Text[Subscript[\!\(\*OverscriptBox[\(a\), \(^\)]\), 1],{vectorLengthB,0,0},{0,1}],Text[Subscript[\!\(\*OverscriptBox[\(a\), \(^\)]\), 2],{0,vectorLengthB,0},{0,1}],
Text[Subscript[\!\(\*OverscriptBox[\(a\), \(^\)]\), 3],{0,0,vectorLengthB},{0,1}],{AbsoluteThickness[1],RGBColor[1,0,0],
Line[{{0,0,0},{vectorLengthB,0,0}}]},{AbsoluteThickness[1],RGBColor[0,1,0],
Line[{{0,0,0},{0,vectorLengthB,0}}]},{AbsoluteThickness[1],RGBColor[0,0,1],
Line[{{0,0,0},{0,0,vectorLengthB}}]}}};

bGraphic = {bShape,{AbsoluteThickness[1],RGBColor[1,0,0],Line[{{0,0,0},{vectorLengthB,0,0}}]},
{AbsoluteThickness[1],RGBColor[0,1,0],Line[{{0,0,0},{0,vectorLengthB,0}}]},{AbsoluteThickness[1],
RGBColor[0,0,1],Line[{{0,0,0},{0,0,vectorLengthB}}]}};

{bGraphicF,bGraphic}
];


GetEnditeGraphic[CONST_Association]:=Module[{vectorLengthE,eHeight,eRadius,eTop,eBase,eShape,eGraphicGeneric,eGraphicF,eGraphic},
vectorLengthE=CONST["Endite Vector Length"];
eHeight = CONST["Endite Height"];
eRadius = CONST["Endite Radius"];
eTop = {0,0,eHeight/2};
eBase = {0,0,-eHeight/2};
eShape = Cylinder[{eBase,eTop},eRadius];
eGraphicGeneric = {eShape,
{{AbsoluteThickness[1],RGBColor[1,0,0],Line[{{0,0,0},{vectorLengthE,0,0}}]},{AbsoluteThickness[1],RGBColor[0,1,0],Line[{{0,0,0},{0,vectorLengthE,0}}]},{AbsoluteThickness[1],RGBColor[0,0,1],Line[{{0,0,0},{0,0,vectorLengthE}}]}}};
eGraphicF = {eGraphicGeneric,
{Text[Subscript[\!\(\*OverscriptBox[\(b\), \(^\)]\), 1],{vectorLengthE,0,0},{0,1}],Text[Subscript[\!\(\*OverscriptBox[\(b\), \(^\)]\), 2],{0,vectorLengthE,0},{0,1}],
Text[Subscript[\!\(\*OverscriptBox[\(b\), \(^\)]\), 3],{0,0,vectorLengthE},{0,1}]}};

eGraphic={{RGBColor[1, 0.75, 0.75],eShape},
{RGBColor[1, 0.75, 1],eShape},
{RGBColor[0.75, 0.75, 1],eShape},
{RGBColor[0.75, 1, 1],eShape},
{RGBColor[0.75, 1, 0.75],eShape},
{RGBColor[1, 1, 0.75],eShape}};

{eGraphicF,eGraphic}
];


GetCoxaGraphic[CONST_Association]:=Module[{cLength,cHeight,cDepth,vectorLengthC,cShape,cGraphicGeneric,cGraphicF,cGraphic},
cLength = CONST["Coxa Length"];
cHeight = CONST["Coxa Height"]; 
cDepth = CONST["Coxa Depth"];
vectorLengthC = CONST["Coxa Vector Length"]; 
cShape = Cuboid[
{-cLength/2,-cDepth/2,-cHeight/2},
{cLength/2,cDepth/2,cHeight/2}];

cGraphicGeneric = {cShape,
{{AbsoluteThickness[1],RGBColor[1,0,0],Line[{{0,0,0},{vectorLengthC,0,0}}]},{AbsoluteThickness[1],RGBColor[0,1,0],Line[{{0,0,0},{0,vectorLengthC,0}}]},{AbsoluteThickness[1],RGBColor[0,0,1],Line[{{0,0,0},{0,0,vectorLengthC}}]}}};

cGraphicF = {cGraphicGeneric,
{Text[Subscript[\!\(\*OverscriptBox[\(c\), \(^\)]\), 1],{vectorLengthC,0,0},{0,1}],Text[Subscript[\!\(\*OverscriptBox[\(c\), \(^\)]\), 2],{0,vectorLengthC,0},{0,1}],
Text[Subscript[\!\(\*OverscriptBox[\(c\), \(^\)]\), 3],{0,0,vectorLengthC},{0,1}]}};

cGraphic={{RGBColor[1,0.75,0.75],cGraphicGeneric},
{RGBColor[1,0.75,1],cGraphicGeneric},
{RGBColor[0.75, 0.75, 1],cGraphicGeneric},
{RGBColor[0.75, 1, 1],cGraphicGeneric},
{RGBColor[0.75, 1, 0.75],cGraphicGeneric},
{RGBColor[1, 1, 0.75],cGraphicGeneric}};

{cGraphicF,cGraphic}
];


GetTrochanterGraphic[CONST_Association]:=Module[{vectorLengthTr,trHeight,trRadius,trTop,trBase,trShape,trGraphic,trGraphicF,trGraphicGeneric},
vectorLengthTr=CONST["Trochanter Vector Length"];
trHeight = CONST["Trochanter Height"];
trRadius = CONST["Trochanter Radius"];
trTop = {0,0,trHeight/2};
trBase = {0,0,-trHeight/2};
trShape = Rotate[Cylinder[{trBase,trTop},trRadius],Pi/2,{1,0,0}];

trGraphicGeneric = {trShape,
{{AbsoluteThickness[1],RGBColor[1,0,0],Line[{{0,0,0},{vectorLengthTr,0,0}}]},{AbsoluteThickness[1],RGBColor[0,1,0],Line[{{0,0,0},{0,vectorLengthTr,0}}]},{AbsoluteThickness[1],RGBColor[0,0,1],Line[{{0,0,0},{0,0,vectorLengthTr}}]}}};
trGraphicF = {trGraphicGeneric,
{Text[Subscript[\!\(\*OverscriptBox[\(d\), \(^\)]\), 1],{vectorLengthTr,0,0},{0,1}],Text[Subscript[\!\(\*OverscriptBox[\(d\), \(^\)]\), 2],{0,vectorLengthTr,0},{0,1}],
Text[Subscript[\!\(\*OverscriptBox[\(d\), \(^\)]\), 3],{0,0,vectorLengthTr},{0,1}]}};
trGraphic={{RGBColor[1, 0.5, 0.5],trShape},
{RGBColor[1, 0.5, 1],trShape},
{RGBColor[0.5, 0.5, 1],trShape},
{RGBColor[0.5, 1, 1],trShape},
{RGBColor[0.5, 1, 0.5],trShape},
{RGBColor[1, 1, 0.5],trShape}};

{trGraphicF,trGraphic}
];


GetFemurGraphic[CONST_Association]:=Module[{fLength,fHeight,fDepth,vectorLengthF,fShape,fGraphicGeneric,fGraphicF,fGraphic},
fLength = CONST["Femur Length"];
fHeight = CONST["Femur Height"]; 
fDepth = CONST["Femur Depth"];
vectorLengthF = CONST["Femur Vector Length"]; 
fShape = Cuboid[
{-fLength/2,-fDepth/2,-fHeight/2},
{fLength/2,fDepth/2,fHeight/2}];


fGraphicGeneric = {fShape,
{{AbsoluteThickness[1],RGBColor[1,0,0],Line[{{0,0,0},{vectorLengthF,0,0}}]},{AbsoluteThickness[1],RGBColor[0,1,0],Line[{{0,0,0},{0,vectorLengthF,0}}]},{AbsoluteThickness[1],RGBColor[0,0,1],Line[{{0,0,0},{0,0,vectorLengthF}}]}}};

fGraphicF = {fGraphicGeneric,
{Text[Subscript[\!\(\*OverscriptBox[\(e\), \(^\)]\), 1],{vectorLengthF,0,0},{0,1}],Text[Subscript[\!\(\*OverscriptBox[\(e\), \(^\)]\), 2],{0,vectorLengthF,0},{0,1}],
Text[Subscript[\!\(\*OverscriptBox[\(e\), \(^\)]\), 3],{0,0,vectorLengthF},{0,1}]}};

fGraphic = {{RGBColor[1,0.5,0.5],fGraphicGeneric},{RGBColor[1,0.5,1],fGraphicGeneric},{RGBColor[0.5, 0.5, 1],fGraphicGeneric},
{RGBColor[0.5, 1, 1],fGraphicGeneric},{RGBColor[0.5, 1, 0.5],fGraphicGeneric},{RGBColor[1, 1, 0.5],fGraphicGeneric}};

{fGraphicF,fGraphic}
];


GetPatellaGraphic[CONST_Association]:=Module[{vectorLengthP,pHeight,pRadius,pTop,pBase,pShape,pGraphicGeneric,pGraphicF,pGraphic},
vectorLengthP=CONST["Patella Vector Length"];
pHeight = CONST["Patella Height"];
pRadius = CONST["Patella Radius"];
pTop = {0,0,pHeight/2};
pBase = {0,0,-pHeight/2};
pShape = Rotate[Cylinder[{pBase,pTop},pRadius],Pi/2,{1,0,0}];

pGraphicGeneric= {pShape,
{{AbsoluteThickness[1],RGBColor[1,0,0],Line[{{0,0,0},{vectorLengthP,0,0}}]},{AbsoluteThickness[1],RGBColor[0,1,0],Line[{{0,0,0},{0,vectorLengthP,0}}]},{AbsoluteThickness[1],RGBColor[0,0,1],Line[{{0,0,0},{0,0,vectorLengthP}}]}}};
pGraphicF = {pGraphicGeneric,
{Text[Subscript[\!\(\*OverscriptBox[\(f\), \(^\)]\), 1],{vectorLengthP,0,0},{0,1}],Text[Subscript[\!\(\*OverscriptBox[\(f\), \(^\)]\), 2],{0,vectorLengthP,0},{0,1}],
Text[Subscript[\!\(\*OverscriptBox[\(f\), \(^\)]\), 3],{0,0,vectorLengthP},{0,1}]}};

pGraphic= {{RGBColor[1, 0.25, 0.25],pShape},
{RGBColor[0.75, 0.25, 0.75],pShape},
{RGBColor[0.25, 0.25, 1],pShape},
{RGBColor[0.25, 0.75, 0.75],pShape},
{RGBColor[0.25, 1, 0.25],pShape},
{RGBColor[0.75, 0.75, 0.25],pShape}};

{pGraphicF,pGraphic}
];


GetTarsusGraphic[CONST_Association]:=Module[{vectorLengthTa,taLength,halfTa,taWidth,taDepth,taShape,taGraphicGeneric,taGraphicF,taGraphic},
taLength = CONST["Tarsus Length"];
taWidth = CONST["Tarsus Width"]; 
taDepth = CONST["Tarsus Depth"];
vectorLengthTa = CONST["Tarsus Vector Length"]; 
halfTa = taLength/2;
taShape = {Polygon[{
{-halfTa,-taDepth/2,taWidth/2},
{halfTa,-taDepth/2,taWidth/4},
{halfTa,-taDepth/2,-taWidth/4},
{-halfTa,-taDepth/2,-taWidth/2}}],

Polygon[{
{-halfTa,taDepth/2,taWidth/2},
{halfTa,taDepth/2,taWidth/4},
{halfTa,taDepth/2,-taWidth/4},
{-halfTa,taDepth/2,-taWidth/2}}],

Polygon[{
{-halfTa,-taDepth/2,taWidth/2},
{halfTa,-taDepth/2,taWidth/4},
{halfTa,taDepth/2,taWidth/4},
{-halfTa,taDepth/2,taWidth/2}}],

Polygon[{
{-halfTa,-taDepth/2,-taWidth/2},
{halfTa,-taDepth/2,-taWidth/4},
{halfTa,taDepth/2,-taWidth/4},
{-halfTa,taDepth/2,-taWidth/2}}],

Polygon[{
{-halfTa,-taDepth/2,taWidth/2},
{-halfTa,-taDepth/2,-taWidth/2},
{-halfTa,taDepth/2,-taWidth/2},
{-halfTa,taDepth/2,taWidth/2}}],

Polygon[{
{halfTa,-taDepth/2,taWidth/4},
{halfTa,-taDepth/2,-taWidth/4},
{halfTa,taDepth/2,-taWidth/4},
{halfTa,taDepth/2,taWidth/4}}]};
taGraphicGeneric= {taShape,
{{AbsoluteThickness[1],RGBColor[1,0,0],Line[{{0,0,0},{vectorLengthTa,0,0}}]},{AbsoluteThickness[1],RGBColor[0,1,0],Line[{{0,0,0},{0,vectorLengthTa,0}}]},{AbsoluteThickness[1],RGBColor[0,0,1],Line[{{0,0,0},{0,0,vectorLengthTa}}]}}};

taGraphicF= {taGraphicGeneric,
{Text[Subscript[\!\(\*OverscriptBox[\(g\), \(^\)]\), 1],{vectorLengthTa,0,0},{0,1}],Text[Subscript[\!\(\*OverscriptBox[\(g\), \(^\)]\), 2],{0,vectorLengthTa,0},{0,1}],
Text[Subscript[\!\(\*OverscriptBox[\(g\), \(^\)]\), 3],{0,0,vectorLengthTa},{0,1}]}};

taGraphic = {{RGBColor[1, 0.25, 0.25],taGraphicGeneric},
{RGBColor[0.75, 0.25, 0.75],taGraphicGeneric},
{RGBColor[0.25, 0.25, 1],taGraphicGeneric},
{RGBColor[0.25, 0.75, 0.75],taGraphicGeneric},
{RGBColor[0.25, 1, 0.25],taGraphicGeneric},
{RGBColor[0.75, 0.75, 0.25],taGraphicGeneric}};

{taGraphicF,taGraphic}
];


LegGraphic[CONST_Association][eGraphic_,cGraphic_,trGraphic_,fGraphic_,pGraphic_,taGraphic_,index_]:=Module[{cOffset,fOffset,tOffset,lGraphic},
cOffset=CONST["Coxa Length"]/2;
fOffset = CONST["Femur Length"]/2;
tOffset = CONST["Tarsus Length"]/2;
lGraphic={Translate[eGraphic,{0,0,0}],
Translate[cGraphic,{cOffset,0,0}],
Translate[trGraphic,{2cOffset,0,0}],
Translate[fGraphic,{2cOffset+fOffset,0,0}],
Translate[pGraphic,{2cOffset+2fOffset,0,0}],
Translate[taGraphic,{2cOffset+2fOffset+tOffset,0,0}],
Text[index]}
];


(* ::Title:: *)
(*Vectors, Rotation/Position*)


(*DEFINE UNIT VECTORS AND DYADS*)
rot0[q_:1]={{1,0,0},{0,1,0},{0,0,1}};
rot1[q_]={{1,0,0},{0,Cos[q],Sin[q]},{0,-Sin[q],Cos[q]}};
rot2[q_]={{Cos[q],0,-Sin[q]},{0,1,0},{Sin[q],0,Cos[q]}};
rot3[q_]={{Cos[q],Sin[q],0},{-Sin[q],Cos[q],0},{0,0,1}};

a[x_]:=unitVector[A,a,x]

b[n__:Range[6]][x_]:=Module[{},If[Length[n]==0,unitVector[Subscript[B, n],Subscript[b, n],x],unitVector[Subscript[B, #],Subscript[b, #],x]&/@n]]
c[n__:Range[6]][x_]:=Module[{},If[Length[n]==0,unitVector[Subscript[C, n],Subscript[c, n],x],unitVector[Subscript[C, #],Subscript[c, #],x]&/@n]]
d[n__:Range[6]][x_]:=Module[{},If[Length[n]==0,unitVector[Subscript[D, n],Subscript[d, n],x],unitVector[Subscript[D, #],Subscript[d, #],x]&/@n]]
e[n__:Range[6]][x_]:=Module[{},If[Length[n]==0,unitVector[Subscript[E, n],Subscript[e, n],x],unitVector[Subscript[E, #],Subscript[e, #],x]&/@n]]
f[n__:Range[6]][x_]:=Module[{},If[Length[n]==0,unitVector[Subscript[F, n],Subscript[f, n],x],unitVector[Subscript[F, #],Subscript[f, #],x]&/@n]]
g[n__:Range[6]][x_]:=Module[{},If[Length[n]==0,unitVector[Subscript[G, n],Subscript[g, n],x],unitVector[Subscript[G, #],Subscript[g, #],x]&/@n]]
t[n__:Range[6]][x_]:=Module[{},If[Length[n]==0,unitVector[Subscript[T, n],Subscript[t, n],x],unitVector[Subscript[T, #],Subscript[t, #],x]&/@n]]
m[n__:Range[6]][x_]:=Module[{},If[Length[n]==0,unitVector[Subscript[M, n],Subscript[m, n],x],unitVector[Subscript[M, #],Subscript[m, #],x]&/@n]]

n[x_]:=unitVector[N,n,x]

aa[x_,y_]:=unitDyad[a[x],a[y]]

bb[n_,x_,y_]:=unitDyad[Subscript[b, n][x],Subscript[b, n][y]]
cc[n_,x_,y_]:=unitDyad[Subscript[c, n][x],Subscript[c, n][y]]
dd[n_,x_,y_]:=unitDyad[Subscript[d, n][x],Subscript[d, n][y]]
ee[n_,x_,y_]:=unitDyad[Subscript[e, n][x],Subscript[e, n][y]]
ff[n_,x_,y_]:=unitDyad[Subscript[f, n][x],Subscript[f, n][y]]
gg[n_,x_,y_]:=unitDyad[Subscript[g, n][x],Subscript[g, n][y]]
tt[n_,x_,y_]:=unitDyad[Subscript[t, n][x],Subscript[t, n][y]]
mm[n_,x_,y_]:=unitDyad[Subscript[m, n][x],Subscript[m, n][y]]



LegRotationDefB[CONST_Association][rA_][i_]:=Module[{qInd,rB,BtN,rotOffset},
qInd=1+3*i;
rotOffset=CONST["Leg Rotational Offsets"][[i]];
rB=rot3[Subscript[q, qInd][t]+rotOffset].rA;
BtN=rB.{n[1],n[2],n[3]};
{rB,BtN}
];

TransformBtoN[ind_,x_,BtoN_]:=x//.{b[ind][1]->BtoN[[ind]][[1]],b[ind][2]->BtoN[[ind]][[2]],b[ind][3]->BtoN[[ind]][[3]]};



LegRotationDefC[i_,rB_]:=Module[{rC,CtN},
rC=rot0[].rB;
CtN=rC.{n[1],n[2],n[3]};
Return[{rC,CtN}]
];

TransformCtoN[ind_,x_,CtoN_]:=x//.{c[ind][1]->CtoN[[ind]][[1]],c[ind][2]->CtoN[[ind]][[2]],c[ind][3]->CtoN[[ind]][[3]]};


LegRotationDefD[i_,rC_]:=Module[{qInd,rD,DtN},
qInd=2+3*i;
rD=rot2[Subscript[q, qInd][t]].rC;
DtN=rD.{n[1],n[2],n[3]};
Return[{rD,DtN}]
];

TransformDtoN[ind_,x_,DtoN_]:=x//.{d[ind][1]->DtoN[[ind]][[1]],d[ind][2]->DtoN[[ind]][[2]],d[ind][3]->DtoN[[ind]][[3]]};


LegRotationDefE[i_,rD_]:=Module[{rE,EtN},
rE=rot0[].rD;
EtN=rE.{n[1],n[2],n[3]};
Return[{rE,EtN}]
];

TransformEtoN[ind_,x_,EtoN_]:=x//.{e[ind][1]->EtoN[[ind]][[1]],e[ind][2]->EtoN[[ind]][[2]],e[ind][3]->EtoN[[ind]][[3]]};


LegRotationDefF[i_,rE_]:=Module[{qInd,rF,FtN},
qInd=3+3*i;
rF=rot2[Subscript[q, qInd][t]].rE;
FtN=rF.{n[1],n[2],n[3]};
Return[{rF,FtN}]
];

TransformFtoN[ind_,x_,FtoN_]:=x//.{f[ind][1]->FtoN[[ind]][[1]],f[ind][2]->FtoN[[ind]][[2]],f[ind][3]->FtoN[[ind]][[3]]};


LegRotationDefG[i_,rF_]:=Module[{rG,GtN},
rG=rot0[].rF;
GtN=rG.{n[1],n[2],n[3]};
Return[{rG,GtN}]
];

TransformGtoN[ind_,x_,GtoN_]:=x//.{g[ind][1]->GtoN[[ind]][[1]],g[ind][2]->GtoN[[ind]][[2]],g[ind][3]->GtoN[[ind]][[3]]};


LegRotationDefT[i_,rG_]:=Module[{rT,TtN},
rT=rot0[].rG;
TtN=rT.{n[1],n[2],n[3]};
Return[{rT,TtN}]
];

TransformTtoN[ind_,x_,TtoN_]:=x//.{t[ind][1]->TtoN[[ind]][[1]],t[ind][2]->TtoN[[ind]][[2]],t[ind][3]->TtoN[[ind]][[3]]};


LegPosVectors[CONST_Association][OrA_,ArB_][i_]:=Module[{cOffset,fOffset,tOffset,BrC,CrD,DrE,ErF,FrG,GrT,xyzo},
cOffset=CONST["Coxa Length"]/2;
fOffset=CONST["Femur Length"]/2;
tOffset=CONST["Tarsus Length"]/2;

xyzo=Flatten[Delete[Reap[
(*From Body*)
Sow[((OrA+ArB[[i]]).n[1]//TranAtoN//TranBtoN[i]),b];
Sow[((OrA+ArB[[i]]).n[2]//TranAtoN//TranBtoN[i]),b];
Sow[((OrA+ArB[[i]]).n[3]//TranAtoN//TranBtoN[i]),b];
(*Coxa*)
BrC=(cOffset)b[i][1];
Sow[((OrA+ArB[[i]]+BrC).n[1]//TranAtoN//TranBtoN[i]//TranCtoN[i]),c];
Sow[((OrA+ArB[[i]]+BrC).n[2]//TranAtoN//TranBtoN[i]//TranCtoN[i]),c];
Sow[((OrA+ArB[[i]]+BrC).n[3]//TranAtoN//TranBtoN[i]//TranCtoN[i]),c];
(*Trochanter*)
CrD=(cOffset)c[i][1];
Sow[((OrA+ArB[[i]]+BrC+CrD).n[1]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]),d];
Sow[((OrA+ArB[[i]]+BrC+CrD).n[2]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]),d];
Sow[((OrA+ArB[[i]]+BrC+CrD).n[3]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]),d];
(*Femur*)
DrE=(fOffset)d[i][1];
Sow[((OrA+ArB[[i]]+BrC+CrD+DrE).n[1]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//distributeScalars),e];
Sow[((OrA+ArB[[i]]+BrC+CrD+DrE).n[2]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//distributeScalars),e];
Sow[((OrA+ArB[[i]]+BrC+CrD+DrE).n[3]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//distributeScalars),e];
(*Patella*)
ErF=(fOffset)e[i][1];
Sow[((OrA+ArB[[i]]+BrC+CrD+DrE+ErF).n[1]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//distributeScalars),f];
Sow[((OrA+ArB[[i]]+BrC+CrD+DrE+ErF).n[2]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//distributeScalars),f];
Sow[((OrA+ArB[[i]]+BrC+CrD+DrE+ErF).n[3]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//distributeScalars),f];
(*Tarsus*)
FrG=(tOffset)f[i][1];
Sow[((OrA+ArB[[i]]+BrC+CrD+DrE+ErF+FrG).n[1]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//TranGtoN[i]//distributeScalars),g];
Sow[((OrA+ArB[[i]]+BrC+CrD+DrE+ErF+FrG).n[2]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//TranGtoN[i]//distributeScalars),g];
Sow[((OrA+ArB[[i]]+BrC+CrD+DrE+ErF+FrG).n[3]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//TranGtoN[i]//distributeScalars),g];
(*Tarsal Tip*)
GrT=(tOffset)g[i][1];
Sow[((OrA+ArB[[i]]+BrC+CrD+DrE+ErF+FrG+GrT).n[1]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//TranGtoN[i]//TranTtoN[i]//distributeScalars),t];
Sow[((OrA+ArB[[i]]+BrC+CrD+DrE+ErF+FrG+GrT).n[2]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//TranGtoN[i]//TranTtoN[i]//distributeScalars),t];
Sow[((OrA+ArB[[i]]+BrC+CrD+DrE+ErF+FrG+GrT).n[3]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//TranGtoN[i]//TranTtoN[i]//distributeScalars),t];

Sow[((OrA+ArB[[i]]).n[1]//TranAtoN//TranBtoN[i]//distributeScalars),m];
Sow[((OrA+ArB[[i]]).n[2]//TranAtoN//TranBtoN[i]//distributeScalars),m];
Sow[(0.n[3]),m];
],1],1];

{xyzo,BrC,CrD,DrE,ErF,FrG,GrT}
]


LegPosVectorsBAK[CONST_Association][OrA_,ArB_][i_]:=Module[{cOffset,fOffset,tOffset,x,y,z,BrC,CrD,DrE,ErF,FrG,GrT,xyzo},
cOffset=CONST["Coxa Length"]/2;
fOffset=CONST["Femur Length"]/2;
tOffset=CONST["Tarsus Length"]/2;
\[AliasDelimiter]
(*From Body*)
x[1]:=(OrA+ArB).n[1]//TranAtoN//TranBtoN[i];
y[1]:=(OrA+ArB).n[2]//TranAtoN//TranBtoN[i];
z[1]:=(OrA+ArB).n[3]//TranAtoN//TranBtoN[i];
Print[x[1]]
(*Coxa*)
BrC=(cOffset)Subscript[b, i][1];
x[2]=(OrA+ArB+BrC).n[1]//TranAtoN//TranBtoN[i]//TranCtoN[i];
y[2]=(OrA+ArB+BrC).n[2]//TranAtoN//TranBtoN[i]//TranCtoN[i];
z[2]=(OrA+ArB+BrC).n[3]//TranAtoN//TranBtoN[i]//TranCtoN[i];
(*Trochanter*)
CrD=(cOffset)Subscript[c, i][1];
x[3]=(OrA+ArB+BrC+CrD).n[1]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i];
y[3]=(OrA+ArB+BrC+CrD).n[2]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i];
z[3]=(OrA+ArB+BrC+CrD).n[3]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i];
(*Femur*)
DrE=(fOffset)Subscript[d, i][1];
x[4]=(OrA+ArB+BrC+CrD+DrE).n[1]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//distributeScalars;
y[4]=(OrA+ArB+BrC+CrD+DrE).n[2]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//distributeScalars;
z[4]=(OrA+ArB+BrC+CrD+DrE).n[3]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//distributeScalars;
(*Patella*)
ErF=(fOffset)Subscript[e, i][1];
x[5]=(OrA+ArB+BrC+CrD+DrE+ErF).n[1]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//distributeScalars;
y[5]=(OrA+ArB+BrC+CrD+DrE+ErF).n[2]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//distributeScalars;
z[5]=(OrA+ArB+BrC+CrD+DrE+ErF).n[3]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//distributeScalars;
(*Tarsus*)
FrG=(tOffset)Subscript[f, i][1];
x[6]=(OrA+ArB+BrC+CrD+DrE+ErF+FrG).n[1]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//TranGtoN[i]//distributeScalars;
y[6]=(OrA+ArB+BrC+CrD+DrE+ErF+FrG).n[2]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//TranGtoN[i]//distributeScalars;
z[6]=(OrA+ArB+BrC+CrD+DrE+ErF+FrG).n[3]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//TranGtoN[i]//distributeScalars;
(*Tarsal Tip*)
GrT=(tOffset)Subscript[g, i][1];
xyzo = {x,y,z};
{xyzo,BrC,CrD,DrE,ErF,FrG,GrT}
]


VecLoopRotationMBAK[n_][i_,rB_]:=Module[{rM,MtN,rNorm,rTheta,rThetaSign},
rNorm=Sqrt[(rB[[1]].n[1])^2+(rB[[1]].n[2])^2];
rTheta=ArcTan[rB[[1]].n[1]*rB[[2]].n[1]+rB[[1]].n[2]*rB[[2]].n[2],rB[[1]].n[1]*rB[[2]].n[2]-rB[[2]].n[1]*rB[[1]].n[2]];
rThetaSign=rTheta/Abs[rTheta];
rM={((rB[[1]].n[1])/rNorm)n[1]+((rB[[1]].n[2])/rNorm)n[2],((-rB[[1]].n[2])/rNorm)n[1]+((rB[[1]].n[1])/rNorm)n[2],n[3]}//gatherDots2//gatherDivisors;
MtN=rM.{n[1],n[2],n[3]};
Return[{rM,MtN}]
];

TransformMtoNBAK[ind_,x_,MtoN_]:=x//.{Subscript[m, ind][1]->MtoN[[ind]][[1]],Subscript[m, ind][2]->MtoN[[ind]][[2]],Subscript[m, ind][3]->MtoN[[ind]][[3]]};


VecLoopRotationM[i_,rB_]:=Module[{qmInd,rM,MtN},
qmInd=2i-1;
rM=rot2[Subscript[mq,qmInd+1][t]].rot1[Subscript[mq,qmInd][t]].rB;
MtN=rM.{n[1],n[2],n[3]};
Return[{rM,MtN}]
];

TransformMtoN[ind_,x_,MtoN_]:=x//.{m[ind][1]->MtoN[[ind]][[1]],m[ind][2]->MtoN[[ind]][[2]],m[ind][3]->MtoN[[ind]][[3]]};



TransformBtoM[ind_,x_,BtoM_]:=x//.{b[ind][1]->BtoM[[ind]][[1]],b[ind][2]->BtoM[[ind]][[2]],b[ind][3]->BtoM[[ind]][[3]]};
TransformCtoM[ind_,x_,CtoM_]:=x//.{c[ind][1]->CtoM[[ind]][[1]],c[ind][2]->CtoM[[ind]][[2]],c[ind][3]->CtoM[[ind]][[3]]};
TransformDtoM[ind_,x_,DtoM_]:=x//.{d[ind][1]->DtoM[[ind]][[1]],d[ind][2]->DtoM[[ind]][[2]],d[ind][3]->DtoM[[ind]][[3]]};
TransformEtoM[ind_,x_,EtoM_]:=x//.{e[ind][1]->EtoM[[ind]][[1]],e[ind][2]->EtoM[[ind]][[2]],e[ind][3]->EtoM[[ind]][[3]]};
TransformFtoM[ind_,x_,FtoM_]:=x//.{f[ind][1]->FtoM[[ind]][[1]],f[ind][2]->FtoM[[ind]][[2]],f[ind][3]->FtoM[[ind]][[3]]};
TransformGtoM[ind_,x_,GtoM_]:=x//.{g[ind][1]->GtoM[[ind]][[1]],g[ind][2]->GtoM[[ind]][[2]],g[ind][3]->GtoM[[ind]][[3]]};
TransformTtoM[ind_,x_,TtoM_]:=x//.{t[ind][1]->TtoM[[ind]][[1]],t[ind][2]->TtoM[[ind]][[2]],t[ind][3]->TtoM[[ind]][[3]]};


LegPosVectorsM[CONST_Association][i_,zB_]:=Module[{cOffset,fOffset,tOffset,MrB,BrC,CrD,DrE,ErF,FrG,GrT,TrM,xyzoM},
cOffset=CONST["Coxa Length"]/2;
fOffset=CONST["Femur Length"]/2;
tOffset=CONST["Tarsus Length"]/2;

xyzoM=Flatten[Delete[Reap[
(*From Body*)
MrB=(zB)m[i][1];
Sow[((MrB).m[i][1]//TranBtoM[i]),b];
Sow[((MrB).m[i][2]//TranBtoM[i]),b];
Sow[((MrB).m[i][3]//TranBtoM[i]),b];
(*Coxa*)
BrC=(cOffset)b[i][1];
Sow[((BrC).m[i][1]//TranBtoM[i]//TranCtoM[i]),c];
Sow[((BrC).m[i][2]//TranBtoM[i]//TranCtoM[i]),c];
Sow[((BrC).m[i][3]//TranBtoM[i]//TranCtoM[i]),c];
(*Trochanter*)
CrD=(cOffset)c[i][1];
Sow[((BrC+CrD).m[i][1]//TranBtoM[i]//TranCtoM[i]//TranDtoM[i]),d];
Sow[((BrC+CrD).m[i][2]//TranBtoM[i]//TranCtoM[i]//TranDtoM[i]),d];
Sow[((BrC+CrD).m[i][3]//TranBtoM[i]//TranCtoM[i]//TranDtoM[i]),d];
(*Femur*)
DrE=(fOffset)d[i][1];
Sow[((BrC+CrD+DrE).m[i][1]//TranBtoM[i]//TranCtoM[i]//TranDtoM[i]//TranEtoM[i]//distributeScalars),e];
Sow[((BrC+CrD+DrE).m[i][2]//TranBtoM[i]//TranCtoM[i]//TranDtoM[i]//TranEtoM[i]//distributeScalars),e];
Sow[((BrC+CrD+DrE).m[i][3]//TranBtoM[i]//TranCtoM[i]//TranDtoM[i]//TranEtoM[i]//distributeScalars),e];
(*Patella*)
ErF=(fOffset)e[i][1];
Sow[((BrC+CrD+DrE+ErF).m[i][1]//TranBtoM[i]//TranCtoM[i]//TranDtoM[i]//TranEtoM[i]//TranFtoM[i]//distributeScalars),f];
Sow[((BrC+CrD+DrE+ErF).m[i][2]//TranBtoM[i]//TranCtoM[i]//TranDtoM[i]//TranEtoM[i]//TranFtoM[i]//distributeScalars),f];
Sow[((BrC+CrD+DrE+ErF).m[i][3]//TranBtoM[i]//TranCtoM[i]//TranDtoM[i]//TranEtoM[i]//TranFtoM[i]//distributeScalars),f];
(*Tarsus*)
FrG=(tOffset)f[i][1];
Sow[((BrC+CrD+DrE+ErF+FrG).m[i][1]//TranBtoM[i]//TranCtoM[i]//TranDtoM[i]//TranEtoM[i]//TranFtoM[i]//TranGtoM[i]//distributeScalars),g];
Sow[((BrC+CrD+DrE+ErF+FrG).m[i][2]//TranBtoM[i]//TranCtoM[i]//TranDtoM[i]//TranEtoM[i]//TranFtoM[i]//TranGtoM[i]//distributeScalars),g];
Sow[((BrC+CrD+DrE+ErF+FrG).m[i][3]//TranBtoM[i]//TranCtoM[i]//TranDtoM[i]//TranEtoM[i]//TranFtoM[i]//TranGtoM[i]//distributeScalars),g];
(*Tarsal Tip*)
GrT=(tOffset)g[i][1];
Sow[((BrC+CrD+DrE+ErF+FrG+GrT).m[i][1]//TranBtoM[i]//TranCtoM[i]//TranDtoM[i]//TranEtoM[i]//TranFtoM[i]//TranGtoM[i]//TranTtoM[i]//distributeScalars),t];
Sow[((BrC+CrD+DrE+ErF+FrG+GrT).m[i][2]//TranBtoM[i]//TranCtoM[i]//TranDtoM[i]//TranEtoM[i]//TranFtoM[i]//TranGtoM[i]//TranTtoM[i]//distributeScalars),t];
Sow[((BrC+CrD+DrE+ErF+FrG+GrT).m[i][3]//TranBtoM[i]//TranCtoM[i]//TranDtoM[i]//TranEtoM[i]//TranFtoM[i]//TranGtoM[i]//TranTtoM[i]//distributeScalars),t];

TrM=-2(cOffset+fOffset+tOffset)m[i][1]-(zB)m[i][3];
],1],1];
(*If[i\[Equal]1,Print[xyzo]];*)
{xyzoM,MrB,BrC,CrD,DrE,ErF,FrG,GrT,TrM}
]


(* ::Title:: *)
(*Inverse Kinematics*)


LegPositionOrientInit[OrA_,ArB_][i_]:=Module[{OrT,OrB},
OrT[i]=OrAo+AorBo[i]+BorCo[i]+CorDo[i]+DorEo[i]+EorFo[i]+ForGo[i]+GorTo[i]//.{x[t]->Subscript[X, body],y[t]->Subscript[Y, body],z[t]->Subscript[Z, body]};
OrB[i]=OrAo+AorBo[i]//.{x[t]->Subscript[X, body],y[t]->Subscript[Y, body],z[t]->Subscript[Z, body]};
Subscript[X, Tact][i]=((OrT[i].n[1]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//TranGtoN[i])//distributeScalars)//.{Subscript[q, n_][t]->Subscript[Q, n]};
Subscript[Y, Tact][i]=((OrT[i].n[2]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//TranGtoN[i])//distributeScalars)//.{Subscript[q, n_][t]->Subscript[Q, n]};
Subscript[Z, Tact][i]=((OrT[i].n[3]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//TranGtoN[i])//distributeScalars)//.{Subscript[q, n_][t]->Subscript[Q, n]};
Subscript[X, Bact][i]=((OrB[i].n[1]//TranAtoN//TranBtoN[i])//distributeScalars)//.{Subscript[q, n_][t]->Subscript[Q, n]};
Subscript[Y, Bact][i]=((OrB[i].n[2]//TranAtoN//TranBtoN[i])//distributeScalars)//.{Subscript[q, n_][t]->Subscript[Q, n]};
Subscript[Z, Bact][i]=((OrB[i].n[3]//TranAtoN//TranBtoN[i])//distributeScalars)//.{Subscript[q, n_][t]->Subscript[Q, n]};
Subscript[X, Tcur][i]=Subscript[X, Tact][i]//.{Subscript[Q, n_]->n*0,Subscript[X, body]->0};
Subscript[Y, Tcur][i]=Subscript[Y, Tact][i]//.{Subscript[Q, n_]->n*0,Subscript[Y, body]->0};
Subscript[Z, Tcur][i]=Subscript[Z, Tact][i]//.{Subscript[Q, n_]->n*0,Subscript[Z, body]->0};
];


GetBodyKinEquations[]:={
Subscript[X, body]-Subscript[X, des],
Subscript[Y, body]-Subscript[Y, des],
Subscript[Z, body]-Subscript[Z, des],

Subscript[C, act][[1]][[1]]- Subscript[C, des][Subscript[\[Theta], r],Subscript[\[Theta], p],Subscript[\[Theta], y]][[1]][[1]],
Subscript[C, act][[1]][[2]]- Subscript[C, des][Subscript[\[Theta], r],Subscript[\[Theta], p],Subscript[\[Theta], y]][[1]][[2]],
Subscript[C, act][[1]][[3]]- Subscript[C, des][Subscript[\[Theta], r],Subscript[\[Theta], p],Subscript[\[Theta], y]][[1]][[3]],

Subscript[C, act][[2]][[1]]- Subscript[C, des][Subscript[\[Theta], r],Subscript[\[Theta], p],Subscript[\[Theta], y]][[2]][[1]],
Subscript[C, act][[2]][[2]]- Subscript[C, des][Subscript[\[Theta], r],Subscript[\[Theta], p],Subscript[\[Theta], y]][[2]][[2]],
Subscript[C, act][[2]][[3]]- Subscript[C, des][Subscript[\[Theta], r],Subscript[\[Theta], p],Subscript[\[Theta], y]][[2]][[3]],

Subscript[C, act][[3]][[1]]- Subscript[C, des][Subscript[\[Theta], r],Subscript[\[Theta], p],Subscript[\[Theta], y]][[3]][[1]],
Subscript[C, act][[3]][[2]]- Subscript[C, des][Subscript[\[Theta], r],Subscript[\[Theta], p],Subscript[\[Theta], y]][[3]][[2]],
Subscript[C, act][[3]][[3]]- Subscript[C, des][Subscript[\[Theta], r],Subscript[\[Theta], p],Subscript[\[Theta], y]][[3]][[3]]};



GetLegKinEquations[i_,OrT_]:=Module[{eqXT,eqYT,eqZT,eqXC,eqYC,eqZC,eqXB,eqYB,eqZB,eqXOrT,eqYOrT,footPlacer,shoulderRadius,bodyRadius},
eqXT=Subscript[X, Tact]-Subscript[X, Tdes];
eqYT=Subscript[Y, Tact]-Subscript[Y, Tdes];
eqZT=Subscript[Z, Tact]-Subscript[Z, Tdes];
eqXC=Subscript[X, Tcur]-Subscript[X, Tdes];
eqYC=Subscript[Y, Tcur]-Subscript[Y, Tdes];
eqZC=Subscript[Z, Tcur]-Subscript[Z, Tdes];
eqXB=Subscript[X, Bact]-Subscript[X, Tdes];
eqYB=Subscript[Y, Bact]-Subscript[Y, Tdes];
eqZB=Subscript[Z, Bact]-Subscript[Z, Tdes];
eqXOrT=((OrT[i].a[1]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//TranGtoN[i])//distributeScalars)//.{Subscript[q, n_][t]->Subscript[Q, n]};
eqYOrT=((OrT[i].a[2]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//TranGtoN[i])//distributeScalars)//.{Subscript[q, n_][t]->Subscript[Q, n]};
footPlacer=(eqXT)^2+(eqYT)^2+(eqZT)^2+(eqXC)^2+(eqYC)^2+(eqZC)^2;
shoulderRadius=(eqXB)^2+(eqYB)^2 (eqZB)^2;
bodyRadius=(eqXOrT)^2+(eqYOrT)^2;
{{eqXT,eqYT,eqZT},{eqXC,eqYC,eqZC},{eqXB,eqYB,eqZB},{eqXOrT,eqYOrT},footPlacer,shoulderRadius,bodyRadius}
];



(* ::Title:: *)
(*Misc*)
