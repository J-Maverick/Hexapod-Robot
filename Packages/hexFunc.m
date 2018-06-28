(* ::Package:: *)

(* ::Title:: *)
(*Initialize*)


Print["Package loaded successfully."]


(* ::Title:: *)
(*Graphic/Animation Functions*)


GetBodyGraphic[uLength_]:=Module[{bodyLength,bodySide,vectorLengthBody,bodyHeight,halfBodyHeight,angledBodyLength,bodyShape,bodyGraphic,bodyGraphicF},
bodyLength  = 1.5uLength;
bodySide = 2bodyLength;
vectorLengthBody = 4 bodyLength;
bodyHeight= 1.5bodyLength;
halfBodyHeight=bodyHeight/2;
angledBodyLength = (1+Sqrt[3])(bodyLength);
bodyShape = {Polygon[{
{bodySide,bodyLength,-halfBodyHeight},
{bodySide,-bodyLength,-halfBodyHeight},
{bodyLength,-angledBodyLength,-halfBodyHeight},
{-bodyLength,-angledBodyLength,-halfBodyHeight},
{-bodySide,-bodyLength,-halfBodyHeight},
{-bodySide,bodyLength,-halfBodyHeight},
{-bodyLength,angledBodyLength,-halfBodyHeight},
{bodyLength,angledBodyLength,-halfBodyHeight}}],

Polygon[{
{bodySide,bodyLength,halfBodyHeight},
{bodySide,-bodyLength,halfBodyHeight},
{bodyLength,-angledBodyLength,halfBodyHeight},
{-bodyLength,-angledBodyLength,halfBodyHeight},
{-bodySide,-bodyLength,halfBodyHeight},
{-bodySide,bodyLength,halfBodyHeight},
{-bodyLength,angledBodyLength,halfBodyHeight},
{bodyLength,angledBodyLength,halfBodyHeight}}],

Polygon[
{{bodySide,bodyLength,-halfBodyHeight},
{bodySide,-bodyLength,-halfBodyHeight},
{bodySide,-bodyLength,halfBodyHeight},
{bodySide,bodyLength,halfBodyHeight}}],

Polygon[
{{-bodySide,bodyLength,-halfBodyHeight},
{-bodySide,-bodyLength,-halfBodyHeight},
{-bodySide,-bodyLength,halfBodyHeight},
{-bodySide,bodyLength,halfBodyHeight}}],

Polygon[
{{-bodyLength,angledBodyLength,-halfBodyHeight},
{bodyLength,angledBodyLength,-halfBodyHeight},
{bodyLength,angledBodyLength,halfBodyHeight},
{-bodyLength,angledBodyLength,halfBodyHeight}}],

Polygon[
{{-bodyLength,-angledBodyLength,-halfBodyHeight},
{bodyLength,-angledBodyLength,-halfBodyHeight},
{bodyLength,-angledBodyLength,halfBodyHeight},
{-bodyLength,-angledBodyLength,halfBodyHeight}}],

Polygon[
{{bodyLength,angledBodyLength,-halfBodyHeight},
{bodySide,bodyLength,-halfBodyHeight},
{bodySide,bodyLength,halfBodyHeight},
{bodyLength,angledBodyLength,halfBodyHeight}}],

Polygon[
{{bodyLength,-angledBodyLength,-halfBodyHeight},
{bodySide,-bodyLength,-halfBodyHeight},
{bodySide,-bodyLength,halfBodyHeight},
{bodyLength,-angledBodyLength,halfBodyHeight}}],

Polygon[
{{-bodyLength,-angledBodyLength,-halfBodyHeight},
{-bodySide,-bodyLength,-halfBodyHeight},
{-bodySide,-bodyLength,halfBodyHeight},
{-bodyLength,-angledBodyLength,halfBodyHeight}}],

Polygon[
{{-bodyLength,angledBodyLength,-halfBodyHeight},
{-bodySide,bodyLength,-halfBodyHeight},
{-bodySide,bodyLength,halfBodyHeight},
{-bodyLength,angledBodyLength,halfBodyHeight}}]
};

bodyGraphicF= {bodyShape,
{Text[Subscript[\!\(\*OverscriptBox[\(a\), \(^\)]\), 1],{vectorLengthBody,0,0},{0,1}],Text[Subscript[\!\(\*OverscriptBox[\(a\), \(^\)]\), 2],{0,vectorLengthBody,0},{0,1}],
Text[Subscript[\!\(\*OverscriptBox[\(a\), \(^\)]\), 3],{0,0,vectorLengthBody},{0,1}],{AbsoluteThickness[1],RGBColor[1,0,0],
Line[{{0,0,0},{vectorLengthBody,0,0}}]},{AbsoluteThickness[1],RGBColor[0,1,0],
Line[{{0,0,0},{0,vectorLengthBody,0}}]},{AbsoluteThickness[1],RGBColor[0,0,1],
Line[{{0,0,0},{0,0,vectorLengthBody}}]}}};

bodyGraphic = {bodyShape,{AbsoluteThickness[1],RGBColor[1,0,0],Line[{{0,0,0},{vectorLengthBody,0,0}}]},
{AbsoluteThickness[1],RGBColor[0,1,0],Line[{{0,0,0},{0,vectorLengthBody,0}}]},{AbsoluteThickness[1],
RGBColor[0,0,1],Line[{{0,0,0},{0,0,vectorLengthBody}}]}};

Return[{bodyGraphicF,bodyGraphic}]
];


GetEnditeGraphic[uLength_]:=Module[{vectorLengthE,eHeight,eRadius,eTop,eBase,eShape,eGraphicGeneric,eGraphicF,eGraphic},
vectorLengthE = 1 uLength;
eHeight = 1.25uLength;
eRadius = uLength/1.5;
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

Return[{eGraphicF,eGraphic}]
];


GetCoxaGraphic[uLength_,getL___]:=Module[{cLength,cHeight,cDepth,vectorLengthC,cShape,cGraphicGeneric,cGraphicF,cGraphic},
cLength = 2uLength; 
If[getL>0,Return[cLength]];
cHeight = 1 uLength; 
cDepth = 1 uLength;
vectorLengthC = 1.5 uLength; 
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

Return[{cGraphicF,cGraphic}]
];


GetTrochanterGraphic[uLength_]:=Module[{vectorLengthTr,trHeight,trRadius,trTop,trBase,trShape,trGraphic,trGraphicF,trGraphicGeneric},
vectorLengthTr = 1 uLength;
trHeight = 1.25uLength;
trRadius = uLength/1.5;
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

Return[{trGraphicF,trGraphic}]
];


GetFemurGraphic[uLength_,getL___]:=Module[{fLength,fHeight,fDepth,vectorLengthF,fShape,fGraphicGeneric,fGraphicF,fGraphic},
fLength = 3 uLength; 
If[getL>0,Return[fLength]];
fHeight = 1 uLength; 
fDepth = 1 uLength;
vectorLengthF = 2 uLength; 
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

Return[{fGraphicF,fGraphic}]
];


GetPatellaGraphic[uLength_]:=Module[{vectorLengthP,pHeight,pRadius,pTop,pBase,pShape,pGraphicGeneric,pGraphicF,pGraphic},
vectorLengthP = 1 uLength;
pHeight = 1.25uLength;
pRadius = uLength/1.5;
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

Return[{pGraphicF,pGraphic}]
];


GetTarsusGraphic[uLength_,getL___]:=Module[{vectorLengthTa,taLength,halfTa,taWidth,taDepth,taShape,taGraphicGeneric,taGraphicF,taGraphic},
vectorLengthTa = 3 uLength;
taLength = 5 uLength;
If[getL>0,Return[taLength]];
halfTa = taLength/2;
taWidth = 1uLength;
taDepth = 1uLength;
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

Return[{taGraphicF,taGraphic}]
];


LegGraphic[uL_,eGraphic_,cGraphic_,trGraphic_,fGraphic_,pGraphic_,taGraphic_,index_]:=Module[{coxaOffset,femurOffset,tarsusOffset,lGraphic},
coxaOffset=GetCoxaGraphic[uL,1]/2;
femurOffset = GetFemurGraphic[uL,1]/2;
tarsusOffset = GetTarsusGraphic[uL,1]/2;
lGraphic={Translate[eGraphic,{0,0,0}],
Translate[cGraphic,{coxaOffset,0,0}],
Translate[trGraphic,{2coxaOffset,0,0}],
Translate[fGraphic,{2coxaOffset+femurOffset,0,0}],
Translate[pGraphic,{2coxaOffset+2femurOffset,0,0}],
Translate[taGraphic,{2coxaOffset+2femurOffset+tarsusOffset,0,0}],
Text[index]};
Return[lGraphic]
];


AnimateLeg[i_]:={
(*Endite*)
Translate[GeometricTransformation[enditeGraphic[[i]],Transpose[rotB[i]]],{xBo[i],yBo[i],zBo[i]}],
(*Coxa*)
Translate[GeometricTransformation[coxaGraphic[[i]],Transpose[rotC[i]]],{xCo[i],yCo[i],zCo[i]}],
(*Trochanter*)
Translate[GeometricTransformation[trochanterGraphic[[i]],Transpose[rotD[i]]],{xDo[i],yDo[i],zDo[i]}],
(*Femur*)
Translate[GeometricTransformation[femurGraphic[[i]],Transpose[rotE[i]]],{xEo[i],yEo[i],zEo[i]}],
(*Patella*)
Translate[GeometricTransformation[patellaGraphic[[i]],Transpose[rotF[i]]],{xFo[i],yFo[i],zFo[i]}],
(*Tarsus*)
Translate[GeometricTransformation[tarsusGraphic[[i]],Transpose[rotG[i]]],{xGo[i],yGo[i],zGo[i]}]}


(* ::Title:: *)
(*Vectors, Rotation/Position*)


UnitVecDyadInit[]:=Module[{},
a[x_]:=unitVector[A,a,x];

Subscript[b, 1][x_]:=unitVector[Subscript[B, 1],Subscript[b, 1_],x];
Subscript[b, 2][x_]:=unitVector[Subscript[B, 2],Subscript[b, 2_],x];
Subscript[b, 3][x_]:=unitVector[Subscript[B, 3],Subscript[b, 3_],x];
Subscript[b, 4][x_]:=unitVector[Subscript[B, 4],Subscript[b, 4_],x];
Subscript[b, 5][x_]:=unitVector[Subscript[B, 5],Subscript[b, 5_],x];
Subscript[b, 6][x_]:=unitVector[Subscript[B, 6],Subscript[b, 6_],x];

Subscript[c, 1][x_]:=unitVector[Subscript[C, 1],Subscript[c, 1_],x];
Subscript[c, 2][x_]:=unitVector[Subscript[C, 2],Subscript[c, 2_],x];
Subscript[c, 3][x_]:=unitVector[Subscript[C, 3],Subscript[c, 3_],x];
Subscript[c, 4][x_]:=unitVector[Subscript[C, 4],Subscript[c, 4_],x];
Subscript[c, 5][x_]:=unitVector[Subscript[C, 5],Subscript[c, 5_],x];
Subscript[c, 6][x_]:=unitVector[Subscript[C, 6],Subscript[c, 6_],x];

Subscript[d, 1][x_]:=unitVector[Subscript[D, 1],Subscript[d, 1_],x];
Subscript[d, 2][x_]:=unitVector[Subscript[D, 2],Subscript[d, 2_],x];
Subscript[d, 3][x_]:=unitVector[Subscript[D, 3],Subscript[d, 3_],x];
Subscript[d, 4][x_]:=unitVector[Subscript[D, 4],Subscript[d, 4_],x];
Subscript[d, 5][x_]:=unitVector[Subscript[D, 5],Subscript[d, 5_],x];
Subscript[d, 6][x_]:=unitVector[Subscript[D, 6],Subscript[d, 6_],x];

Subscript[e, 1][x_]:=unitVector[Subscript[E, 1],Subscript[e, 1_],x];
Subscript[e, 2][x_]:=unitVector[Subscript[E, 2],Subscript[e, 2_],x];
Subscript[e, 3][x_]:=unitVector[Subscript[E, 3],Subscript[e, 3_],x];
Subscript[e, 4][x_]:=unitVector[Subscript[E, 4],Subscript[e, 4_],x];
Subscript[e, 5][x_]:=unitVector[Subscript[E, 5],Subscript[e, 5_],x];
Subscript[e, 6][x_]:=unitVector[Subscript[E, 6],Subscript[e, 6_],x];

Subscript[f, 1][x_]:=unitVector[Subscript[F, 1],Subscript[f, 1_],x];
Subscript[f, 2][x_]:=unitVector[Subscript[F, 2],Subscript[f, 2_],x];
Subscript[f, 3][x_]:=unitVector[Subscript[F, 3],Subscript[f, 3_],x];
Subscript[f, 4][x_]:=unitVector[Subscript[F, 4],Subscript[f, 4_],x];
Subscript[f, 5][x_]:=unitVector[Subscript[F, 5],Subscript[f, 5_],x];
Subscript[f, 6][x_]:=unitVector[Subscript[F, 6],Subscript[f, 6_],x];

Subscript[g, 1][x_]:=unitVector[Subscript[G, 1],Subscript[g, 1_],x];
Subscript[g, 2][x_]:=unitVector[Subscript[G, 2],Subscript[g, 2_],x];
Subscript[g, 3][x_]:=unitVector[Subscript[G, 3],Subscript[g, 3_],x];
Subscript[g, 4][x_]:=unitVector[Subscript[G, 4],Subscript[g, 4_],x];
Subscript[g, 5][x_]:=unitVector[Subscript[G, 5],Subscript[g, 5_],x];
Subscript[g, 6][x_]:=unitVector[Subscript[G, 6],Subscript[g, 6_],x];

Subscript[t, 1][x_]:=unitVector[Subscript[T, 1],Subscript[t, 1_],x];
Subscript[t, 2][x_]:=unitVector[Subscript[T, 2],Subscript[t, 2_],x];
Subscript[t, 3][x_]:=unitVector[Subscript[T, 3],Subscript[t, 3_],x];
Subscript[t, 4][x_]:=unitVector[Subscript[T, 4],Subscript[t, 4_],x];
Subscript[t, 5][x_]:=unitVector[Subscript[T, 5],Subscript[t, 5_],x];
Subscript[t, 6][x_]:=unitVector[Subscript[T, 6],Subscript[t, 6_],x];


n[x_]:=unitVector[N,n,x];

aa[x_,y_]:=unitDyad[a[x],a[y]];

Subscript[bb, 1][x_,y_]:=unitDyad[Subscript[b, 1][x],Subscript[b, 1][y]];
Subscript[bb, 2][x_,y_]:=unitDyad[Subscript[b, 2][x],Subscript[b, 2][y]];
Subscript[bb, 3][x_,y_]:=unitDyad[Subscript[b, 3][x],Subscript[b, 3][y]];
Subscript[bb, 4][x_,y_]:=unitDyad[Subscript[b, 4][x],Subscript[b, 4][y]];
Subscript[bb, 5][x_,y_]:=unitDyad[Subscript[b, 5][x],Subscript[b, 5][y]];
Subscript[bb, 6][x_,y_]:=unitDyad[Subscript[b, 6][x],Subscript[b, 6][y]];

Subscript[cc, 1][x_,y_]:=unitDyad[Subscript[c, 1][x],Subscript[c, 1][y]];
Subscript[cc, 2][x_,y_]:=unitDyad[Subscript[c, 2][x],Subscript[c, 2][y]];
Subscript[cc, 3][x_,y_]:=unitDyad[Subscript[c, 3][x],Subscript[c, 3][y]];
Subscript[cc, 4][x_,y_]:=unitDyad[Subscript[c, 4][x],Subscript[c, 4][y]];
Subscript[cc, 5][x_,y_]:=unitDyad[Subscript[c, 5][x],Subscript[c, 5][y]];
Subscript[cc, 6][x_,y_]:=unitDyad[Subscript[c, 6][x],Subscript[c, 6][y]];

Subscript[dd, 1][x_,y_]:=unitDyad[Subscript[d, 1][x],Subscript[d, 1][y]];
Subscript[dd, 2][x_,y_]:=unitDyad[Subscript[d, 2][x],Subscript[d, 2][y]];
Subscript[dd, 3][x_,y_]:=unitDyad[Subscript[d, 3][x],Subscript[d, 3][y]];
Subscript[dd, 4][x_,y_]:=unitDyad[Subscript[d, 4][x],Subscript[d, 4][y]];
Subscript[dd, 5][x_,y_]:=unitDyad[Subscript[d, 5][x],Subscript[d, 5][y]];
Subscript[dd, 6][x_,y_]:=unitDyad[Subscript[d, 6][x],Subscript[d, 6][y]];

Subscript[ee, 1][x_,y_]:=unitDyad[Subscript[e, 1][x],Subscript[e, 1][y]];
Subscript[ee, 2][x_,y_]:=unitDyad[Subscript[e, 2][x],Subscript[e, 2][y]];
Subscript[ee, 3][x_,y_]:=unitDyad[Subscript[e, 3][x],Subscript[e, 3][y]];
Subscript[ee, 4][x_,y_]:=unitDyad[Subscript[e, 4][x],Subscript[e, 4][y]];
Subscript[ee, 5][x_,y_]:=unitDyad[Subscript[e, 5][x],Subscript[e, 5][y]];
Subscript[ee, 6][x_,y_]:=unitDyad[Subscript[e, 6][x],Subscript[e, 6][y]];

Subscript[ff, 1][x_,y_]:=unitDyad[Subscript[f, 1][x],Subscript[f, 1][y]];
Subscript[ff, 2][x_,y_]:=unitDyad[Subscript[f, 2][x],Subscript[f, 2][y]];
Subscript[ff, 3][x_,y_]:=unitDyad[Subscript[f, 3][x],Subscript[f, 3][y]];
Subscript[ff, 4][x_,y_]:=unitDyad[Subscript[f, 4][x],Subscript[f, 4][y]];
Subscript[ff, 5][x_,y_]:=unitDyad[Subscript[f, 5][x],Subscript[f, 5][y]];
Subscript[ff, 6][x_,y_]:=unitDyad[Subscript[f, 6][x],Subscript[f, 6][y]];

Subscript[gg, 1][x_,y_]:=unitDyad[Subscript[g, 1][x],Subscript[g, 1][y]];
Subscript[gg, 2][x_,y_]:=unitDyad[Subscript[g, 2][x],Subscript[g, 2][y]];
Subscript[gg, 3][x_,y_]:=unitDyad[Subscript[g, 3][x],Subscript[g, 3][y]];
Subscript[gg, 4][x_,y_]:=unitDyad[Subscript[g, 4][x],Subscript[g, 4][y]];
Subscript[gg, 5][x_,y_]:=unitDyad[Subscript[g, 5][x],Subscript[g, 5][y]];
Subscript[gg, 6][x_,y_]:=unitDyad[Subscript[g, 6][x],Subscript[g, 6][y]];
];



GenericRot[]:=Module[{},
rot0[q_:1]={{1,0,0},{0,1,0},{0,0,1}};
rot1[q_]={{1,0,0},{0,Cos[q],Sin[q]},{0,-Sin[q],Cos[q]}};
rot2[q_]={{Cos[q],0,-Sin[q]},{0,1,0},{Sin[q],0,Cos[q]}};
rot3[q_]={{Cos[q],Sin[q],0},{-Sin[q],Cos[q],0},{0,0,1}};
];


BodyRotationDef[]:=Module[{},
rotA=rot3[Subscript[q, 3][t]].rot2[Subscript[q, 2][t]].rot1[Subscript[q, 1][t]];
AtoN=rotA.{n[1],n[2],n[3]};
TranAtoN[x_]:=x//.{a[1]->AtoN[[1]],a[2]->AtoN[[2]],a[3]->AtoN[[3]]};
];


LegRotationDef[i_]:=Module[{qInd1,qInd2,qInd3},
qInd1=1+3*i;
qInd2=2+3*i;
qInd3=3+3*i;
rotB[i]=rot3[Subscript[q, qInd1][t]+legRotOffset[[i]]].rot3[Subscript[q, 3][t]].rot2[Subscript[q, 2][t]].rot1[Subscript[q, 1][t]];
BtoN[i]=rotB[i].{n[1],n[2],n[3]};
TranBtoN[in_][xx_]:=TranBN[in,xx];
TranBN[ind_,x_]:=x//.{Subscript[b, ind][1]->BtoN[ind][[1]],Subscript[b, ind][2]->BtoN[ind][[2]],Subscript[b, ind][3]->BtoN[ind][[3]]};
rotC[i]=rot0[].rot3[Subscript[q, qInd1][t]+legRotOffset[[i]]].rot3[Subscript[q, 3][t]].rot2[Subscript[q, 2][t]].rot1[Subscript[q, 1][t]];
CtoN[i]=rotC[i].{n[1],n[2],n[3]};
TranCtoN[in_][xx_]:=TranCN[in,xx];
TranCN[ind_,x_]:=x//.{Subscript[c, ind][1]->CtoN[ind][[1]],Subscript[c, ind][2]->CtoN[ind][[2]],Subscript[c, ind][3]->CtoN[ind][[3]]};
rotD[i]=rot2[Subscript[q, qInd2][t]].rot0[].rot3[Subscript[q, qInd1][t]+legRotOffset[[i]]].rot3[Subscript[q, 3][t]].rot2[Subscript[q, 2][t]].rot1[Subscript[q, 1][t]];
DtoN[i]=rotD[i].{n[1],n[2],n[3]};
TranDtoN[in_][xx_]:=TranDN[in,xx];
TranDN[ind_,x_]:=x//.{Subscript[d, ind][1]->DtoN[ind][[1]],Subscript[d, ind][2]->DtoN[ind][[2]],Subscript[d, ind][3]->DtoN[ind][[3]]};
rotE[i]=rot0[].rot2[Subscript[q, qInd2][t]].rot0[].rot3[Subscript[q, qInd1][t]+legRotOffset[[i]]].rot3[Subscript[q, 3][t]].rot2[Subscript[q, 2][t]].rot1[Subscript[q, 1][t]];
EtoN[i]=rotE[i].{n[1],n[2],n[3]};
TranEtoN[in_][xx_]:=TranEN[in,xx];
TranEN[ind_,x_]:=x//.{Subscript[e, ind][1]->EtoN[ind][[1]],Subscript[e, ind][2]->EtoN[ind][[2]],Subscript[e, ind][3]->EtoN[ind][[3]]};
rotF[i]=rot2[Subscript[q, qInd3][t]].rot0[].rot2[Subscript[q, qInd2][t]].rot0[].rot3[Subscript[q, qInd1][t]+legRotOffset[[i]]].rot3[Subscript[q, 3][t]].rot2[Subscript[q, 2][t]].rot1[Subscript[q, 1][t]];
FtoN[i]=rotF[i].{n[1],n[2],n[3]};
TranFtoN[in_][xx_]:=TranFN[in,xx];
TranFN[ind_,x_]:=x//.{Subscript[f, ind][1]->FtoN[ind][[1]],Subscript[f, ind][2]->FtoN[ind][[2]],Subscript[f, ind][3]->FtoN[ind][[3]]};
rotG[i]=rot0[].rot2[Subscript[q, qInd3][t]].rot0[].rot2[Subscript[q, qInd2][t]].rot0[].rot3[Subscript[q, qInd1][t]+legRotOffset[[i]]].rot3[Subscript[q, 3][t]].rot2[Subscript[q, 2][t]].rot1[Subscript[q, 1][t]];
GtoN[i]=rotG[i].{n[1],n[2],n[3]};
TranGtoN[in_][xx_]:=TranGN[in,xx];
TranGN[ind_,x_]:=x//.{Subscript[g, ind][1]->GtoN[ind][[1]],Subscript[g, ind][2]->GtoN[ind][[2]],Subscript[g, ind][3]->GtoN[ind][[3]]};
]


BodyPosVectorDef[]:=Module[{},
OrAo = x[t]n[1]+y[t]n[2]+z[t]n[3];

AorBo={(-cornerXOffset)a[1]+(cornerYOffset)a[2],(-sideXOffset)a[1]+(sideYOffset)a[2],(-cornerXOffset)a[1]+(-cornerYOffset)a[2],
(cornerXOffset)a[1]+(cornerYOffset)a[2],(sideXOffset)a[1]+(sideYOffset)a[2],(cornerXOffset)a[1]+(-cornerYOffset)a[2]};

xAo=OrAo.n[1]//TranAtoN;
yAo=OrAo.n[2]//TranAtoN;
zAo=OrAo.n[3]//TranAtoN;
];


LegPosVectorDef[i_]:=Module[{},
(*From Body*)
xBo[i]=(OrAo+AorBo[[i]]).n[1]//TranAtoN//TranBtoN[i];
yBo[i]=(OrAo+AorBo[[i]]).n[2]//TranAtoN//TranBtoN[i];
zBo[i]=(OrAo+AorBo[[i]]).n[3]//TranAtoN//TranBtoN[i];
(*Coxa*)
BorCo[i]=(coxaOffset)Subscript[b, i][1];
xCo[i]=(OrAo+AorBo[[i]]+BorCo[i]).n[1]//TranAtoN//TranBtoN[i]//TranCtoN[i];
yCo[i]=(OrAo+AorBo[[i]]+BorCo[i]).n[2]//TranAtoN//TranBtoN[i]//TranCtoN[i];
zCo[i]=(OrAo+AorBo[[i]]+BorCo[i]).n[3]//TranAtoN//TranBtoN[i]//TranCtoN[i];
(*Trochanter*)
CorDo[i]=(coxaOffset)Subscript[c, i][1];
xDo[i]=(OrAo+AorBo[[i]]+BorCo[i]+CorDo[i]).n[1]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i];
yDo[i]=(OrAo+AorBo[[i]]+BorCo[i]+CorDo[i]).n[2]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i];
zDo[i]=(OrAo+AorBo[[i]]+BorCo[i]+CorDo[i]).n[3]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i];
(*Femur*)
DorEo[i]=(femurOffset)Subscript[d, i][1];
xEo[i]=(OrAo+AorBo[[i]]+BorCo[i]+CorDo[i]+DorEo[i]).n[1]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//distributeScalars;
yEo[i]=(OrAo+AorBo[[i]]+BorCo[i]+CorDo[i]+DorEo[i]).n[2]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//distributeScalars;
zEo[i]=(OrAo+AorBo[[i]]+BorCo[i]+CorDo[i]+DorEo[i]).n[3]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//distributeScalars;
(*Patella*)
EorFo[i]=(femurOffset)Subscript[e, i][1];
xFo[i]=(OrAo+AorBo[[i]]+BorCo[i]+CorDo[i]+DorEo[i]+EorFo[i]).n[1]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//distributeScalars;
yFo[i]=(OrAo+AorBo[[i]]+BorCo[i]+CorDo[i]+DorEo[i]+EorFo[i]).n[2]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//distributeScalars;
zFo[i]=(OrAo+AorBo[[i]]+BorCo[i]+CorDo[i]+DorEo[i]+EorFo[i]).n[3]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//distributeScalars;
(*Tarsus*)
ForGo[i]=(tarsusOffset)Subscript[f, i][1];
xGo[i]=(OrAo+AorBo[[i]]+BorCo[i]+CorDo[i]+DorEo[i]+EorFo[i]+ForGo[i]).n[1]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//TranGtoN[i]//distributeScalars;
yGo[i]=(OrAo+AorBo[[i]]+BorCo[i]+CorDo[i]+DorEo[i]+EorFo[i]+ForGo[i]).n[2]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//TranGtoN[i]//distributeScalars;
zGo[i]=(OrAo+AorBo[[i]]+BorCo[i]+CorDo[i]+DorEo[i]+EorFo[i]+ForGo[i]).n[3]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//TranGtoN[i]//distributeScalars;
(*Tarsal Tip*)
GorTo[i]=(tarsusOffset)Subscript[g, i][1];
]


(* ::Title:: *)
(*Inverse Kinematics*)


BodyPositionOrientInit[]:=Module[{},
Subscript[C, des][roll_,pitch_,yaw_]:=rot3[yaw].rot2[pitch].rot1[roll];
Subscript[C, act]=rotA//.{Subscript[q, n_][t]->Subscript[Q, n]};
Subscript[X, bodycur]=0;
Subscript[Y, bodycur]=0;
Subscript[Z, bodycur]=1;
Subscript[Q, cur]=ConstantArray[0,21];
OrA=OrAo//.{x[t]->Subscript[X, body],y[t]->Subscript[Y, body],z[t]->Subscript[Z, body]};
];


LegPositionOrientInit[i_]:=(
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
);


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


RefreshXYZQ[]:=(
x[t_]=.;
y[t_]=.;
z[t_]=.;
Subscript[q, 1][t_]=.;
Subscript[q, 2][t_]=.;
Subscript[q, 3][t_]=.;
Subscript[q, 4][t_]=.;
Subscript[q, 5][t_]=.;
Subscript[q, 6][t_]=.;
Subscript[q, 7][t_]=.;
Subscript[q, 8][t_]=.;
Subscript[q, 9][t_]=.;
Subscript[q, 10][t_]=.;
Subscript[q, 11][t_]=.;
Subscript[q, 12][t_]=.;
Subscript[q, 13][t_]=.;
Subscript[q, 14][t_]=.;
Subscript[q, 15][t_]=.;
Subscript[q, 16][t_]=.;
Subscript[q, 17][t_]=.;
Subscript[q, 18][t_]=.;
Subscript[q, 19][t_]=.;
Subscript[q, 20][t_]=.;
Subscript[q, 21][t_]=.;);
