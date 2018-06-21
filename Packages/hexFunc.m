(* ::Package:: *)

(* ::Title:: *)
(*Initialize*)


Print["Package loaded successfully."]


(* ::Title:: *)
(*Graphic/Animation Functions*)


GetBodyGraphic[]:=Module[{},
unitLength = 1;
bodyLength  = 1.5unitLength;
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
];


GetEnditeGraphic[]:=Module[{},
vectorLengthEndite = 1 unitLength;
enditeHeight = 1.25unitLength;
enditeRadius = unitLength/1.5;
enditeTop = {0,0,enditeHeight/2};
enditeBase = {0,0,-enditeHeight/2};
enditeShape = Cylinder[{enditeBase,enditeTop},enditeRadius];
enditeGraphicGeneric = {enditeShape,
{{AbsoluteThickness[1],RGBColor[1,0,0],Line[{{0,0,0},{vectorLengthEndite,0,0}}]},{AbsoluteThickness[1],RGBColor[0,1,0],Line[{{0,0,0},{0,vectorLengthEndite,0}}]},{AbsoluteThickness[1],RGBColor[0,0,1],Line[{{0,0,0},{0,0,vectorLengthEndite}}]}}};
enditeGraphicF = {enditeGraphicGeneric,
{Text[Subscript[\!\(\*OverscriptBox[\(b\), \(^\)]\), 1],{vectorLengthEndite,0,0},{0,1}],Text[Subscript[\!\(\*OverscriptBox[\(b\), \(^\)]\), 2],{0,vectorLengthEndite,0},{0,1}],
Text[Subscript[\!\(\*OverscriptBox[\(b\), \(^\)]\), 3],{0,0,vectorLengthEndite},{0,1}]}};

enditeGraphic={{RGBColor[1, 0.75, 0.75],enditeShape},
{RGBColor[1, 0.75, 1],enditeShape},
{RGBColor[0.75, 0.75, 1],enditeShape},
{RGBColor[0.75, 1, 1],enditeShape},
{RGBColor[0.75, 1, 0.75],enditeShape},
{RGBColor[1, 1, 0.75],enditeShape}};
];


GetCoxaGraphic[]:=Module[{},
coxaLength = 2unitLength; 
coxaHeight = 1 unitLength; 
coxaDepth = 1 unitLength;
vectorLengthCoxa = 1.5 unitLength; 
coxaShape = Cuboid[
{-coxaLength/2,-coxaDepth/2,-coxaHeight/2},
{coxaLength/2,coxaDepth/2,coxaHeight/2}];

coxaGraphicGeneric = {coxaShape,
{{AbsoluteThickness[1],RGBColor[1,0,0],Line[{{0,0,0},{vectorLengthCoxa,0,0}}]},{AbsoluteThickness[1],RGBColor[0,1,0],Line[{{0,0,0},{0,vectorLengthCoxa,0}}]},{AbsoluteThickness[1],RGBColor[0,0,1],Line[{{0,0,0},{0,0,vectorLengthCoxa}}]}}};

coxaGraphicF = {coxaGraphicGeneric,
{Text[Subscript[\!\(\*OverscriptBox[\(c\), \(^\)]\), 1],{vectorLengthCoxa,0,0},{0,1}],Text[Subscript[\!\(\*OverscriptBox[\(c\), \(^\)]\), 2],{0,vectorLengthCoxa,0},{0,1}],
Text[Subscript[\!\(\*OverscriptBox[\(c\), \(^\)]\), 3],{0,0,vectorLengthCoxa},{0,1}]}};

coxaGraphic={{RGBColor[1,0.75,0.75],coxaGraphicGeneric},
{RGBColor[1,0.75,1],coxaGraphicGeneric},
{RGBColor[0.75, 0.75, 1],coxaGraphicGeneric},
{RGBColor[0.75, 1, 1],coxaGraphicGeneric},
{RGBColor[0.75, 1, 0.75],coxaGraphicGeneric},
{RGBColor[1, 1, 0.75],coxaGraphicGeneric}};
];


GetTrochanterGraphic[]:=Module[{},
vectorLengthTrochanter = 1 unitLength;
trochanterHeight = 1.25unitLength;
trochanterRadius = unitLength/1.5;
trochanterTop = {0,0,trochanterHeight/2};
trochanterBase = {0,0,-trochanterHeight/2};
trochanterShape = Rotate[Cylinder[{trochanterBase,trochanterTop},trochanterRadius],Pi/2,{1,0,0}];

trochanterGraphicGeneric = {trochanterShape,
{{AbsoluteThickness[1],RGBColor[1,0,0],Line[{{0,0,0},{vectorLengthTrochanter,0,0}}]},{AbsoluteThickness[1],RGBColor[0,1,0],Line[{{0,0,0},{0,vectorLengthTrochanter,0}}]},{AbsoluteThickness[1],RGBColor[0,0,1],Line[{{0,0,0},{0,0,vectorLengthTrochanter}}]}}};
trochanterGraphicF = {trochanterGraphicGeneric,
{Text[Subscript[\!\(\*OverscriptBox[\(d\), \(^\)]\), 1],{vectorLengthTrochanter,0,0},{0,1}],Text[Subscript[\!\(\*OverscriptBox[\(d\), \(^\)]\), 2],{0,vectorLengthTrochanter,0},{0,1}],
Text[Subscript[\!\(\*OverscriptBox[\(d\), \(^\)]\), 3],{0,0,vectorLengthTrochanter},{0,1}]}};
trochanterGraphic={{RGBColor[1, 0.5, 0.5],trochanterShape},
{RGBColor[1, 0.5, 1],trochanterShape},
{RGBColor[0.5, 0.5, 1],trochanterShape},
{RGBColor[0.5, 1, 1],trochanterShape},
{RGBColor[0.5, 1, 0.5],trochanterShape},
{RGBColor[1, 1, 0.5],trochanterShape}};
];


GetFemurGraphic[]:=Module[{},
femurLength = 3 unitLength; 
femurHeight = 1 unitLength; 
femurDepth = 1 unitLength;
vectorLengthFemur = 2 unitLength; 
femurShape = Cuboid[
{-femurLength/2,-femurDepth/2,-femurHeight/2},
{femurLength/2,femurDepth/2,femurHeight/2}];


femurGraphicGeneric = {femurShape,
{{AbsoluteThickness[1],RGBColor[1,0,0],Line[{{0,0,0},{vectorLengthFemur,0,0}}]},{AbsoluteThickness[1],RGBColor[0,1,0],Line[{{0,0,0},{0,vectorLengthFemur,0}}]},{AbsoluteThickness[1],RGBColor[0,0,1],Line[{{0,0,0},{0,0,vectorLengthFemur}}]}}};

femurGraphicF = {femurGraphicGeneric,
{Text[Subscript[\!\(\*OverscriptBox[\(e\), \(^\)]\), 1],{vectorLengthFemur,0,0},{0,1}],Text[Subscript[\!\(\*OverscriptBox[\(e\), \(^\)]\), 2],{0,vectorLengthFemur,0},{0,1}],
Text[Subscript[\!\(\*OverscriptBox[\(e\), \(^\)]\), 3],{0,0,vectorLengthFemur},{0,1}]}};

femurGraphic = {{RGBColor[1,0.5,0.5],femurGraphicGeneric},{RGBColor[1,0.5,1],femurGraphicGeneric},{RGBColor[0.5, 0.5, 1],femurGraphicGeneric},
{RGBColor[0.5, 1, 1],femurGraphicGeneric},{RGBColor[0.5, 1, 0.5],femurGraphicGeneric},{RGBColor[1, 1, 0.5],femurGraphicGeneric}};
];


GetPatellaGraphic[]:=Module[{},
vectorLengthPatella = 1 unitLength;
patellaHeight = 1.25unitLength;
patellaRadius = unitLength/1.5;
patellaTop = {0,0,patellaHeight/2};
patellaBase = {0,0,-patellaHeight/2};
patellaShape = Rotate[Cylinder[{patellaBase,patellaTop},patellaRadius],Pi/2,{1,0,0}];

patellaGraphicGeneric= {patellaShape,
{{AbsoluteThickness[1],RGBColor[1,0,0],Line[{{0,0,0},{vectorLengthPatella,0,0}}]},{AbsoluteThickness[1],RGBColor[0,1,0],Line[{{0,0,0},{0,vectorLengthPatella,0}}]},{AbsoluteThickness[1],RGBColor[0,0,1],Line[{{0,0,0},{0,0,vectorLengthPatella}}]}}};
patellaGraphicF = {patellaGraphicGeneric,
{Text[Subscript[\!\(\*OverscriptBox[\(f\), \(^\)]\), 1],{vectorLengthPatella,0,0},{0,1}],Text[Subscript[\!\(\*OverscriptBox[\(f\), \(^\)]\), 2],{0,vectorLengthPatella,0},{0,1}],
Text[Subscript[\!\(\*OverscriptBox[\(f\), \(^\)]\), 3],{0,0,vectorLengthPatella},{0,1}]}};

patellaGraphic= {{RGBColor[1, 0.25, 0.25],patellaShape},
{RGBColor[0.75, 0.25, 0.75],patellaShape},
{RGBColor[0.25, 0.25, 1],patellaShape},
{RGBColor[0.25, 0.75, 0.75],patellaShape},
{RGBColor[0.25, 1, 0.25],patellaShape},
{RGBColor[0.75, 0.75, 0.25],patellaShape}};
];


GetTarsusGraphic[]:=Module[{},
vectorLengthTarsus = 3 unitLength;
tarsusLength = 5 unitLength;
halfTarsus = tarsusLength/2;
tarsusWidth = 1unitLength;
tarsusDepth = 1unitLength;
tarsusShape = {Polygon[{
{-halfTarsus,-tarsusDepth/2,tarsusWidth/2},
{halfTarsus,-tarsusDepth/2,tarsusWidth/4},
{halfTarsus,-tarsusDepth/2,-tarsusWidth/4},
{-halfTarsus,-tarsusDepth/2,-tarsusWidth/2}}],

Polygon[{
{-halfTarsus,tarsusDepth/2,tarsusWidth/2},
{halfTarsus,tarsusDepth/2,tarsusWidth/4},
{halfTarsus,tarsusDepth/2,-tarsusWidth/4},
{-halfTarsus,tarsusDepth/2,-tarsusWidth/2}}],

Polygon[{
{-halfTarsus,-tarsusDepth/2,tarsusWidth/2},
{halfTarsus,-tarsusDepth/2,tarsusWidth/4},
{halfTarsus,tarsusDepth/2,tarsusWidth/4},
{-halfTarsus,tarsusDepth/2,tarsusWidth/2}}],

Polygon[{
{-halfTarsus,-tarsusDepth/2,-tarsusWidth/2},
{halfTarsus,-tarsusDepth/2,-tarsusWidth/4},
{halfTarsus,tarsusDepth/2,-tarsusWidth/4},
{-halfTarsus,tarsusDepth/2,-tarsusWidth/2}}],

Polygon[{
{-halfTarsus,-tarsusDepth/2,tarsusWidth/2},
{-halfTarsus,-tarsusDepth/2,-tarsusWidth/2},
{-halfTarsus,tarsusDepth/2,-tarsusWidth/2},
{-halfTarsus,tarsusDepth/2,tarsusWidth/2}}],

Polygon[{
{halfTarsus,-tarsusDepth/2,tarsusWidth/4},
{halfTarsus,-tarsusDepth/2,-tarsusWidth/4},
{halfTarsus,tarsusDepth/2,-tarsusWidth/4},
{halfTarsus,tarsusDepth/2,tarsusWidth/4}}]};
tarsusGraphicGeneric= {tarsusShape,
{{AbsoluteThickness[1],RGBColor[1,0,0],Line[{{0,0,0},{vectorLengthTarsus,0,0}}]},{AbsoluteThickness[1],RGBColor[0,1,0],Line[{{0,0,0},{0,vectorLengthTarsus,0}}]},{AbsoluteThickness[1],RGBColor[0,0,1],Line[{{0,0,0},{0,0,vectorLengthTarsus}}]}}};

tarsusGraphicF= {tarsusGraphicGeneric,
{Text[Subscript[\!\(\*OverscriptBox[\(g\), \(^\)]\), 1],{vectorLengthTarsus,0,0},{0,1}],Text[Subscript[\!\(\*OverscriptBox[\(g\), \(^\)]\), 2],{0,vectorLengthTarsus,0},{0,1}],
Text[Subscript[\!\(\*OverscriptBox[\(g\), \(^\)]\), 3],{0,0,vectorLengthTarsus},{0,1}]}};

tarsusGraphic = {{RGBColor[1, 0.25, 0.25],tarsusGraphicGeneric},
{RGBColor[0.75, 0.25, 0.75],tarsusGraphicGeneric},
{RGBColor[0.25, 0.25, 1],tarsusGraphicGeneric},
{RGBColor[0.25, 0.75, 0.75],tarsusGraphicGeneric},
{RGBColor[0.25, 1, 0.25],tarsusGraphicGeneric},
{RGBColor[0.75, 0.75, 0.25],tarsusGraphicGeneric}};
];


LegGraphic[enditeGraphic_,coxaGraphic_,trochanterGraphic_,femurGraphic_,patellaGraphic_,tarsusGraphic_,index_]:=
{
Translate[enditeGraphic,{0,0,0}],
Translate[coxaGraphic,{coxaOffset,0,0}],
Translate[trochanterGraphic,{2coxaOffset,0,0}],
Translate[femurGraphic,{2coxaOffset+femurOffset,0,0}],
Translate[patellaGraphic,{2coxaOffset+2femurOffset,0,0}],
Translate[tarsusGraphic,{2coxaOffset+2femurOffset+tarsusOffset,0,0}],
Text[index]}





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


LegPositionOrientInit[i_]:=Module[{},
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


(* ::Title:: *)
(*Misc*)


RefreshXYZQ[]:=Module[{},
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
Subscript[q, 21][t_]=.;];
