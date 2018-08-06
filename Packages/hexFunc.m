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


AnimateLeg[i_]:={
(*Endite*)
Translate[GeometricTransformation[enditeGraphic[[i]],Transpose[rotB[[i]]]],{xBo[[i]],yBo[[i]],zBo[[i]]}],
(*Coxa*)
Translate[GeometricTransformation[coxaGraphic[[i]],Transpose[rotC[[i]]]],{xCo[[i]],yCo[[i]],zCo[[i]]}],
(*Trochanter*)
Translate[GeometricTransformation[trochanterGraphic[[i]],Transpose[rotD[[i]]]],{xDo[[i]],yDo[[i]],zDo[[i]]}],
(*Femur*)
Translate[GeometricTransformation[femurGraphic[[i]],Transpose[rotE[[i]]]],{xEo[[i]],yEo[[i]],zEo[[i]]}],
(*Patella*)
Translate[GeometricTransformation[patellaGraphic[[i]],Transpose[rotF[[i]]]],{xFo[[i]],yFo[[i]],zFo[[i]]}],
(*Tarsus*)
Translate[GeometricTransformation[tarsusGraphic[[i]],Transpose[rotG[[i]]]],{xGo[[i]],yGo[[i]],zGo[[i]]}]}


(* ::Title:: *)
(*Vectors, Rotation/Position*)


LegRotationDefB[CONST_Association][gRot_,rA_][i_]:=Module[{qInd,rB,BtN,rotOffset},
qInd=1+3*i;
rotOffset=CONST["Leg Rotational Offsets"][[i]];
rB=gRot[[3]][Subscript[q, qInd][t]+rotOffset].rA;
BtN=rB.{n[1],n[2],n[3]};
Return[{rB,BtN}];
];

TranslateBtoN[ind_,x_,BtoN_]:=x//.{Subscript[b, ind][1]->BtoN[[ind]][[1]],Subscript[b, ind][2]->BtoN[[ind]][[2]],Subscript[b, ind][3]->BtoN[[ind]][[3]]};



LegRotationDefC[gRot_][i_,rB_]:=Module[{qInd,rC,CtN},
qInd=1+3*i;
rC=gRot[[4]][].rB;
CtN=rC.{n[1],n[2],n[3]};
Return[{rC,CtN}]
];

TranslateCtoN[ind_,x_,CtoN_]:=x//.{Subscript[c, ind][1]->CtoN[[ind]][[1]],Subscript[c, ind][2]->CtoN[[ind]][[2]],Subscript[c, ind][3]->CtoN[[ind]][[3]]};


LegRotationDefD[gRot_][i_,rC_]:=Module[{qInd1,qInd2,rD,DtN},
qInd1=1+3*i;
qInd2=2+3*i;
rD=gRot[[2]][Subscript[q, qInd2][t]].rC;
DtN=rD.{n[1],n[2],n[3]};
Return[{rD,DtN}]
];

TranslateDtoN[ind_,x_,DtoN_]:=x//.{Subscript[d, ind][1]->DtoN[[ind]][[1]],Subscript[d, ind][2]->DtoN[[ind]][[2]],Subscript[d, ind][3]->DtoN[[ind]][[3]]};


LegRotationDefE[gRot_][i_,rD_]:=Module[{qInd1,qInd2,rE,EtN},
qInd1=1+3*i;
qInd2=2+3*i;
rE=gRot[[4]][].rD;
EtN=rE.{n[1],n[2],n[3]};
Return[{rE,EtN}]
];

TranslateEtoN[ind_,x_,EtoN_]:=x//.{Subscript[e, ind][1]->EtoN[[ind]][[1]],Subscript[e, ind][2]->EtoN[[ind]][[2]],Subscript[e, ind][3]->EtoN[[ind]][[3]]};


LegRotationDefF[gRot_][i_,rE_]:=Module[{qInd1,qInd2,qInd3,rF,FtN},
qInd1=1+3*i;
qInd2=2+3*i;
qInd3=3+3*i;
rF=gRot[[2]][Subscript[q, qInd3][t]].rE;
FtN=rF.{n[1],n[2],n[3]};
Return[{rF,FtN}]
];

TranslateFtoN[ind_,x_,FtoN_]:=x//.{Subscript[f, ind][1]->FtoN[[ind]][[1]],Subscript[f, ind][2]->FtoN[[ind]][[2]],Subscript[f, ind][3]->FtoN[[ind]][[3]]};


LegRotationDefG[gRot_][i_,rF_]:=Module[{qInd1,qInd2,qInd3,rG,GtN},
qInd1=1+3*i;
qInd2=2+3*i;
qInd3=3+3*i;
rG=gRot[[4]][].rF;
GtN=rG.{n[1],n[2],n[3]};
Return[{rG,GtN}]
];

TranslateGtoN[ind_,x_,GtoN_]:=x//.{Subscript[g, ind][1]->GtoN[[ind]][[1]],Subscript[g, ind][2]->GtoN[[ind]][[2]],Subscript[g, ind][3]->GtoN[[ind]][[3]]};


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
BrC=(cOffset)Subscript[b, i][1];
Sow[((OrA+ArB[[i]]+BrC).n[1]//TranAtoN//TranBtoN[i]//TranCtoN[i]),c];
Sow[((OrA+ArB[[i]]+BrC).n[2]//TranAtoN//TranBtoN[i]//TranCtoN[i]),c];
Sow[((OrA+ArB[[i]]+BrC).n[3]//TranAtoN//TranBtoN[i]//TranCtoN[i]),c];
(*Trochanter*)
CrD=(cOffset)Subscript[c, i][1];
Sow[((OrA+ArB[[i]]+BrC+CrD).n[1]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]),d];
Sow[((OrA+ArB[[i]]+BrC+CrD).n[2]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]),d];
Sow[((OrA+ArB[[i]]+BrC+CrD).n[3]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]),d];
(*Femur*)
DrE=(fOffset)Subscript[d, i][1];
Sow[((OrA+ArB[[i]]+BrC+CrD+DrE).n[1]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//distributeScalars),e];
Sow[((OrA+ArB[[i]]+BrC+CrD+DrE).n[2]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//distributeScalars),e];
Sow[((OrA+ArB[[i]]+BrC+CrD+DrE).n[3]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//distributeScalars),e];
(*Patella*)
ErF=(fOffset)Subscript[e, i][1];
Sow[((OrA+ArB[[i]]+BrC+CrD+DrE+ErF).n[1]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//distributeScalars),f];
Sow[((OrA+ArB[[i]]+BrC+CrD+DrE+ErF).n[2]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//distributeScalars),f];
Sow[((OrA+ArB[[i]]+BrC+CrD+DrE+ErF).n[3]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//distributeScalars),f];
(*Tarsus*)
FrG=(tOffset)Subscript[f, i][1];
Sow[((OrA+ArB[[i]]+BrC+CrD+DrE+ErF+FrG).n[1]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//TranGtoN[i]//distributeScalars),g];
Sow[((OrA+ArB[[i]]+BrC+CrD+DrE+ErF+FrG).n[2]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//TranGtoN[i]//distributeScalars),g];
Sow[((OrA+ArB[[i]]+BrC+CrD+DrE+ErF+FrG).n[3]//TranAtoN//TranBtoN[i]//TranCtoN[i]//TranDtoN[i]//TranEtoN[i]//TranFtoN[i]//TranGtoN[i]//distributeScalars),g];
(*Tarsal Tip*)
GrT=(tOffset)Subscript[g, i][1];
],1],1];
(*If[i\[Equal]1,Print[xyzo]];*)
{xyzo,BrC,CrD,DrE,ErF,FrG,GrT}
]


LegPosVectorsBAK[CONST_Association][OrA_,ArB_][i_]:=Module[{cOffset,fOffset,tOffset,x,y,z,BrC,CrD,DrE,ErF,FrG,GrT,xyzo},
cOffset=CONST["Coxa Length"]/2;
fOffset=CONST["Femur Length"]/2;
tOffset=CONST["Tarsus Length"]/2;

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
