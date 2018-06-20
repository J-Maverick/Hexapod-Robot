(* ::Package:: *)

(* ::Title:: *)
(*Initialize*)


Print["Package loaded successfully."]


(* ::Title:: *)
(*Graphic/Animation Functions*)


BodyPoly[sideLength_,bodyLength_,bodyHeight_]:=Module[{angledLength,halfBodyHeight},
angledLength = (1+Sqrt[3])(bodyLength);
halfBodyHeight=bodyHeight/2;
{Polygon[{
{sideLength,bodyLength,-halfBodyHeight},
{sideLength,-bodyLength,-halfBodyHeight},
{bodyLength,-angledLength,-halfBodyHeight},
{-bodyLength,-angledLength,-halfBodyHeight},
{-sideLength,-bodyLength,-halfBodyHeight},
{-sideLength,bodyLength,-halfBodyHeight},
{-bodyLength,angledLength,-halfBodyHeight},
{bodyLength,angledLength,-halfBodyHeight}}],

Polygon[{
{sideLength,bodyLength,halfBodyHeight},
{sideLength,-bodyLength,halfBodyHeight},
{bodyLength,-angledLength,halfBodyHeight},
{-bodyLength,-angledLength,halfBodyHeight},
{-sideLength,-bodyLength,halfBodyHeight},
{-sideLength,bodyLength,halfBodyHeight},
{-bodyLength,angledLength,halfBodyHeight},
{bodyLength,angledLength,halfBodyHeight}}],

Polygon[
{{sideLength,bodyLength,-halfBodyHeight},
{sideLength,-bodyLength,-halfBodyHeight},
{sideLength,-bodyLength,halfBodyHeight},
{sideLength,bodyLength,halfBodyHeight}}],

Polygon[
{{-sideLength,bodyLength,-halfBodyHeight},
{-sideLength,-bodyLength,-halfBodyHeight},
{-sideLength,-bodyLength,halfBodyHeight},
{-sideLength,bodyLength,halfBodyHeight}}],

Polygon[
{{-bodyLength,angledLength,-halfBodyHeight},
{bodyLength,angledLength,-halfBodyHeight},
{bodyLength,angledLength,halfBodyHeight},
{-bodyLength,angledLength,halfBodyHeight}}],

Polygon[
{{-bodyLength,-angledLength,-halfBodyHeight},
{bodyLength,-angledLength,-halfBodyHeight},
{bodyLength,-angledLength,halfBodyHeight},
{-bodyLength,-angledLength,halfBodyHeight}}],

Polygon[
{{bodyLength,angledLength,-halfBodyHeight},
{sideLength,bodyLength,-halfBodyHeight},
{sideLength,bodyLength,halfBodyHeight},
{bodyLength,angledLength,halfBodyHeight}}],

Polygon[
{{bodyLength,-angledLength,-halfBodyHeight},
{sideLength,-bodyLength,-halfBodyHeight},
{sideLength,-bodyLength,halfBodyHeight},
{bodyLength,-angledLength,halfBodyHeight}}],

Polygon[
{{-bodyLength,-angledLength,-halfBodyHeight},
{-sideLength,-bodyLength,-halfBodyHeight},
{-sideLength,-bodyLength,halfBodyHeight},
{-bodyLength,-angledLength,halfBodyHeight}}],

Polygon[
{{-bodyLength,angledLength,-halfBodyHeight},
{-sideLength,bodyLength,-halfBodyHeight},
{-sideLength,bodyLength,halfBodyHeight},
{-bodyLength,angledLength,halfBodyHeight}}]
}]

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


RotDefine[i_]:=Module[{qInd1,qInd2,qInd3},
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


PosDefine[i_]:=Module[{},
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
