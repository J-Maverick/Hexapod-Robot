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


robotGraphicAnim={
(*Body*)
Translate[GeometricTransformation[bodyGraphic,Transpose[rotA]],{xAo,yAo,zAo}],

(*Arm 1*)
(*Endite*)
Translate[GeometricTransformation[enditeGraphic1,Transpose[rotB[1]]],{xBo[1],yBo[1],zBo[1]}],
(*Coxa*)
Translate[GeometricTransformation[coxaGraphic1,Transpose[rotC[1]]],{xCo[1],yCo[1],zCo[1]}],
(*Trochanter*)
Translate[GeometricTransformation[trochanterGraphic1,Transpose[rotD[1]]],{xDo[1],yDo[1],zDo[1]}],
(*Femur*)
Translate[GeometricTransformation[femurGraphic1,Transpose[rotE[1]]],{xEo[1],yEo[1],zEo[1]}],
(*Patella*)
Translate[GeometricTransformation[patellaGraphic1,Transpose[rotF[1]]],{xFo[1],yFo[1],zFo[1]}],
(*Tarsus*)
Translate[GeometricTransformation[tarsusGraphic1,Transpose[rotG[1]]],{xGo[1],yGo[1],zGo[1]}],

(*Arm 2*)
(*Endite*)
Translate[GeometricTransformation[enditeGraphic2,Transpose[rotB[2]]],{xBo[2],yBo[2],zBo[2]}],
(*Coxa*)
Translate[GeometricTransformation[coxaGraphic2,Transpose[rotC[2]]],{xCo[2],yCo[2],zCo[2]}],
(*Trochanter*)
Translate[GeometricTransformation[trochanterGraphic2,Transpose[rotD[2]]],{xDo[2],yDo[2],zDo[2]}],
(*Femur*)
Translate[GeometricTransformation[femurGraphic2,Transpose[rotE[2]]],{xEo[2],yEo[2],zEo[2]}],
(*Patella*)
Translate[GeometricTransformation[patellaGraphic2,Transpose[rotF[2]]],{xFo[2],yFo[2],zFo[2]}],
(*Tarsus*)
Translate[GeometricTransformation[tarsusGraphic2,Transpose[rotG[2]]],{xGo[2],yGo[2],zGo[2]}],

(*Arm 3*)
(*Endite*)
Translate[GeometricTransformation[enditeGraphic3,Transpose[rotB[3]]],{xBo[3],yBo[3],zBo[3]}],
(*Coxa*)
Translate[GeometricTransformation[coxaGraphic3,Transpose[rotC[3]]],{xCo[3],yCo[3],zCo[3]}],
(*Trochanter*)
Translate[GeometricTransformation[trochanterGraphic3,Transpose[rotD[3]]],{xDo[3],yDo[3],zDo[3]}],
(*Femur*)
Translate[GeometricTransformation[femurGraphic3,Transpose[rotE[3]]],{xEo[3],yEo[3],zEo[3]}],
(*Patella*)
Translate[GeometricTransformation[patellaGraphic3,Transpose[rotF[3]]],{xFo[3],yFo[3],zFo[3]}],
(*Tarsus*)
Translate[GeometricTransformation[tarsusGraphic3,Transpose[rotG3]],{xGo[3],yGo[3],zGo[3]}],

(*Arm 4*)
(*Endite*)
Translate[GeometricTransformation[enditeGraphic4,Transpose[rotB[4]]],{xBo[4],yBo[4],zBo[4]}],
(*Coxa*)
Translate[GeometricTransformation[coxaGraphic4,Transpose[rotC[4]]],{xCo[4],yCo[4],zCo[4]}],
(*Trochanter*)
Translate[GeometricTransformation[trochanterGraphic4,Transpose[rotD[4]]],{xDo[4],yDo[4],zDo[4]}],
(*Femur*)
Translate[GeometricTransformation[femurGraphic4,Transpose[rotE[4]]],{xEo[4],yEo[4],zEo[4]}],
(*Patella*)
Translate[GeometricTransformation[patellaGraphic4,Transpose[rotF[4]]],{xFo[4],yFo[4],zFo[4]}],
(*Tarsus*)
Translate[GeometricTransformation[tarsusGraphic4,Transpose[rotG[4]]],{xGo[4],yGo[4],zGo[4]}],

(*Arm 5*)
(*Endite*)
Translate[GeometricTransformation[enditeGraphic5,Transpose[rotB[5]]],{xBo[5],yBo[5],zBo[5]}],
(*Coxa*)
Translate[GeometricTransformation[coxaGraphic5,Transpose[rotC[5]]],{xCo[5],yCo[5],zCo[5]}],
(*Trochanter*)
Translate[GeometricTransformation[trochanterGraphic5,Transpose[rotD[5]]],{xDo[5],yDo[5],zDo[5]}],
(*Femur*)
Translate[GeometricTransformation[femurGraphic5,Transpose[rotE[5]]],{xEo[5],yEo[5],zEo[5]}],
(*Patella*)
Translate[GeometricTransformation[patellaGraphic5,Transpose[rotF[5]]],{xFo[5],yFo[5],zFo[5]}],
(*Tarsus*)
Translate[GeometricTransformation[tarsusGraphic5,Transpose[rotG[5]]],{xGo[5],yGo[5],zGo[5]}],

(*Arm 6*)
(*Endite*)
Translate[GeometricTransformation[enditeGraphic6,Transpose[rotB[6]]],{xBo[6],yBo[6],zBo[6]}],
(*Coxa*)
Translate[GeometricTransformation[coxaGraphic6,Transpose[rotC[6]]],{xCo[6],yCo[6],zCo[6]}],
(*Trochanter*)
Translate[GeometricTransformation[trochanterGraphic6,Transpose[rotD[6]]],{xDo[6],yDo[6],zDo[6]}],
(*Femur*)
Translate[GeometricTransformation[femurGraphic6,Transpose[rotE[6]]],{xEo[6],yEo[6],zEo[6]}],
(*Patella*)
Translate[GeometricTransformation[patellaGraphic6,Transpose[rotF[6]]],{xFo[6],yFo[6],zFo[6]}],
(*Tarsus*)
Translate[GeometricTransformation[tarsusGraphic6,Transpose[rotG[6]]],{xGo[6],yGo[6],zGo[6]}]
};


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
