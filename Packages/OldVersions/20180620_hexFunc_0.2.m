(* ::Package:: *)

(* ::Title:: *)
(*Initialize*)


Print["Package loaded successfully."]


(* ::Title:: *)
(*Graphics Functions*)


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


(* ::Title:: *)
(*Vectors*)


RotDefine[i_]:=Module[{qInd1,qInd2,qInd3},
qInd1=1+3*i;
qInd2=2+3*i;
qInd3=3+3*i;
rotB[i]=rot3[Subscript[q, qInd1][t]+legRotOffset[i]].rot3[Subscript[q, 3][t]].rot2[Subscript[q, 2][t]].rot1[Subscript[q, 1][t]];
BtoN[i]=rotB[i].{n[1],n[2],n[3]};
TranBtoN[ind_,x_]:=x//.{Subscript[b, ind][1]->BtoN[ind,[1]],Subscript[b, ind][2]->BtoN[ind,[2]],Subscript[b, ind][3]->BtoN[ind,[3]]};
rotC[i]=rot0[].rot3[Subscript[q, qInd1][t]+legRotOffset[i]].rot3[Subscript[q, 3][t]].rot2[Subscript[q, 2][t]].rot1[Subscript[q, 1][t]];
CtoN[i]=rotC[i].{n[1],n[2],n[3]};
TranCtoN[ind_,x_]:=x//.{Subscript[b, ind][1]->CtoN[ind,[1]],Subscript[b, ind][2]->CtoN[ind,[2]],Subscript[b, ind][3]->CtoN[ind,[3]]};
rotD[i]=rot2[Subscript[q, qInd2][t]].rot0[].rot3[Subscript[q, qInd1][t]+legRotOffset[i]].rot3[Subscript[q, 3][t]].rot2[Subscript[q, 2][t]].rot1[Subscript[q, 1][t]];
DtoN[i]=rotD[i].{n[1],n[2],n[3]};
TranDtoN[ind_,x_]:=x//.{Subscript[b, ind][1]->DtoN[ind,[1]],Subscript[b, ind][2]->DtoN[ind,[2]],Subscript[b, ind][3]->DtoN[ind,[3]]};
rotE[i]=rot0[].rot2[Subscript[q, qInd2][t]].rot0[].rot3[Subscript[q, qInd1][t]+legRotOffset[i]].rot3[Subscript[q, 3][t]].rot2[Subscript[q, 2][t]].rot1[Subscript[q, 1][t]];
EtoN[i]=rotE[i].{n[1],n[2],n[3]};
TranEtoN[ind_,x_]:=x//.{Subscript[b, ind][1]->EtoN[ind,[1]],Subscript[b, ind][2]->EtoN[ind,[2]],Subscript[b, ind][3]->EtoN[ind,[3]]};
rotF[i]=rot2[Subscript[q, qInd3][t]].rot0[].rot2[Subscript[q, qInd2][t]].rot0[].rot3[Subscript[q, qInd1][t]+legRotOffset[i]].rot3[Subscript[q, 3][t]].rot2[Subscript[q, 2][t]].rot1[Subscript[q, 1][t]];
FtoN[i]=rotF[i].{n[1],n[2],n[3]};
TranFtoN[ind_,x_]:=x//.{Subscript[b, ind][1]->FtoN[ind,[1]],Subscript[b, ind][2]->FtoN[ind,[2]],Subscript[b, ind][3]->FtoN[ind,[3]]};
rotG[i]=rot0[].rot2[Subscript[q, qInd3][t]].rot0[].rot2[Subscript[q, qInd2][t]].rot0[].rot3[Subscript[q, qInd1][t]+legRotOffset[i]].rot3[Subscript[q, 3][t]].rot2[Subscript[q, 2][t]].rot1[Subscript[q, 1][t]]
GtoN[i]=rotG[i].{n[1],n[2],n[3]};
TranGtoN[ind_,x_]:=x//.{Subscript[b, ind][1]->GtoN[ind,[1]],Subscript[b, ind][2]->GtoN[ind,[2]],Subscript[b, ind][3]->GtoN[ind,[3]]}];
