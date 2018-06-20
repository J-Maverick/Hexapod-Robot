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
