(* ::Package:: *)

Get["Alex`"];


measles=Import["https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv"];


measles=measles//associationThreadAll[First[#],Rest[#]]&;


measles//Length


measles=measles//keyDropAll["index"];


Keys[measles[[1]]]


measles[[1]]


(* ::Text:: *)
(*Overall measles vaccination rate*)


mmr=measles//GroupBy[#state&->(#mmr&)]/*Map[Select[NumericQ[#]&&#>0&]]/*Select[Length[#]>1&]/*ReverseSortBy[Median]/*Function[BoxWhiskerChart[#,ChartStyle->Black,LabelStyle->Directive[FontName->"Arial",12,Black],BarOrigin->Left,ChartLabels->Automatic,AspectRatio->1.2,ImageSize->500,PlotLabel->"MMR vaccination rates by state"]]


(* ::Text:: *)
(*Overall vaccination rate*)


overall=measles//GroupBy[#state&->(#overall&)]/*Map[Select[NumericQ[#]&&#>0&]]/*Select[Length[#]>1&]/*ReverseSortBy[Median]/*Function[BoxWhiskerChart[#,ChartStyle->Black,LabelStyle->Directive[FontName->"Arial",12,Black],BarOrigin->Left,ChartLabels->Automatic,AspectRatio->1.2,ImageSize->500,PlotLabel->"Overall vaccination rates by state"]]


Framed[Grid[{{overall,mmr}}],FrameStyle->None,ImageMargins->5]


Export["Downloads/vaccinations.png",%,ImageResolution->200]
