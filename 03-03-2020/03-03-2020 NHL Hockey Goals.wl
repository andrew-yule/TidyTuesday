(* ::Package:: *)

goals=Import["https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv"];


goals=goals//Rest//associationThreadAll[goals[[1]]];


goals[[1]]


goals//countBy[#player&]
Length[%]


goals[[-5]]


ageOp[str_]:=StringSplit[str,"-"]//ToExpression/*Apply[Function[#1+#2/365.]]


goalsByPlayerAndAge=goals//GroupBy[Function[#player]->Function[{ageOp[#age],#goals}]]/*Map[Sort/*Function[FoldList[Function[{d1,d2},{d2[[1]],d1[[2]]+d2[[2]]}],#]]](*/*Select[#[[-1,2]]\[GreaterEqual]700&]*);


goalsByPlayerAndAge//Keys


goalsByPlayerAndAge=goalsByPlayerAndAge//KeySortBy[MatchQ[#,"Alex Ovechkin"|"Wayne Gretzky"]&];


Framed[Column[{
Item[Style["Accumulated career goals with age for top hockey players",Bold,16,FontFamily->"Arial"],ItemSize->40],
Style["Alex Ovechkin is impressive but still far from Wayne Gretzky",12,FontFamily->"Arial"],
goalsByPlayerAndAge//ggListPlot[#//KeyValueMap[Function[{key,value},If[MatchQ[key,"Alex Ovechkin"|"Wayne Gretzky"],Callout[value,key//stringAppend[" ("<>ToString[Max[value[[All,2]]]]<>" goals)"],{Scaled[1],Above}],value]]],PlotStyle->Table[Which[key=="Wayne Gretzky",{Thick,Blue},key=="Alex Ovechkin",{Thick,Red},True,Gray],{key,Keys@#}],FrameLabel->{"Age","Goals Scored"}]&
},Alignment->Left],FrameStyle->None,ImageMargins->5]


Export["Downloads/NHL Goals.png",%,ImageResolution->200]


(* ::Text:: *)
(*How many players have scored a certain number of goals on their birthday*)


goals[[All,"age"]]//Map[StringSplit[#,"-"]&/*Last/*ToExpression]/*ReverseSort


goals//GroupBy[Function[#player]->Function[{ToExpression@Last@StringSplit[#age,"-"],#goals}]]//Map[Select[#[[1]]==0&]/*slice[All,2]/*Total]/*Sort/*Function[BarChart[#,BarOrigin->Left,ChartLabels->Automatic,AspectRatio->1.25,ImageSize->500,LabelStyle->Directive[FontFamily->"Arial",12,Black],Frame->{{True,False},{True,False}},FrameLabel->"Number of goals scored on birthday"]]
