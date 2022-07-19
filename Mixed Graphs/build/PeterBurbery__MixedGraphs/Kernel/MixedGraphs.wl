(* ::Package:: *)

BeginPackage["PeterBurbery`MixedGraphs`"];

(* Declare your packages public symbols here. *)

RandomMixedGraph;
EulerizeGraph;
UndirectedGraphToMixedGraph;
RandomWeightedMixedGraph;
MixedGraphDirectedArcs;
MixedGraphUndirectedEdges;
GraphInformation;
TakeLargestGraphComponentBy;
GraphicalDegreeSequenceQ;
GraphConvexHull;
OddVertexList;
OddVertexQ;
EvenVertexList;
EvenVertexQ;
RandomSymbolicMixedGraph;
RandomSymbolicWeightedMixedGraph;
MixedGraphToDigraph;
GeneralizedGraphData;
Begin["`Private`"];

(* Define your public and private symbols here. *)

ClearAll[RandomMixedGraph,EulerizeGraph,UndirectedGraphToMixedGraph,RandomWeightedMixedGraph,MixedGraphDirectedArcs,MixedGraphUndirectedEdges,GraphInformation,TakeLargestGraphComponentBy,GraphicalDegreeSequenceQ,GraphConvexHull,OddVertexList,OddVertexQ,EvenVertexList,EvenVertexQ,RandomSymbolicWeightedMixedGraph,MixedGraphToDigraph,GeneralizedGraphData];
RandomMixedGraph[{vertices_,edges_},threshold_,options:OptionsPattern[]]/;0<=threshold<=1:=Block[{replaceCount,randomGraph} ,randomGraph=RandomGraph[{vertices,edges},options];replaceCount=Floor[threshold edges];randomGraph=Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,randomGraph,RandomSample[EdgeList[randomGraph],replaceCount]]]];

RandomMixedGraph[{vertices_,edges_},threshold_,k_?IntegerQ,options:OptionsPattern[]]/;0<=threshold<=1:=Table[RandomMixedGraph[{vertices,edges},threshold,options],k]

RandomMixedGraph[{vertices_,edges_},threshold_,array_List,options:OptionsPattern[]]/;0<=threshold<=1:=Array[RandomMixedGraph[{vertices,edges},threshold,options]&,array]

RandomMixedGraph[graphDistribution_,threshold_,options:OptionsPattern[]]/;0<=threshold<=1:=Block[{replaceCount,randomGraph} ,randomGraph=RandomGraph[graphDistribution,options];replaceCount=Floor[threshold EdgeCount[randomGraph]];randomGraph=Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,randomGraph,RandomSample[EdgeList[randomGraph],replaceCount]]]];

RandomMixedGraph[graphDistribution_,threshold_,k_?IntegerQ,options:OptionsPattern[]]/;0<=threshold<=1\[And]k>=1:=Table[RandomMixedGraph[graphDistribution,threshold,options],k]

RandomMixedGraph[graphDistribution_,threshold_,array_List,options:OptionsPattern[]]/;0<=threshold<=1:=Array[RandomMixedGraph[graphDistribution,threshold,options]&,array]


RandomMixedGraph::usage ="RandomMixedGraph[{vertices, edges}, threshold] creates a random mixed graph with vertices vertices and edges edges where directed edges make up threshold of the entirenumber of edges\nRandomMixedGraph[{vertices, edges}, threshold, k] creates k random mixed graphs with vertices vertices and edges edges where directed edges make up threshold of the entire number of edges\nRandomMixedGraph[{vertices, edges}, threshold, randomFunction, array] creates an array of dimensions array of random mixed graphs with vertices vertices and edges edges where directed edges make up threshold of the entire number of edges \nRandomMixedGraph[distribution, threshold, randomFunction] creates a random mixed graph with graph distribution distribution where directed edges make up threshold of the entire number of edges \nRandomMixedGraph[distribution, threshold, randomFunction, k] creates k random mixed graphs with graph distribution distribution where directed edges make up threshold of the entire number of edges with edge weights assigned by randomFunction\nRandomMixedGraph[distribution, threshold, array] creates an array of dimensions array of random mixed graphs with graph distribution distribution where directed edges make up threshold of the entire number of edges";

UndirectedGraphToMixedGraph[graph_?GraphQ,threshold_]:=Block[{replaceCount} ,replaceCount=Floor[threshold EdgeCount[graph]];Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,graph,RandomSample[EdgeList[graph],replaceCount]],EdgeWeight->Thread[EdgeList[graph]->(AnnotationValue[{graph,#1},EdgeWeight])&/@EdgeList[graph]]]]/;0<=threshold<=1

RandomWeightedMixedGraph[{vertices_,edges_},threshold_,randomFunction_,options:OptionsPattern[]]/;0<=threshold<=1:=Block[{replaceCount,randomGraph} ,randomGraph=RandomGraph[{vertices,edges},options];replaceCount=Floor[threshold edges];randomGraph=Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,randomGraph,RandomSample[EdgeList[randomGraph],replaceCount]]];Graph[randomGraph,EdgeWeight->(Map[#->randomFunction[]&,EdgeList[randomGraph]])]];

RandomWeightedMixedGraph[{vertices_,edges_},threshold_,randomFunction_,k_?IntegerQ,options:OptionsPattern[]]/;0<=threshold<=1:=Table[RandomWeightedMixedGraph[{vertices,edges},threshold,randomFunction],k]

RandomWeightedMixedGraph[{vertices_,edges_},threshold_,randomFunction_,array_List,options:OptionsPattern[]]/;0<=threshold<=1:=Array[RandomWeightedMixedGraph[{vertices,edges},threshold,randomFunction]&,array]

RandomWeightedMixedGraph[graphDistribution_,threshold_,randomFunction_,options:OptionsPattern[]]/;0<=threshold<=1:=Block[{replaceCount,randomGraph} ,randomGraph=RandomGraph[graphDistribution,options];replaceCount=Floor[threshold EdgeCount[randomGraph]];randomGraph=Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,randomGraph,RandomSample[EdgeList[randomGraph],replaceCount]]];Graph[randomGraph,EdgeWeight->(Map[#->randomFunction[]&,EdgeList[randomGraph]])]];

RandomWeightedMixedGraph[graphDistribution_,threshold_,randomFunction_,k_?IntegerQ,options:OptionsPattern[]]/;0<=threshold<=1\[And]k>=1:=Table[RandomWeightedMixedGraph[graphDistribution,threshold,randomFunction],k]

RandomWeightedMixedGraph[graphDistribution_,threshold_,randomFunction_,array_List,options:OptionsPattern[]]/;0<=threshold<=1:=Array[RandomWeightedMixedGraph[graphDistribution,threshold,randomFunction]&,array]
RandomWeightedMixedGraph::usage="RandomWeightedMixedGraph[{vertices, edges}, threshold, randomFunction] creates a random mixed graph with vertices vertices and edges edges where directed edges make up threshold of the entire number of edges with edge weights assigned by randomFunction\nRandomWeightedMixedGraph[{vertices, edges}, threshold, randomFunction, k] creates k random mixed graphs with vertices vertices and edges edges where directed edges make up threshold of the entire number of edges with edge weights assigned by randomFunction\nRandomWeightedMixedGraph[{vertices, edges}, threshold, randomFunction, array] creates an array of dimensions array of random mixed graphs with vertices vertices and edges edges where directed edges make up threshold of the entire number of edges with edge weights assigned by randomFunction\nRandomWeightedMixedGraph[distribution, threshold, randomFunction] creates a random mixed graph with graph distribution distribution where directed edges make up threshold of the entire number of edges with edge weights assigned by randomFunction\nRandomWeightedMixedGraph[distribution, threshold, randomFunction, k] creates k random mixed graphs with graph distribution distribution where directed edges make up threshold of the entire number of edges with edge weights assigned by randomFunction\nRandomWeightedMixedGraph[distribution, threshold, randomFunction, array] creates an array of dimensions array of random mixed graphs with graph distribution distribution where directed edges make up threshold of the entire number of edges with edge weights assigned by randomFunction";

MixedGraphDirectedArcs[graph_?GraphQ]:=Cases[EdgeList[graph],_\[DirectedEdge]_]

MixedGraphUndirectedEdges[graph_?GraphQ]:=EdgeList[graph,_\[UndirectedEdge]_]

ClearAll[EulerizeGraph]
EulerizeGraph[graph_?(UndirectedGraphQ[#]\[And]ConnectedGraphQ[#]&)]:=EdgeAdd[graph,FindEdgeCover[Subgraph[graph,VertexList[graph,x_/;OddQ@VertexDegree[graph,x]]]]]
EulerizeGraph[graph_?(MixedGraphQ[#]\[And]ConnectedGraphQ[#]&)]:=EdgeAdd[graph,FindEdgeCover[Subgraph[graph,VertexList[graph,x_/;OddQ@VertexDegree[graph,x]]]]]
GraphInformation[graph_?GraphQ]:=<|"Acyclic"->AcyclicGraphQ[graph],"Bipartite"->BipartiteGraphQ[graph],"Complete"->CompleteGraphQ[graph],"Connected"->ConnectedGraphQ[graph],"EdgeTransitive"->EdgeTransitiveGraphQ[graph],"WeightedEdge"->EdgeWeightedGraphQ[graph],"Empty"->EmptyGraphQ[graph],"Eulerian"->EulerianGraphQ[graph],"Hamiltonian"->HamiltonianGraphQ[graph],"LoopFree"->LoopFreeGraphQ[graph],"Mixed"->MixedGraphQ[graph],"Path"->PathGraphQ[graph],"Planar"->PlanarGraphQ[graph],"Simple"->SimpleGraphQ[graph],"Tree"->TreeGraphQ[graph],"Undirected"->UndirectedGraphQ[graph],"VertexTransitive"->VertexTransitiveGraphQ[graph],"WeightedVertex"->VertexWeightedGraphQ[graph],"WeaklyConnected"->WeaklyConnectedGraphQ[graph],"Weighted"->WeightedGraphQ[graph]|>

TakeLargestGraphComponentBy[graph_?GraphQ,function_:EdgeCount,length_:1]:=TakeLargestBy[ConnectedGraphComponents[graph],function,length]


GraphicalDegreeSequenceQ[sequence:{___Integer}]:=EvenQ[Total[sequence]]&&Block[{orderedSequence},orderedSequence=ReverseSort[sequence];ContainsOnly[(k|->( Sum[orderedSequence[[i]],{i,1,k}]<=k(k-1)+Sum[Min[{orderedSequence[[i]],k}],{i,k+1,Length[sequence]}]))/@Range[Length[sequence]],{True}]]

GraphConvexHull[graph_?GraphQ,vertexSet_]:=FixedPoint[Function[in,Union@Flatten@Function[{g,v},FindPath[g,First[#],Last[#],GraphDistance[g,First[#],Last[#]],All]&/@Subsets[v,{2}]][graph,in]][#]&,{1,2,9}]/;SubsetQ[VertexList[graph],vertexSet]


OddVertexList[graph_?GraphQ]:=VertexList[graph,_?(OddQ[VertexDegree[graph,#]]&)]
OddVertexQ[graph_?GraphQ,vertex_]:=OddQ[VertexDegree[graph,vertex]]/;MemberQ[VertexList[graph],vertex]
EvenVertexList[graph_?GraphQ]:=VertexList[graph,_?(EvenQ[VertexDegree[graph,#]]&)]
EvenVertexQ[graph_?GraphQ,vertex_]:=EvenQ[VertexDegree[graph,vertex]]/;MemberQ[VertexList[graph],vertex]

RandomSymbolicMixedGraph[{vertices_,edges_},threshold_,variablename_,options:OptionsPattern[]]/;0<=threshold<=1:=Block[{replaceCount,randomGraph},randomGraph=VertexReplace[RandomGraph[{vertices,edges},options],Thread[Range[20]->(Indexed[variablename,#1]&)/@Range[20]]];replaceCount=Floor[threshold edges];randomGraph=Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,randomGraph,RandomSample[EdgeList[randomGraph],replaceCount]]]];
 RandomSymbolicMixedGraph[{vertices_,edges_},threshold_,variablename_,k_?IntegerQ,options:OptionsPattern[]]/;0<=threshold<=1:=Table[RandomSymbolicMixedGraph[{vertices,edges},threshold],k];

 RandomSymbolicMixedGraph[{vertices_,edges_},threshold_,variablename_,array_List,options:OptionsPattern[]]/;0<=threshold<=1:=Array[RandomSymbolicMixedGraph[{vertices,edges},threshold]&,array];

 RandomSymbolicMixedGraph[graphDistribution_,threshold_,variablename_,options:OptionsPattern[]]/;0<=threshold<=1:=Block[{replaceCount,randomGraph},randomGraph=RandomGraph[graphDistribution,options];replaceCount=Floor[threshold EdgeCount[randomGraph]];randomGraph=Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,randomGraph,RandomSample[EdgeList[randomGraph],replaceCount]]]];

 RandomSymbolicMixedGraph[graphDistribution_,threshold_,variablename_,k_?IntegerQ,options:OptionsPattern[]]/;0<=threshold<=1&&k>=1:=Table[RandomSymbolicMixedGraph[graphDistribution,threshold],k];

 RandomSymbolicMixedGraph[graphDistribution_,threshold_,variablename_,array_List,options:OptionsPattern[]]/;0<=threshold<=1:=Array[RandomSymbolicMixedGraph[graphDistribution,threshold]&,array];


RandomSymbolicWeightedMixedGraph[{vertices_,edges_},threshold_,randomFunction_,variablename_,options:OptionsPattern[]]/;0<=threshold<=1:=Block[{replaceCount,randomGraph},randomGraph=VertexReplace[RandomGraph[{vertices,edges},options],Thread[Range[20]->(Indexed[variablename,#]&/@Range[20])]];replaceCount=Floor[threshold edges];randomGraph=Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,randomGraph,RandomSample[EdgeList[randomGraph],replaceCount]]];Graph[randomGraph,EdgeWeight->(#1->randomFunction[]&)/@EdgeList[randomGraph]]];

RandomSymbolicWeightedMixedGraph[{vertices_,edges_},threshold_,randomFunction_,variablename_,k_?IntegerQ,options:OptionsPattern[]]/;0<=threshold<=1:=Table[RandomSymbolicWeightedMixedGraph[{vertices,edges},threshold,randomFunction],k];
RandomSymbolicWeightedMixedGraph[{vertices_,edges_},threshold_,randomFunction_,variablename_,array_List,options:OptionsPattern[]]/;0<=threshold<=1:=Array[RandomSymbolicWeightedMixedGraph[{vertices,edges},threshold,randomFunction]&,array];

RandomSymbolicWeightedMixedGraph[graphDistribution_,threshold_,randomFunction_,variablename_,options:OptionsPattern[]]/;0<=threshold<=1:=Block[{replaceCount,randomGraph},randomGraph=RandomGraph[graphDistribution,options];replaceCount=Floor[threshold EdgeCount[randomGraph]];randomGraph=Graph[Fold[EdgeAdd[EdgeDelete[#1,#2],First[#2]\[DirectedEdge]Last[#2]]&,randomGraph,RandomSample[EdgeList[randomGraph],replaceCount]]];Graph[randomGraph,EdgeWeight->(#1->randomFunction[]&)/@EdgeList[randomGraph]]];

RandomSymbolicWeightedMixedGraph[graphDistribution_,threshold_,randomFunction_,variablename_,k_?IntegerQ,options:OptionsPattern[]]/;0<=threshold<=1&&k>=1:=Table[RandomSymbolicWeightedMixedGraph[graphDistribution,threshold,randomFunction],k];

 RandomSymbolicWeightedMixedGraph[graphDistribution_,threshold_,randomFunction_,variablename_,array_List,options:OptionsPattern[]]/;0<=threshold<=1:=Array[RandomSymbolicWeightedMixedGraph[graphDistribution,threshold,randomFunction]&,array];

MixedGraphToDigraph[graph_?MixedGraphQ, 
  options : OptionsPattern[]] := 
 WeightedAdjacencyGraph[
  Replace[Normal@
    Chop@WeightedAdjacencyMatrix[graph], {0 -> \[Infinity]}, {2}], 
  options]
  GeneralizedGraphData[graph_?GraphQ]:=AssociationThread[{"IncidenceMatrix","Order","Size","Nodes","Edges","UndirectedEdges","DirectedArcs","AdjacencyMatrix"}->{IncidenceMatrix[graph],VertexCount[graph],EdgeCount[graph],VertexList[graph],EdgeList[graph],EdgeList[graph,_\[UndirectedEdge]_],EdgeList[graph,_\[DirectedEdge]_],AdjacencyMatrix[graph]}]


End[]; (* End `Private` *)

EndPackage[];


