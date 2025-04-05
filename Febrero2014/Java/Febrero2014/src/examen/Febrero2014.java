package examen;

import java.util.Iterator;
import java.util.List;

import dataStructures.graph.BreadthFirstTraversal;
import dataStructures.graph.DepthFirstTraversal;
import dataStructures.graph.DiGraph;
import dataStructures.graph.DictionaryDiGraph;
import dataStructures.set.Set;
import dataStructures.set.HashSet;
import dataStructures.dictionary.Dictionary;
import dataStructures.dictionary.HashDictionary;

public class Febrero2014<V> {

	public static <V> DiGraph<V> reverseDiGraph(DiGraph<V> g) {

		DiGraph<V> sol = new DictionaryDiGraph<V>();

		for(V v:g.vertices()){
			sol.addVertex(v);
			for(V suc: g.successors(v)){
				sol.addVertex(suc);
				sol.addDiEdge(suc,v);
			}
		}
		return sol;

	}

	public static <V> DiGraph<V> restrictDiGraph(DiGraph<V> g, Set<V> vs) {
		DiGraph<V> sol = new DictionaryDiGraph<>();
		Iterator<V> it = g.vertices().iterator();

		for(V v : vs){
			sol.addVertex(v);
		}

		while(it.hasNext()){
			V actualvertex = it.next();
			if(vs.isElem(actualvertex)){
				for(V x :g.successors(actualvertex)){
					if(vs.isElem(x)){
						sol.addDiEdge(actualvertex,x);
					}
				}
			}
		}
		return sol;
	}

	public static <V> Set<V> sccOf (DiGraph<V> g, V src) {

		DepthFirstTraversal<V> dft = new DepthFirstTraversal<V>(g, src);
		Set<V> vs = new HashSet<>();

		Iterator<V> it = dft.verticesIterator();

		while (it.hasNext()){
			vs.insert(it.next());
		}

		DiGraph<V> gr = reverseDiGraph(restrictDiGraph(g,vs));

		DepthFirstTraversal<V> sol = new DepthFirstTraversal<>(gr, src);
		Iterator<V> itaux = sol.vertices().iterator();
		vs = new HashSet<>();

		while (itaux.hasNext()){
			vs.insert(itaux.next());
		}
		return vs;
	}

	public static <V> Set<Set<V>> stronglyConnectedComponentsDiGraph(DiGraph<V> g) {
		Set<Set<V>> sol = new HashSet<>();
		Set<V> scc = sccOf(g,firstVertex(g.vertices()));

		if(g.vertices().isEmpty()){
			return sol;
		}else{
			sol.insert(scc);
			for(V v: scc){
				g.deleteVertex(v);
			}
			sol = stronglyConnectedComponentsDiGraphaux(g,sol);
		}
		return sol;
	}

	public static <V> Set<Set<V>> stronglyConnectedComponentsDiGraphaux(DiGraph<V> g, Set<Set<V>> solution) {

		if(g.vertices().isEmpty()){
			return solution;
		}else{
			Set<V> scc = sccOf(g,firstVertex(g.vertices()));
			solution.insert(scc);
			for(V v: scc){
				g.deleteVertex(v);
			}
			return stronglyConnectedComponentsDiGraphaux(g, solution);
		}
	}
	
	private static <V> V firstVertex(Set<V> v) {
		Iterator<V> iter = v.iterator();
		return iter.next();
	}
	
	public static void main(String[] args) {
		DiGraph<Character> g = new DictionaryDiGraph<Character>(); 
		
		Character A = 'A';
		Character B = 'B';
		Character C = 'C';
		Character D = 'D';
		Character E = 'E';
		Character F = 'F';
		Character G = 'G';
		Character H = 'H';
		
		g.addVertex(A);
		g.addVertex(B);
		g.addVertex(C);
		g.addVertex(D);
		g.addVertex(E);
		g.addVertex(F);
		g.addVertex(G);
		g.addVertex(H);
		
		g.addDiEdge(A, B);
		g.addDiEdge(B, E);
		g.addDiEdge(B, F);
		g.addDiEdge(C, D);
		g.addDiEdge(C, G);
		g.addDiEdge(D, C);
		g.addDiEdge(D, H);
		g.addDiEdge(E, A);
		g.addDiEdge(E, F);
		g.addDiEdge(F, G);
		g.addDiEdge(G, F);
		g.addDiEdge(H, D);
		g.addDiEdge(H, G);
		
		System.out.println(g.toString());
		
		System.out.println(reverseDiGraph(g).toString());
		
		Set<Character> vs = new HashSet<Character>();
		
		vs.insert(A);
		vs.insert(B);
		vs.insert(E);
		vs.insert(F);
		vs.insert(G);
		
		System.out.println("restrictdigraph:" + restrictDiGraph(g, vs).toString());
		System.out.println(sccOf(g, A).toString());
		System.out.println(stronglyConnectedComponentsDiGraph(g).toString());
		
	}
	
}
