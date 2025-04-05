/**----------------------------------------------
 * -- Estructuras de Datos.  2018/19
 * -- 2º Curso del Grado en Ingeniería [Informática | del Software | de Computadores].
 * -- Escuela Técnica Superior de Ingeniería en Informática. UMA
 * --
 * -- Examen 4 de febrero de 2019
 * --
 * -- ALUMNO/NAME:
 * -- GRADO/STUDIES:
 * -- NÚM. MÁQUINA/MACHINE NUMBER:
 * --
 * ----------------------------------------------
 */

import dataStructures.graph.WeightedGraph;
import dataStructures.graph.WeightedGraph.WeightedEdge;

import dataStructures.dictionary.Dictionary;
import dataStructures.dictionary.HashDictionary;
import dataStructures.priorityQueue.LinkedPriorityQueue;
import dataStructures.set.HashSet;
import dataStructures.set.Set;

import java.util.PriorityQueue;

public class Kruskal {
	public static <V,W> Set<WeightedEdge<V,W>> kruskal(WeightedGraph<V,W> g) {
		Dictionary<V,V> dict = new HashDictionary<>();
		LinkedPriorityQueue<WeightedEdge<V,W>> pq = new LinkedPriorityQueue<>();
		Set<WeightedEdge<V,W>> t = new HashSet<>();

		for(V v : g.vertices()){
			dict.insert(v,v);
		}
		for(WeightedEdge<V,W> e:g.edges()){
			pq.enqueue(e);
		}

		while(!pq.isEmpty()){
			WeightedEdge<V,W> primero = pq.first();
			pq.dequeue();
			if(representante(dict,primero.source()) != representante(dict,primero.destination())){
				dict.insert(representante(dict, primero.destination()), primero.source());
				t.insert(primero);
			}
		}
		
		return t;
	}

	public static <V> V representante(Dictionary<V,V> dict, V v){
		while (dict.valueOf(v) != v){
			v = dict.valueOf(v);
		}
		return v;
	}


	// Sólo para evaluación continua / only for part time students
	public static <V,W> Set<Set<WeightedEdge<V,W>>> kruskals(WeightedGraph<V,W> g) {

		// COMPLETAR
		
		return null;
	}
}
