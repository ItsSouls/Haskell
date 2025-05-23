/** ------------------------------------------------------------------------------
  * Estructuras de Datos. 2º Curso. ETSI Informática. UMA
  *
  * Control del día 4-9-2013
  * 
  * Diámetro de un grafo conexo 
  *
  * (completa y sustituye los siguientes datos)
  * Titulación: Grado en Ingeniería …………………………………… [Informática | del Software | de Computadores].
  *
  * Alumno: APELLIDOS, NOMBRE
  *
  * -------------------------------------------------------------------------------
  */

import dataStructures.graph.BreadthFirstTraversal;
import dataStructures.graph.Graph;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class GraphUtil {

	/**
	 * LENGTH: Calcula el número de elementos que contiene un iterable
	 * 
	 * @param it  El iterador
	 * @return   Número de elementos en el iterador
	 */
	public static <T> int length(Iterable<T> it) {
		int l = 0;

		for (T t: it){
			l += 1;
		}

	    return l;
	}

	/**
	 * ECCENTRICITY: Calcula la excentricidad de un vértice en un grafo El algoritmo toma la
	 * longitud del camino máximo en un recorrido en profundidad del grafo
	 * comenzando en el vértice dado.
	 * 
	 * @param graph    Grafo
	 * @param v        Vértice del grafo
	 * @return         Excentricidad del vértice
	 */
	public static <T> int eccentricity(Graph<T> graph, T v) {
		BreadthFirstTraversal<T> paths = new BreadthFirstTraversal<>(graph,v);
		int maxlength = 0;
		for(Iterable<T> aux :paths.paths()){
			if (length(aux) > maxlength){
				maxlength = length(aux);
			}
		}

	    return maxlength - 1;
	}

	/**
	 * DIAMETER: Se define como la máxima excentricidad de los vértices del grafo.
	 * 
	 * @param graph
	 * @return
	 */

	public static <T> int diameter(Graph<T> graph) {
		List<Integer> list = new ArrayList<>();
		int dia = 0;

		for (T v:graph.vertices()){
			if(eccentricity(graph, v) > dia){
				dia = eccentricity(graph,v);
			}
		}
		
	    return dia;
	}
	
	/** 
	 * Estima y justifica la complejidad del método diameter
	 */
}
