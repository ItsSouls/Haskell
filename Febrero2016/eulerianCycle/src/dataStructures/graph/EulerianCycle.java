/**
 * Student's name:
 * Student's group:
 *
 * Data Structures. Grado en Informática. UMA.
 */

package dataStructures.graph;

import dataStructures.list.*;

public class EulerianCycle<V> {
    private List<V> eCycle;

    @SuppressWarnings("unchecked")
    public EulerianCycle(Graph<V> g) {
        Graph<V> graph = (Graph<V>) g.clone();
        eCycle = eulerianCycle(graph);
    }

    public boolean isEulerian() {
        return eCycle != null;
    }

    public List<V> eulerianCycle() {
        return eCycle;
    }

    // J.1
    private static <V> boolean isEulerian(Graph<V> g) {
        if(g.isEmpty() || g.vertices().size() == 1){
            return true;
        }else if (g.vertices().size() == 2){
            return false;
        }else{
            for(V v:g.vertices()){
                if(g.degree(v) % 2 != 0){
                    return false;
                }
            }
        }
        return true;
    }

    // J.2
    private static <V> void remove(Graph<V> g, V v, V u) {
        g.deleteEdge(v,u);
        for(V ver :g.vertices()){
            if(g.degree(ver) == 0){
                g.deleteVertex(ver);
            }
        }
    }

    // J.3
    private static <V> List<V> extractCycle(Graph<V> g, V v0) {
        List<V> ciclo = new ArrayList<>();
        ciclo.append(v0);
        V v = v0;

        while (!g.successors(v).isEmpty()){
            V u = g.successors(v).iterator().next();
            ciclo.append(u);
            remove(g,v,u);
            v = u;
        }
        return ciclo;
    }

    // J.4
    private static <V> void connectCycles(List<V> xs, List<V> ys) {
    	List<V> sol = new ArrayList<>();
        if(xs.isEmpty()){
            for(V y:ys) xs.append(y);
        }else{
            for (V v : xs){
                if(v == ys.get(0)){
                    for(V y:ys) {
                        sol.append(y);
                    }
                }else{
                    sol.append(v);
                }
            }
            while (!xs.isEmpty()){
                xs.remove(0);// Limpiar xs
            }
            for (V v : sol) {
                xs.append(v);  // Rellenar xs con los elementos de sol
            }
        }
    }

    // J.5
    private static <V> V vertexInCommon(Graph<V> g, List<V> cycle) {
    		for(V v:cycle){
                if(g.vertices().isElem(v)){
                    return v;
                }
            }
    		return null;
    }

    // J.6
    private static <V> List<V> eulerianCycle(Graph<V> g) {
    	if(!isEulerian(g)){
            return null;
        }else{
            List<V> cicloparcial = extractCycle(g,g.vertices().iterator().next());
            while(!g.isEmpty()){
                connectCycles(cicloparcial,(extractCycle(g,vertexInCommon(g,cicloparcial))));
            }
            return cicloparcial;
        }
    }
}
