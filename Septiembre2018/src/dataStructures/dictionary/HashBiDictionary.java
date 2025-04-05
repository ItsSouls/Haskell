package dataStructures.dictionary;
import dataStructures.list.List;

import dataStructures.list.ArrayList;
import dataStructures.set.AVLSet;
import dataStructures.set.Set;
import dataStructures.tuple.Tuple2;

import java.security.cert.TrustAnchor;
import java.util.HashSet;

/**
 * Estructuras de Datos. Grados en Informatica. UMA.
 * Examen de septiembre de 2018.
 *
 * Apellidos, Nombre:
 * Titulacion, Grupo:
 */
public class HashBiDictionary<K,V> implements BiDictionary<K,V>{
	private Dictionary<K,V> bKeys;
	private Dictionary<V,K> bValues;
	
	public HashBiDictionary() {
		bKeys = new HashDictionary<>();
		bValues = new HashDictionary<>();
	}
	
	public boolean isEmpty() {
		return bKeys.isEmpty() && bValues.isEmpty();
	}
	
	public int size() {
		return bKeys.size();
	}
	
	public void insert(K k, V v) {
		if(bKeys.isDefinedAt(k)){
			bValues.delete(bKeys.valueOf(k));
			bValues.insert(v,k);
			bKeys.insert(k,v);
		}else{
			bKeys.insert(k,v);
			bValues.insert(v,k);
		}
	}
	
	public V valueOf(K k) {
		if(bKeys.isDefinedAt(k)){
			return bKeys.valueOf(k);
		}else{
			return null;
		}
	}
	
	public K keyOf(V v) {
		if(bValues.isDefinedAt(v)){
			return bValues.valueOf(v);
		}else{
			return null;
		}
	}
	
	public boolean isDefinedKeyAt(K k) {
		return bKeys.isDefinedAt(k);
	}
	
	public boolean isDefinedValueAt(V v) {
		return bValues.isDefinedAt(v);
	}
	
	public void deleteByKey(K k) {
		if(bKeys.isDefinedAt(k)){
			bValues.delete(bKeys.valueOf(k));
			bKeys.delete(k);
		}else{
			throw new RuntimeException("La K no existe");
		}
	}
	
	public void deleteByValue(V v) {
		if(bValues.isDefinedAt(v)){
			bKeys.delete(bValues.valueOf(v));
			bValues.delete(v);
		}else{
			throw new RuntimeException("El V no existe");
		}
	}
	
	public Iterable<K> keys() {
		return bKeys.keys();
	}
	
	public Iterable<V> values() {
		return bValues.keys();
	}
	
	public Iterable<Tuple2<K, V>> keysValues() {
		return bKeys.keysValues();
	}

	public static <K,V> boolean isInyective(Dictionary<K, V> dict) {
		HashSet<Object> keyset = new HashSet<>();
		HashSet<Object> valueset = new HashSet<>();

		for (K k : dict.keys()) {
			keyset.add(k);
		}
		for (V v : dict.values()) {
			valueset.add(v);
		}
        return keyset.size() == valueset.size();
    }
		
	public static <K,V extends Comparable<? super V>> BiDictionary<K, V> toBiDictionary(Dictionary<K,V> dict) {
		HashBiDictionary<K,V> sol = new HashBiDictionary<>();
		HashDictionary<V,K> valuesdict = new HashDictionary<>();

		if(isInyective(dict)){
			for(Tuple2<K,V> t:dict.keysValues()){
				valuesdict.insert(t._2(),t._1());
			}
			sol.bKeys = dict;
			sol.bValues = valuesdict;
		}else{
			throw new IllegalArgumentException("El diccionario no es inyectivo");
		}
		return sol;
	}
	
	public <W> BiDictionary<K, W> compose(BiDictionary<V,W> bdic) {
		HashBiDictionary<K,W> sol = new HashBiDictionary<>();
		for(V k :bValues.keys()){
			if(bdic.isDefinedKeyAt(k)){
				sol.insert(bValues.valueOf(k),bdic.valueOf(k));
			}
		}
		return sol;
	}
		
	public static <K extends Comparable<? super K>> boolean isPermutation(BiDictionary<K,K> bd) {
		for(K k:bd.keys()){
			if(!bd.isDefinedValueAt(k)){
				return false;
			}
		}
		return true;
	}
	
	// Solo alumnos con evaluaci�n por examen final.
    // =====================================
	
	public static <K extends Comparable<? super K>> List<K> orbitOf(K k, BiDictionary<K,K> bd) {
		// TODO
		return null;
	}
	
	public static <K extends Comparable<? super K>> List<List<K>> cyclesOf(BiDictionary<K,K> bd) {
		// TODO
		return null;
	}

    // =====================================
	
	
	@Override
	public String toString() {
		return "HashBiDictionary [bKeys=" + bKeys + ", bValues=" + bValues + "]";
	}
	
	
}
