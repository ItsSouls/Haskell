
/**
 * Huffman trees and codes.
 *
 * Data Structures, Grado en Informatica. UMA.
 *
 *
 * Student's name:
 * Student's group:
 */

import dataStructures.dictionary.AVLDictionary;
import dataStructures.dictionary.Dictionary;
import dataStructures.list.ArrayList;
import dataStructures.list.LinkedList;
import dataStructures.list.List;
import dataStructures.priorityQueue.BinaryHeapPriorityQueue;
import dataStructures.priorityQueue.PriorityQueue;
import dataStructures.tuple.Tuple2;

public class Huffman {

    // Exercise 1
    public static Dictionary<Character, Integer> weights(String s) {
    	Dictionary<Character, Integer> sol = new AVLDictionary<>();
        if(s.isEmpty()){
            throw new RuntimeException("el string es vacio");
        }
        for(char a : s.toCharArray()){
            if(sol.isDefinedAt(a)){
                sol.insert(a, sol.valueOf(a) + 1);
            }else{
                sol.insert(a, 1);
            }
        }
        return sol;
    }

    // Exercise 2.a
    public static PriorityQueue<WLeafTree<Character>> huffmanLeaves(String s) {
    	//to do
                PriorityQueue<WLeafTree<Character>> sol = new BinaryHeapPriorityQueue<>();
    	        for(Tuple2<Character, Integer> tuple: weights(s).keysValues()){
                    WLeafTree<Character> wl = new WLeafTree<>(tuple._1(), tuple._2());
                    sol.enqueue(wl);
                }
                return sol;
    }

    // Exercise 2.b
    public static WLeafTree<Character> huffmanTree(String s) {
        if (weights(s).size() < 2){
            throw new HuffmanException("cadena invalida");
        }else{
            PriorityQueue<WLeafTree<Character>> hl = huffmanLeaves(s);
            WLeafTree<Character> first = hl.first();
            hl.dequeue();
            while (!hl.isEmpty()){
                first = new WLeafTree<>(first,hl.first());
                hl.dequeue();
            }
            return first;
        }
    }

    // Exercise 3.a
    public static Dictionary<Character, List<Integer>> joinDics(Dictionary<Character, List<Integer>> d1, Dictionary<Character, List<Integer>> d2) {
        for(Tuple2<Character,List<Integer>> t:d1.keysValues()){
            d2.insert(t._1(),t._2());
        }
    	return d2;
    }

    // Exercise 3.b
    public static Dictionary<Character, List<Integer>> prefixWith(int i, Dictionary<Character, List<Integer>> d) {
        for(Tuple2<Character,List<Integer>> t:d.keysValues()){
            t._2().prepend(i);
            d.insert(t._1(), t._2());
        }
        return d;
    }

    /*/ Exercise 3.c
    public static Dictionary<Character, List<Integer>> huffmanCode(WLeafTree<Character> ht) {
        //to do
        Dictionary<Character, List<Integer>> dictl = new AVLDictionary<>();
        Dictionary<Character, List<Integer>> dictr = new AVLDictionary<>();

        if(ht.leftChild()!=null) {
            dictl = huffmanCode(ht.leftChild());
            dictl = prefixWith(0, dictl);
        }
        if(ht.rightChild()!=null) {
            dictr = huffmanCode(ht.rightChild());
            dictr = prefixWith(1, dictr);
        }
        if(ht.isLeaf()){
            List<Integer> l = new ArrayList<>();
            dictl.insert(ht.elem(),l);
        }else{
            dictl = joinDics(dictl,dictr);
        }
        return dictl;
    }*/

    public static Dictionary<Character, List<Integer>> huffmanCode(WLeafTree<Character> ht) {
        Dictionary<Character,List<Integer>> dictl = new AVLDictionary<>();
        Dictionary<Character,List<Integer>> dictr = new AVLDictionary<>();

        if(ht.leftChild()!= null){
            dictl = huffmanCode(ht.leftChild());
            dictl = prefixWith(0,dictl);
        }
        if(ht.rightChild()!=null){
            dictr = huffmanCode(ht.rightChild());
            dictr = prefixWith(1,dictr);
        }
        if(ht.isLeaf()){
            List<Integer> l = new ArrayList<>();
            dictl.insert(ht.elem(),l);
        }else{
            dictl = joinDics(dictl,dictr);
        }
        return dictl;
    }

    // Exercise 4
    public static List<Integer> encode(String s, Dictionary<Character, List<Integer>> hc) {
        //to do 
    	return null;
    }

    // Exercise 5
    public static String decode(List<Integer> bits, WLeafTree<Character> ht) {
        //to do 
    	return null;
    }
}
