/******************************************************************************
 * Student's name:
 * Student's group:
 * Data Structures. Grado en Informática. UMA.
******************************************************************************/

package dataStructures.vector;

import java.util.Iterator;

public class SparseVector<T> implements Iterable<T> {

    protected interface Tree<T> {

        T get(int sz, int i);

        Tree<T> set(int sz, int i, T x);
    }

    // Unif Implementation

    protected static class Unif<T> implements Tree<T> {

        private T elem;

        public Unif(T e) {
            elem = e;
        }

        @Override
        public T get(int sz, int i) {
            if(sz == 0){
                throw new VectorException("Vector vacío");
            }else{
                return  elem;
            }
        }

        @Override
        public Tree<T> set(int sz, int i, T x) {
            if (sz == 1){
                Unif<T> sol = new Unif<>(x);
                return sol;
            }else{
                Tree<T> aux = new Node<>(new Unif<T>(this.elem),new Unif<T>(this.elem));
                return aux.set(sz,i,x);
            }
        }

        @Override
        public String toString() {
            return "Unif(" + elem + ")";
        }
    }

    // Node Implementation

    protected static class Node<T> implements Tree<T> {

        private Tree<T> left, right;

        public Node(Tree<T> l, Tree<T> r) {
            left = l;
            right = r;
        }

        @Override
        public T get(int sz, int i) {
            if(i < sz/2){
                return left.get(sz/2,i);
            }else{
                return right.get(sz/2,i-sz/2);
            }
        }

        @Override
        public Tree<T> set(int sz, int i, T x) {
            if(i < sz/2){
                left = left.set(sz/2,i,x);
            }else{
                right = right.set(sz/2,i-sz/2,x);
            }
            return new Node<T> (left,right);
        }

        protected Tree<T> simplify() {
            if(left instanceof Unif<?> && right instanceof Unif<?> && left.get(1,0) == right.get(1,0)){
                return left;
            }else {
                return this;
            }
        }

        @Override
        public String toString() {
            return "Node(" + left + ", " + right + ")";
        }
    }

    // SparseVector Implementation

    private int size;
    private Tree<T> root;

    public SparseVector(int n, T elem) {
        if (n >= 0) {
            size = (int) Math.pow(2, n);
            root = new Unif<>(elem);
        }else{
            throw new VectorException("n es negativa");
        }
    }

    public int size() {
        return size;
    }

    public T get(int i) {
        if (i <= size || i >= 0){
            return root.get(size,i);
        }else{
            throw new VectorException("n es negativa");
        }
    }

    public void set(int i, T x) {
        if (i <= size && i >= 0){
            root = root.set(size,i,x);
        }else{
            throw new VectorException("n es negativa");
        }
    }

    @Override
    public Iterator<T> iterator() {
        return new SparseIterator();
    }

    private class SparseIterator implements Iterator<T>{
        private int current;

        public SparseIterator(){current=0;}
        @Override
        public boolean hasNext() {
            return current < size;
        }

        @Override
        public T next() {
            T res = root.get(size,current);
            current++;
            return res;
        }
    }

    @Override
    public String toString() {
        return "SparseVector(" + size + "," + root + ")";
    }
}
