/******************************************************************************
 * Student's name:
 * Student's group:
 * Data Structures. Grado en Informática. UMA.
******************************************************************************/

package dataStructures.vector;

import java.util.Iterator;

public class SparseVectorSol<T> implements Iterable<T> {

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
            if(sz == 1){
                Unif<T> sol = new Unif<>(x);
                return sol;
            }else{
                Tree<T> sol = new Node<>(new Unif<>(elem), new Unif<>(elem));
                return sol.set(sz,i,x);
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
            if (i < sz/2) {
                return left.get(sz/2,i);
            }else{
                return right.get(sz / 2, i - sz / 2);
            }
        }

        @Override
        public Tree<T> set(int sz, int i, T x) {
            if(i>=sz/2){
                if(this.right instanceof Unif<?>){
                    Unif<T> u = (Unif<T>) right;
                    right = u.set(sz/2,i-(sz/2),x);
                }else{
                    right = right.set(sz/2,(i-sz/2),x);
                }
            }else{
                if(this.left instanceof Unif<?>){
                    Unif<T> u = (Unif<T>) left;
                    left = u.set(sz/2,i,x);
                }else{
                    left = left.set(sz/2,i,x);
                }
            }
            Tree<T> sol = new Node<>(left,right);
            return sol;
        }

        protected Tree<T> simplify() {
            if(this.right instanceof Unif<?> && this.left instanceof Unif<?> && left.get( 1,0) == right.get(1,0)){
                return this.right;
            }else{
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

    public SparseVectorSol(int n, T elem) {
        if(n<0){
            throw new VectorException("n es negativo");
        }else{
            size = (int) Math.pow(2,n);
            root = new Unif<>(elem);
        }
    }

    public int size() {
        return size;
    }

    public T get(int i) {
        if (i < 0 || i > size){
            throw new VectorException("indice invalido");
        }else{
            return root.get(size, i);
        }
    }

    public void set(int i, T x) {
        if (i < 0 || i > size){
            throw new VectorException("indice invalido");
        }else{
           root = root.set(size,i,x);;
        }
    }

    @Override
    public Iterator<T> iterator() {
        // TODO
        return new SparseIterator();
    }

    private class SparseIterator implements Iterator<T>{
        private int current;
        public SparseIterator(){
            current=0;
        }
        @Override
        public boolean hasNext() {
            return current<size;
        }
        @Override
        public T next() {
            T res=root.get(size, current);
            current++;
            return res;
        }
    }

    @Override
    public String toString() {
        return "SparseVector(" + size + "," + root + ")";
    }
}
