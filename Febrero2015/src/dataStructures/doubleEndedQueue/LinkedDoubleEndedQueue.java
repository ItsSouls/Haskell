/**
 * Estructuras de Datos. Grado en Informática, IS e IC. UMA.
 * Examen de Febrero 2015.
 *
 * Implementación del TAD Deque
 *
 * Apellidos:
 * Nombre:
 * Grado en Ingeniería ...
 * Grupo:
 * Número de PC:
 */

package dataStructures.doubleEndedQueue;

public class LinkedDoubleEndedQueue<T> implements DoubleEndedQueue<T> {

    private static class Node<E> {
        private E elem;
        private Node<E> next;
        private Node<E> prev;

        public Node(E x, Node<E> nxt, Node<E> prv) {
            elem = x;
            next = nxt;
            prev = prv;
        }
    }

    private Node<T> first, last;

    /**
     *  Invariants:
     *  if queue is empty then both first and last are null
     *  if queue is non-empty:
     *      * first is a reference to first node and last is ref to last node
     *      * first.prev is null
     *      * last.next is null
     *      * rest of nodes are doubly linked
     */

    /**
     * Complexity:
     */
    public LinkedDoubleEndedQueue() {

    }

    /**
     * Complexity:
     */
    @Override
    public boolean isEmpty() {
        // TODO Auto-generated method stub
        return this.first == null && this.last == null;
    }

    /**
     * Complexity:
     */
    @Override

    public void addFirst(T x) {
        Node<T> newNode = new Node<>(x, first, null);
        if (isEmpty()) {
            first = last = newNode;
        } else {
            first.prev = newNode;  // El antiguo `first` apunta al nuevo nodo
            first = newNode;       // El nuevo nodo ahora es `first`
        }
    }


    /**
     * Complexity:
     */
    @Override
    public void addLast(T x) {
        Node<T> newNode = new Node<>(x, null, last);
        if (isEmpty()) {
            first = last = newNode;
        } else {
            last.next = newNode;   // El antiguo `last` apunta al nuevo nodo
            last = newNode;        // El nuevo nodo ahora es `last`
        }
    }


    /**
     * Complexity:
     */
    @Override
    public T first() {
        if(isEmpty()){
            return null;
        }else{
            return first.elem;
        }
    }

    /**
     * Complexity:
     */
    @Override
    public T last() {
        if(isEmpty()){
            return null;
        }else{
            return last.elem;
        }
    }

    /**
     * Complexity:
     */
    @Override
    public void deleteFirst() {
        if(isEmpty()){
            throw new RuntimeException("Dequeue is empty");
        } if (first.equals(last)) {
            first = null;
            last = null;
        }else{
            first = first.next;
            first.prev = null;
        }
        // TODO Auto-generated method stub

    }

    /**
     * Complexity:
     */
    @Override
    public void deleteLast() {
        if(isEmpty()){
            throw new RuntimeException("Dequeue is empty");
        } if (first.equals(last)) {
            first = null;
            last = null;
        }else{
            last = last.prev;
            last.next = null;
        }
    }

    /**
     * Returns representation of queue as a String.
     */
    @Override
    public String toString() {
    String className = getClass().getName().substring(getClass().getPackage().getName().length()+1);
        String s = className+"(";
        for (Node<T> node = first; node != null; node = node.next)
            s += node.elem + (node.next != null ? "," : "");
        s += ")";
        return s;
    }
}
