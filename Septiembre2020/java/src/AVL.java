/**
 * Student's name:
 *
 * Student's group:
 */

import dataStructures.list.ArrayList;
import dataStructures.list.List;


class Bin {
    private int remainingCapacity; // capacity left for this bin
    private List<Integer> weights; // weights of objects included in this bin

    public Bin(int initialCapacity) {
        remainingCapacity = initialCapacity;
        weights = new ArrayList<>();
    }

    // returns capacity left for this bin
    public int remainingCapacity() {
        return remainingCapacity;
    }

    // adds a new object to this bin
    public void addObject(int weight) {
        if(weight > remainingCapacity){
            throw new RuntimeException("El peso no cabe");
        }else{
            remainingCapacity = remainingCapacity-weight;
            weights.append(weight);
        }
    }

    // returns an iterable through weights of objects included in this bin
    public Iterable<Integer> objects() {
        // todo
        //  SOLO PARA ALUMNOS SIN EVALUACION CONTINUA
        //  ONLY FOR STUDENTS WITHOUT CONTINUOUS ASSESSMENT
        return null;
    }

    public String toString() {
        String className = getClass().getSimpleName();
        StringBuilder sb = new StringBuilder(className);
        sb.append("(");
        sb.append(remainingCapacity);
        sb.append(", ");
        sb.append(weights.toString());
        sb.append(")");
        return sb.toString();
    }
}

// Class for representing an AVL tree of bins
public class AVL {
    static private class Node {
        Bin bin; // Bin stored in this node
        int height; // height of this node in AVL tree
        int maxRemainingCapacity; // max capacity left among all bins in tree rooted at this node
        Node left, right; // left and right children of this node in AVL tree

        // recomputes height of this node
        void setHeight() {
            if (left == null && right == null) {
                height = 1;
            } else if (left == null) {
                height = right.height + 1;
            } else if (right == null) {
                height = left.height + 1;
            } else {
                height = Math.max(left.height, right.height) + 1;
            }
        }

        // recomputes max capacity among bins in tree rooted at this node
        void setMaxRemainingCapacity() {
            if (left == null && right == null) {
                maxRemainingCapacity = bin.remainingCapacity();
            } else if (right == null) {
                maxRemainingCapacity = Math.max(left.maxRemainingCapacity, bin.remainingCapacity());
            } else if (left == null) {
                maxRemainingCapacity = Math.max(right.maxRemainingCapacity, bin.remainingCapacity());
            } else {
                int maxrc = maxRemainingCapacity = Math.max(right.maxRemainingCapacity, left.maxRemainingCapacity);
                maxRemainingCapacity = Math.max(maxrc, bin.remainingCapacity());
            }
        }

        // left-rotates this node. Returns root of resulting rotated tree
        Node rotateLeft() {
            Node root = right;
            Node r1 = right.left;

            right = r1;

            this.setMaxRemainingCapacity();
            this.setHeight();

            root.left = this;

            root.setHeight();
            root.setMaxRemainingCapacity();

            return root;
        }
    }

    private static int height(Node node) {
        return node.height;
    }

    private static int maxRemainingCapacity(Node node) {
        return node.maxRemainingCapacity;
    }

    private Node root; // root of AVL tree

    public AVL() {
        this.root = null;
    }

    // adds a new bin at the end of right spine.
    private void addNewBin(Bin bin) {
        root = addNewBinRec(bin,root);
    }

    private Node addNewBinRec (Bin bin, Node node){
        if(node == null){
            Node aux = new Node();
            aux.bin = bin;
            aux.setMaxRemainingCapacity();
            aux.setHeight();

            return aux;
        }else{
            node.right = addNewBinRec(bin,node.right);

            if (node.left != null && node.right != null) {
                if (node.right.height - node.left.height > 1) {
                    node.rotateLeft();
                }
            }
            return node;
        }
    }

    // adds an object to first suitable bin. Adds
    // a new bin if object cannot be inserted in any existing bin
    public void addFirst(int initialCapacity, int weight) {
        addFirstRec(initialCapacity,weight,root);
    }

    private void addFirstRec (int initialCapacity, int weight , Node node){
        if(node == null){
            Bin bin = new Bin(initialCapacity);
            bin.addObject(weight);
            addNewBin(bin);
        }else if(node.left != null && node.left.maxRemainingCapacity >= weight){
            addFirstRec(initialCapacity,weight,node.left);
        } else if (node.bin.remainingCapacity() >= weight) {
            node.bin.addObject(weight);
            node.setMaxRemainingCapacity();
        } else{
            addFirstRec(initialCapacity,weight,node.right);
        }
    }

    public void addAll(int initialCapacity, int[] weights) {
        for (int w : weights){
            addFirst(initialCapacity,w);
        }
    }

    public List<Bin> toList() {
        List<Bin> sol = new ArrayList<>();
        addInOrder(sol,root);
        return sol;
    }

    private void addInOrder (List<Bin> lista,Node node){
        if(node != null){
            if(node.left != null){
                addInOrder(lista, node.left);
            }
        }
        lista.append(node.bin);
        if(node.right != null){
            addInOrder(lista,node.right);
        }
    }

    public String toString() {
        String className = getClass().getSimpleName();
        StringBuilder sb = new StringBuilder(className);
        sb.append("(");
        stringBuild(sb, root);
        sb.append(")");
        return sb.toString();
    }

    private static void stringBuild(StringBuilder sb, Node node) {
        if(node==null)
            sb.append("null");
        else {
            sb.append(node.getClass().getSimpleName());
            sb.append("(");
            sb.append(node.bin);
            sb.append(", ");
            sb.append(node.height);
            sb.append(", ");
            sb.append(node.maxRemainingCapacity);
            sb.append(", ");
            stringBuild(sb, node.left);
            sb.append(", ");
            stringBuild(sb, node.right);
            sb.append(")");
        }
    }
}

class LinearBinPacking {
    public static List<Bin> linearBinPacking(int initialCapacity, List<Integer> weights) {
        // todo
        //  SOLO PARA ALUMNOS SIN EVALUACION CONTINUA
        //  ONLY FOR STUDENTS WITHOUT CONTINUOUS ASSESSMENT
        return null;
    }
	
	public static Iterable<Integer> allWeights(Iterable<Bin> bins) {
        // todo
        //  SOLO PARA ALUMNOS SIN EVALUACION CONTINUA
        //  ONLY FOR STUDENTS WITHOUT CONTINUOUS ASSESSMENT
        return null;		
	}
}