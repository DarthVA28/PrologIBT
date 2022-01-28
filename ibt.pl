/* Basic predicates */ 

ibt(empty).
ibt(node(N, L, R)):- integer(N), ibt(L), ibt(R).

/***** SOME HELPER FUNCTIONS *****/

/* Concatenate two lists */
concat([],List,List).
concat([Elem|List1],List2,[Elem|List3]) :- concat(List1,List2,List3).

/* Count occurences of element in a list */
count(X,[X|Tl],C):- count(X,Tl,C1),
                    C is C1+1.
count(X,[Hd|Tl],C):-Hd \== X,
                    count(X,Tl,C).
count(_,[],0).

/* Last element of list */
lastelem([X|[]],X).
lastelem([_|Tl],Y) :- last1(Tl,Y).

/* max value in a BST */
maxTree(node(X,L,R),M):-   maxTree(L,M1),
                                maxTree(R,M2),
                                Ml is max(M1,M2),
                                M is max(Ml,X).
maxTree(empty,-10000).

/* min value in a BST */
minTree(node(X,L,R),M):-   minTree(L,M1),
                                minTree(R,M2),
                                Ml is min(M1,M2),
                                M is min(Ml,X).
minTree(empty,10000).

/* Sublist */
subList(I,[Hd|Tl1],[Hd|Tl],L):-   I1 is I-1, 
                                subList(I1,Tl1,Tl,L).            
subList(0,List,[],List).

/* L is the list of elements sorted by c-th occurence */
/* List of elements encountered stored in L2 */

extractH([(X,K)|Tl],L,L2,C):-   not(count((X,K),L2,C)),
                                extractH(Tl,L,[(X,K)|L2],C).

extractH([(X,K1)|Tl],[X|Tl1],L2,C):- count((X,K1),L2,C),
                                     extractH(Tl,Tl1,[(X,K1)|L2],C).

extractH([],[],_,_).

extract1(L,Ls):- extractH(L,Ls,[],0).
extract2(L,Ls):- extractH(L,Ls,[],1).
extract3(L,Ls):- extractH(L,Ls,[],2).

/* Creates an Euler Tour ls with each element indexed */

eTH(node(X,L,R), Ls, Idx):-         Idx1 is 2*Idx,
                                    Idx2 is 2*Idx+1,
                                    eTH(L,L1,Idx1),
                                    eTH(R,L2,Idx2),
                                    concat([(X,Idx)],L1,A),
                                    concat(A,[(X,Idx)],B),
                                    concat(L2,[(X,Idx)],C),
                                    concat(B,C,Ls).

eTH(empty,[],_).

eT1(T,L):- eTH(T,L,1).

/* ASSIGNMENT BEGINS HERE */

/* Size */ 
/* N is the size of subtrees L, R + 1 */ 

size(node(_,L,R), N):- size(L,N1),
                            size(R,N2),
                            N is N1+N2+1.

size(empty,0).

/* Height */ 
/* Ht = max(H(L),H(R))+1  */ 

height(node(_,L,R), H):-   height(L,H1),
                                height(R,H2),
                                H is max(H1,H2)+1.


height(empty,0).

/* Preorder Traversal */ 
/* Preorder of T is concatenation of preorder traversal of [X], L,R */ 

preorder(node(X,L,R), [X|Tl]):-     preorder(L,L1),
                                    preorder(R,L2),
                                    concat(L1,L2,Tl).

preorder(empty,[]).

/* Inorder Traversal */ 
/* Inorder of T is concatenation of inorder traversal of L, [X], R */ 

inorder(node(X,L,R),Ls):-       inorder(L,L1),
                                inorder(R,L2),
                                concat(L1,[X],B),
                                concat(B,L2,Ls).

inorder(empty,[]).

/* Postorder Traversal */ 
/* Postorder of T is concatenation of postorder traversal of L, R, [X] */ 


postorder(node(X,L,R),Ls):-    postorder(L,L1),
                                    postorder(R,L2),
                                    concat(L1,L2,B),
                                    concat(B,[X],Ls).

postorder(empty,[]).

/* Tail Recursive Preorder */
/* Similar to preorder but tail recursive starting from rightmost element of Tree */ 

preorder_h(empty,Ls,Ls).

preorder_h(node(N,L,R),Lt1,Lf) :- 
	preorder_h(R,Lt1,Lt2),
	preorder_h(L,Lt2,Lt3),
	concat([N],Lt3,Lf).

trPreorder(T,L) :- preorder_h(T,[],L).

/* Tail Recursive Inorder */
/* Similar to inorder but tail recursive starting from rightmost element of Tree */ 

inorder_h(empty,Ls,Ls).

inorder_h(node(N,L,R),Lt1,Lf) :- 
	inorder_h(R,Lt1,Lt2),
    concat([N],Lt2,Lt3),
	inorder_h(L,Lt3,Lf).

trInorder(T,L) :- inorder_h(T,[],L).

/* TaiL Recursive Postorder */
/* Similar to postorder but tail recursive starting from rightmost element of Tree */ 


postorder_h(empty,Ls,Ls).

postorder_h(node(N,L,R),Lt1,Lf) :-
	postorder_h(L,Lt1,Lt2),
	postorder_h(R,Lt2,Lt3),
	concat(Lt3,[N],Lf).

trPostorder(T,L) :- postorder_h(T,[],L).

/* Euler Tour */
/* ET of Node(X,L,R) is X ET(L) X ET(R) X */

eulerTour(node(X,L,R), Ls):-   eulerTour(L,L1),
                                    eulerTour(R,L2),
                                    concat([X],L1,A),
                                    concat(A,[X],B),
                                    concat(L2,[X],C),
                                    concat(B,C,Ls).

eulerTour(empty,[]).

/* Extract PreOrder from Euler Tour */
/* Construct ET with each node given label for distinctness, extract list of elements 
extracted and sorted by first occurence */ 

preET(BT, Ls):- eT1(BT,ET),
                extract1(ET,Ls).

/* Extract InOrder from Euler Tour */
/* Construct ET with each node given label for distinctness, extract list of elements 
extracted and sorted by second occurence */ 

inET(BT, Ls):-  eT1(BT,ET),
                extract2(ET,Ls).


/* Extract PostOrder from Euler Tour */
/* Construct ET with each node given label for distinctness, extract list of elements 
extracted and sorted by third occurence */ 

postET(BT, Ls):-eT1(BT,ET),
                extract3(ET,Ls).

/* Check Balanced Tree */ 
/* |Height difference of L,R subtrees|<= 1, recurse at subtrees */

isBalanced(node(_,L,R)):-  height(L,H1),
                                height(R,H2),
                                H1-H2 < 2,
                                H1-H2 > -2,
                                isBalanced(L),
                                isBalanced(R).

isBalanced(empty).

/* Check if a Binary Tree is a BST */ 
/* Max key in left and min key in right compare with node */
/* Recurse for children */

isBST(node(X,L,R)):- maxTree(L,Ml),
                    minTree(R,Mr),
                    X>=Ml,
                    X<Mr,
                    isBST(L),
                    isBST(R).

isBST(empty).

/* Make BST */
/* For the sorted list, use make BST of left and right half (splitting about median) */

makeBST(TL,node(X,L,R)):-  TL \== [],
                                sort(TL,Ls),
                                length(Ls,L1),
                                M is div(L1,2),
                                subList(M,Ls,LL,[Hd|RTl]),
                                X is Hd,
                                makeBST(LL,L),
                                makeBST(RTl,R).

makeBST([],empty).

/* Find an element in BST */
/* True if key is the element or find in left or find in right depending on < or > key of root */

lookup(N,node(X,L,_)):-    N<X,
                                lookup(N,L).

lookup(N,node(X,_,R)):-    N>X,
                                lookup(N,R).

lookup(N,node(N,_,_)).

/* Insert Element */
/* If present the tree is same */
/* If less than root inserted then insert in left else in right */
                                
insert(N,node(X,L,R),node(X,node(X1,L1,R1),R)):-  N<X,
                                                            insert(N,L,node(X1,L1,R1)).
                                
insert(N,node(X,L,R),node(X,L,node(X2,L2,R2))):- N>X,
                                                            insert(N,R,node(X2,L2,R2)).

insert(N,node(N,L,R),node(N,L,R)).

insert(N,empty,node(N,empty,empty)). 

/* Delete Element */
/* If not present the tree is same */
/* If less than root, delete then insert in left else in right */
/* If key is non root then bring up key from left or right child and recursively delete it */
/* O(h) time */

findIS(IP,node(IP,empty,_)).
findIS(IS,node(_,Left,_)) :- findIS(IS,Left).

delete(N,node(X,L,R),node(X,BT1,R)):-   N<X,
                                        delete(N,L,BT1).                          
delete(N,node(X,L,R),node(X,L,BT2)):-   N>X,
                                        delete(N,R,BT2).

delete(_,empty,empty).
delete(N,node(N,empty,empty),empty).
delete(N,node(N,node(X1,L1,R1),empty),node(X1,L1,R1)). 
delete(N,node(N,empty,BT),BT). 
delete(N,node(N,L,R),node(IS,L,BT2)):- findIS(IS,R), delete(IS,R,BT2).

deletehelp(N,BT,BT1):- delete(N,BT,BT1), isBST(BT1).

/*  Convert Tree to String  */
                                                            
toString(node(X,L,R),S):-  toString(L,S1),
                                toString(R,S2),
                                term_string(X,S3),
                                string_concat("(",S3,S4),
                                string_concat(S4,", ",S5),
                                string_concat(S5,S1,S6),
                                string_concat(S6,", ",S7),
                                string_concat(S7,S2,S8),
                                string_concat(S8,")",S).

toString(empty,"()").