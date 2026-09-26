---
papersize: A4
geometry:
- margin=0.5in
documentclass: article
classoption:
- twocolumn
linestretch: 0.9
header-includes:
- \usepackage[document]{ragged2e}
---
# CMSC 57 2nd Long Exam

Instructions: Choose the best answer from the choices.

1. Which of these 4 quantities is the largest?

    a. The number of all possible 4 letter long passwords (roman alphabet, case-sensitive).

    b. The number of all possible 2-4 character long alphanumeric (roman alphabet or numbers) passwords (case sensitive)

    c. The number of all possible 20 bit files

    d. The number all possible 5 letter strings (case-insensitive) that can be formed from the roman alphabet

2. Which of the following statements are true?

    a. In the binomial expansion $(x+y)^{2m}$, the term $x^my^m$ has the coefficient $c=\frac{(2m)!}{m!m!}$

    b. The number of $r$-combinations from a set of $n$ elements is equal to the number of $q$-combinations from a set of $n$ elements, where $q = n-r$.

    c. Both a and b

    d. None of the above.

3. Which of the following terms appear in the binomial expansion of $(x + y)^{12}$?

    a. $495 x^9 y^3$

    b. $792 x^7 y^5$

    c. $220 x^{10} y^{2}$

    d. None of the above

4. Which of the following events are more likely?

    a. The probability of rolling the same number three times on a 6-sided dice.

    b. The probability of rolling a natural 20 (rolling a 20 from a 20 sided dice)

    c. The binomial probability $b(4;20,0.4)$ 

    d. The probability of flipping heads for all 6 coins

5. Which of the following statements are true?

    a. If $p(R)=0$ then $p(\overline{R})\neq 0$  

    b. Given two events $Q$ and $R$, if the probability $p(Q|R)$ is equal to $p(Q)$, then $Q$ and $R$ must be independent.

    c. Both a and b are true

    d. None of the above.

6. Given an event $E$, where $E \neq \emptyset$ and $E \neq S$ ($E$ is nonempty and is not equal to the sample space) which of the following pairs of events are independent?

    a. $p(E)$ and $p(\overline{E})$

    b. $p(E)$ and $p(E)$

    c. Both a and b

    d. None of the above

7. You are taking an exam with 20 multiple choice questions (with 4 choices each). For any given question, each choice is equally likely to be correct. Which of the following events have the highest probability?

    a. Guessing exactly 3 questions incorrectly

    b. Guessing exactly 3 questions correctly

    c. Guessing exactly 5 questions incorrectly

    d. Guessing exactly 5 questions correctly

8. Which of the following statements are true about Bayesian probabilities?

    a. The posterior probability is always greater than the prior probability

    b. The posterior probability is always lesser then the prior probability

    c. The posterior probability is always greater than zero 

    d. None of the above

9. Given nonempty events $Q$,$R$,$Q|R$, and $R|Q$, what can you say about the equality, $\frac{p(Q|R)}{p(R|Q)} = \frac{p(Q)}{p(R)}$ ?

    a. The equality is always true if $Q$ and $R$ are independent, it is false if $Q$ and $R$ are dependent

    b. The equality is always true if $Q$ and $R$ are dependent, it is false $Q$ and $R$ are independent

    c. The equality is never true.

    d. The equality is always true.

10. Suppose you are playing a table top role playing game where the damage you deal is based on three values: attack damage, critical rate, and critical damage. When your player hits an enemy there is some chance that it will deal some additional critical damage on top of the original damage. Which of these damage values will deal the most amount of expected damage?

    a. 30% chance to deal an additional 120% of the original damage.

    b. 60% chance to deal an additional 60% of the original damage.

    c. 15% chance to deal an additional 150% of the original damage.

    d. 45% chance to deal and additional 90% of the original damage.

11. Given an undirected graph $G = (V,E)$ where $V = \{v_1, v_2, v_3, \cdots, v_n\}$ and $E = \big\{\{v_i,v_j\}|1\leq i<j\leq n\big\}$. Which of the following statements about $G$ are true?

    a. $G$ is a complete graph for any $n > 1$.

    b. $G$ is a connected graph for any $n > 1$.

    c. Both a and b are true.

    d. None of the above.

12. Given the adjacency matrix for a weighted undirected graph, $D$, where each element denotes distance between the two vertices ($D_{ij}$ denotes the distance between vertices $v_i$ and $v_j$). Two vertices with zero weight do not share an edge.

    $$
    D = \begin{bmatrix}
    0 & 2 & 0 & 3 & 1 \\
    2 & 0 & 3 & 0 & 4 \\
    0 & 3 & 0 & 0 & 2 \\
    3 & 0 & 0 & 0 & 5 \\
    1 & 4 & 2 & 5 & 0
    \end{bmatrix}
    $$

    Among the choices, which pair of vertices has the shortest path between them?

    a. $v_1$ and $v_3$

    b. $v_2$ and $v_4$

    c. $v_4$ and $v_3$

    d. None of the above, these vertex pairs do not have a path.

13. Given a graph $G = (V,E)$, which of the following statements are true?

    a. If there exists a vertex $(a,b)$ in $E$ then there is always path from $b$ to $a$

    b. If $G$ is a complete graph then there exists some subgraph $G'=(V', E')$ (where $V' \subseteq V$ and $E' \subseteq E$) which is a cycle.

    c. All cycles are bipartite graphs

    d. None of the above

14. Which of the following undirected simple graphs are guaranteed to contain Euler circuits?

    a. A cycle

    b. A tree

    c. A complete graph

    d. A connected graph

15. Which of the following is true about simple undirected graphs?

    a. A complete graph with $n$ vertices will have a degree weight of $2n$

    b. A cycle with $n$ vertices will have a degree weight of $n$

    c. For any complete graph $G(V,E)$, $|V|<|E|$

    d. None of the above (all statements are false)

16. Which of the following statements are NOT true about Trees?

    a. If $A$ is the ancestor of $B$ then $B$ is the descendant of $A$.

    b. If $A$ is a leaf then there must be at least one vertex that is an ancestor of $A$ that is not $A$.

    c. If $A$ and $B$ are siblings then the they have the same number of ancestors.

    d. If $A$ and $B$ are siblings then a child of $A$ and a child of $B$ have the same number of ancestors.

17. Suppose we are creating a tree from the following graph,

    $$
    \begin{aligned}
    \text{Vertex} && \text{Neighbors}\\
    A&&\{B\}\\
    B&&\{A,C,E,G\}\\
    C&&\{B\}\\
    D&&\{E\}\\
    E&&\{B,D,F\}\\
    F&&\{E,H\}\\
    G&&\{B\}\\
    H&&\{F\}
    \end{aligned}
    $$
    Which root designation produces a root with the most leaves?

    a. Tree with E as the root

    b. Tree with A as the root

    c. Tree with D as the root

    d. None of the above, this graph is not a tree.

18. Let a simple undirected graph $H(V,E)$ where $V = \{A,B,C,D,E\}$ and $E = \{\{A,B\}, \{A,C\}, \{C,D\}, \{C,E\}\}$. If you build a tree with $a$ as the root, which of the following are valid traversals?

    a. breadth first: A,B,C,D,E

    b. depth first: A,B,C,D,E

    c. both a and b

    d. none of the above

19. Given the following tree, with the vertices $\{A,B,C,D,E,F\}$ and the following family specifications:

    > $A$ is the root, it's children are $B$ and $C$
    >
    > $B$'s only child is $D$.
    >
    > $C$'s children are $E$ and $F$. 

    Which of the following are correct traversals of the tree?

    a. preorder: A,B,D,C,E,F; postorder: D,E,B,F,C,A

    b. preorder: A,B,D,E,C,F; postorder: D,E,B,F,C,A

    c. preorder: A,B,D,C,E,F; postorder: D,B,E,F,C,A

    d. preorder: A,B,C,D,E,F; postorder: D,B,E,F,C,A

20. What is the total weight of the minimum spanning tree of the following weighted graph?
    $$
    \begin{bmatrix}
    0&1&1&1&2&0\\
    1&0&1&2&0&0\\
    1&1&0&3&0&0\\
    1&2&3&0&1&2\\
    2&0&0&1&0&0\\
    0&0&0&2&0&0\\
    \end{bmatrix}
    $$

    a. 4

    b. 5

    c. 6

    d. 7
