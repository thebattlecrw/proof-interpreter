# Un interpréteur et un prouveur en logique propositionnelle

**Auteur :** Sebastian LOVEJOY-BLACK 

Ce projet implémente un prouveur en logique propositionnelle. L'intégralité de l'implémentation se trouve dans le fichier `prover.ml`.

## Structure du projet

```bash
.
├── compile.sh  
├── lexer.mll  
├── main.ml     
├── parser.mly  
├── prover.ml   
└── README.md   
```

- **`compile.sh`** - Script pour compiler l'interpréteur.
- **`lexer.mll`** - Analyse lexicale avec - `ocamllex`.
- **`main.ml`** - Boucle d'interpretation pour le parseur.
- **`parser.mly`** - Analyse syntaxique avec `ocamlyacc`.
- **`prover.ml`** - Coeur du projet : implémentation des formules, évaluation, preuve et tactiques (la partie principale du projet)
## Utilisation du prouveur

### Compilation et exécution

Pour compiler l'interpréteur, exécutez la commande :

```bash
./compile.sh
```
Ensuite, pour lancer le prouveur :

```bash
./build/prover
```

### Commandes utilisables 

- **`Proof [formule].`** : Commancer la preuve d'une formule. Exemple : `Proof ((P \/ Q) ==> R) ==> (P ==> R) /\ (Q ==> R).`
- **`[tactique] [H1] [H2].`** Appliquer une tactique sans argument, avec une hypothèse, ou bien avec deux hypothèses. Exemples : `Impl_Intro.`, `Or_Elim H0`, `Impl_Elim H1 H2`, etc.
- **`Qed.`** : Ne fonctionne pas comme Qed. dans des vrais prouveurs, mais vérifie s'il reste des buts dans la preuve.
- **`CTRL+D`** : Terminer la session (EOF).

### Exemple de session 

```bash
> Proof (P ==> Q) ==> (P ==> Q).

======================
(P ==> Q) ==> (P ==> Q)
> Impl_Intro.

H0 : P ==> Q
======================
P ==> Q
> Impl_Intro.

H1 : P
H0 : P ==> Q
======================
Q
> Impl_Elim H0 H1.

H2 : Q
H1 : P
H0 : P ==> Q
======================
Q
> Exact H2.
All goals completed.
> Qed.
Proof completed successfully. Qed.
> 
Goodbye.

```