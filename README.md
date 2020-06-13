# Mini-projet MrSudoku

[contact:zslyvain@gmail.com](mailto:zslyvain@gmail.com)

Le jeu du Sudoku en mini-projet.

Utiliser `lein run` pour lancer le programme.

## Solveur: DFS et récursif

> En raison d'absence de cours, l'alogorithme SAT n'est pas appliquée.  

J'utilise algorithme DFS (Depth-first search) pour résoudre le sudoku. 
Pour une case non rensignée, le solveur va calculer toutes les réponses (chiffres 1-9) possibles dans une collection.
Les réponses ne produisent pas conflicts, donc cette case sera remplie avec une réponse parmi eux.
Le solveur va parcourir toutes les cases.
- Si on arrive à résoudre toutes les cases (jusqu'à (9,9)), le sudoku est résolu
- Si pour une case, il n'y a aucune réponse possible, il faut qu'on revienne à l'arrière, 
et prenne une autre réponse pour la case à l'arrière.


## loader (Générateur)

Un programme open-source en Java est utilisé donc en appuyant sur le bouton "load", un tout nouveau sudoku sera affiché.
Mais cela ne contrôle pas de difficulté de sudoku.
Lien: [https://qqwing.com/download.html](https://qqwing.com/download.html) 

**Licence du générateur QQWing**

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.



