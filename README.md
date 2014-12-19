Gozdni-ideali
=============

##Struktura **Tree**

Primer dreves z uporabo strukture *Tree*:
```
tree1 = Tree 0 [Tree 1 []]
tree2 = Tree 2 [Tree 3 [],Tree 4 []]
```

Drevo vedno vsebuje očeta (v prvem primeru je to vozlišče *0*, v drugem pa vozlišče z imenom *2*), le-ta pa lahko vsebuje poljubno sinov.
```
  0        2
  |       / \
  1      3   4
  
tree1    tree2
```

*Gozd* je lahko sestavljen iz poljubnega števila dreves:
```
forest = [tree1, tree2]
```

**Pri označevanju vozlišč je potrebno biti pazljiv, da ima vsako vozlišče svoje (unikatno) ime in da si le ta sledijo v naraščajočem vrstnem redu. Začetno vozlišče prvega drevesa mora imeti oznako 0.**

## Pomožne funkcije
### prepareTree
Pomožna funkcija *prepareTree* nam na podlagi vhodnega niza znakov pripravi ustrezno strukturo.
Primer:
```
*Main>prepareTree
2[34]
Tree 2 [Tree 3 [],Tree 4 []]
```
Rezultat je torej pravilno napisano drevo.

## Gozdni ideali - algoritem
Osnovna ideja algoritma je, da se sprehodimo skozi gozd, izberemo i-to vozlišče in:
  - če je vozlišče belo (nepobarvano), ga pobarvamo in nadaljujemo z izvajanjem algoritma (na drevesu, kjer je i-to vozlišče že črno)
  - če je vozlišče črno (pobarvano), nadaljujemo z izvajanjem algoritma na (trenutnem) poddrevesu, nato pa vozlišče odbarvamo

Sled izvajanja algoritma na zgoraj definiranem gozdu:
````
0   0           0   1              0   1              0   1              0   1              1   1              1   1              1   1           1   1           1   0              1   0              1   1              1   1              1   1              1   1  
|  / \    -->   |  / \    -->      |  / \    -->      |  / \    -->      |  / \    -->      |  / \    -->      |  / \    -->      |  / \    -->   |  / \    -->   |  / \    -->      |  / \    -->      |  / \    -->      |  / \    -->      |  / \    -->      |  / \ 
0 0   0         0 0   0            0 0   1            0 1   1            0 1   0            0 1   0            0 1   1            0 0   1         0 0   0         0 0   0            1 0   0            1 0   0            1 0   1            1 1   1            1 1   0
````



