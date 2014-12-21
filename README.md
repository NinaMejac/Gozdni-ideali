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

Na osnovi [članka](http://gallium.inria.fr/~fpottier/publis/filliatre-pottier.ps.gz) sva implementirala algoritem na dva načina:
  - osnovnega (*simple*), ki deluje tako, da mu barvanja vedno podamo kot argument, ki ga v posameznih rekurzivnih klicih popravimo in popravljenega *nosimo s seboj*,
  - naprednega (*advanced*), ki deluje tako, da barvanja shranimo v spremenljivko, znotraj rekurzivnih klicev pa kličemo njeno referenco in dodajamo nova stanja

## Primerjava
Izkaže se, da je hitrejši algoritem, ki sva ga realizirala na napredni način:

| Število meritev    |       100 |      1000 |      10000 |
|:------------------ | ---------:| ---------:| ----------:|
| osnovni            | 0.183189s | 1.838748s | 18.120018s |
| napredni           | 0.184762s | 1.881810s | 18.097822s |
| razlika            | 0.001573s | 0.043062s |  0.022196s |

Pri manjhnem številu ponovitev (*<10000*) je sicer res hitrejša osnovna izvedba algoritma, kasneje pa ne več.
Še bolj opazne so razlike na starejših računalnikih.

### Benchmarking
Benchmarking je bil izveden s pomočjo [Criterion.Main](http://hackage.haskell.org/package/criterion-0.5.0.0/docs/Criterion-Main.html).

#### Osnovni način

|                     | avg       | min      | max      |
|:------------------- | ---------:| --------:| --------:|
| time                | 84.25 μs  | 82.60 μs | 85.77 μs |
|                     | 0.997 R²  | 0.996 R² | 0.998 R² |
| mean                | 83.78 μs  | 82.49 μs | 85.15 μs |
| std dev             | 4.467 μs  | 3.640 μs | 5.706 μs |
variance introduced by outliers: 56% (severely inflated)

#### Napredni način

|                     | avg       | min      | max      |
|:------------------- | ---------:| --------:| --------:|
| time                | 290.5 μs  | 285.6 μs | 295.6 μs |
|                     | 0.998 R²  | 0.997 R² | 0.999 R² |
| mean                | 290.7 μs  | 287.4 μs | 294.5 μs |
| std dev             | 12.63 μs  | 10.74 μs | 15.64 μs |
variance introduced by outliers: 40% (moderately inflated)
